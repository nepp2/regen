
use std::{hash::Hash};
use std::time::{Duration, Instant};
use std::collections::{HashMap, HashSet};
use std::alloc::{alloc, Layout};

use crate::{compile::Function, env::{self, CellUid, Env}, hotload::hotload_value, interpret, perm_alloc::{Ptr, perm}, types::TypeHandle};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SignalId(pub u64);

#[derive(Copy, Clone)]
pub struct Signal {
  pub id : SignalId,
  pub event_loop : Ptr<EventLoop>,
  pub output_type : TypeHandle,
  pub output : *mut (),
  pub operation : StreamOperation,
  pub last_update : u64,
}

#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  observers : HashMap<SignalId, Vec<SignalId>>,
  signals : HashMap<SignalId, Ptr<Signal>>,
  signal_cell_map : HashMap<SignalId, CellUid>,
  timers : HashSet<SignalId>,
  update_count : u64,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TimerState {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
}

#[derive(Clone, Copy)]
pub struct RegenCallback { f : *const Function }

#[derive(Clone, Copy)]
pub enum StreamOperation {
  Timer,
  Poll{ input_signal: Ptr<Signal>, event_source : *mut (), poll_function : RegenCallback },
  State(Ptr<Signal>, RegenCallback),
  NativeHook(fn(Env, *mut ())),
}

pub fn add_native_hook<Data>(
  env : Env,
  input_signal : Ptr<Signal>,
  data : Ptr<Data>,
  f : fn(Env, Ptr<Data>),
)
{
  let op = {
    let f_ptr = unsafe { std::mem::transmute(f as *const ()) };
    StreamOperation::NativeHook(f_ptr)
  };
  let data_ptr = Ptr::to_ptr(data) as *mut ();
  let s = create_signal(env.event_loop, env.c.void_tag, data_ptr, op);
  add_observer(env.event_loop, input_signal, s);
}

fn update_signal(env : Env, update_number : u64, mut signal : Ptr<Signal>) -> bool {
  use StreamOperation::*;
  signal.last_update = update_number;
  match signal.operation {
    Timer => {
      panic!("timer signals should never be triggered by other signals");
    }
    Poll { input_signal, event_source, poll_function } => {
      let event = input_signal.output;
      let mut return_val = true;
      let f = poll_function.f as *const Function;
      interpret::interpret_function(
        f,
        &[event_source as u64, signal.output as u64, event as u64],
        Some((&mut return_val) as *mut bool as *mut ()));
      return_val
    }
    NativeHook(f) => {
      f(env, signal.output);
      true
    }
    State(parent, f) => {
      let event = parent.output;
      interpret::interpret_function(
        f.f as *const Function,
        &[signal.output as u64, event as u64],
        None);
      true
    }
  }
}

pub fn create_event_loop() -> Ptr<EventLoop> {
  perm(EventLoop {
    start_time: Instant::now(),
    id_counter: 0,
    signals: HashMap::new(),
    observers: HashMap::new(),
    signal_cell_map: HashMap::new(),
    timers: HashSet::new(),
    update_count: 0,
  })
}

fn next_id(mut el : Ptr<EventLoop>) -> SignalId {
  let id = SignalId(el.id_counter);
  el.id_counter += 1;
  id
}

fn create_signal(
  mut el : Ptr<EventLoop>,
  output_type : TypeHandle,
  output_addr : *mut (),
  operation : StreamOperation,
) -> Ptr<Signal>
{
  let signal = perm(Signal {
    id : next_id(el),
    event_loop: el,
    output_type,
    output: output_addr,
    operation,
    last_update: 0,
  });
  el.signals.insert(signal.id, signal);
  signal
}

pub fn destroy_signal(mut el : Ptr<EventLoop>, id : SignalId) {
  el.signals.remove(&id);
  el.signal_cell_map.remove(&id);
  el.timers.remove(&id);
  for (_, obs) in el.observers.iter_mut() {
    obs.retain(|&ob| ob != id);
  }
}

pub fn create_timer(env : Env, current_millisecond : i64, millisecond_interval : i64) -> Ptr<Signal> {
  let timer_state = TimerState {
    millisecond_interval,
    next_tick_millisecond: current_millisecond + millisecond_interval,
  };
  let state = alloc_val(timer_state);
  let mut el = env.event_loop;
  let s = create_signal(el, env.c.tick_event_tag, state, StreamOperation::Timer);
  el.timers.insert(s.id);
  s
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_signal_input(env : Env, id : SignalId) {
  let el = env.event_loop;
  let signal = el.signals[&id];
  let should_push = update_signal(env, el.update_count, signal);
  if should_push {
    push_new_value(env, signal);
  }
}

fn push_new_value(env : Env, signal : Ptr<Signal>) {
  if let Some(cell_uid) = env.event_loop.signal_cell_map.get(&signal.id) {
    hotload_value(*cell_uid, env, signal.output, signal.output_type);
  }
  if let Some(observers) = env.event_loop.observers.get(&signal.id) {
    for &id in observers {
      handle_signal_input(env, id);
    }
  }
}

fn add_observer(mut el : Ptr<EventLoop>, parent : Ptr<Signal>, signal : Ptr<Signal>) {
  el.observers.entry(parent.id).or_insert(vec![]).push(signal.id);
}

fn handle_timer_pulse(mut env : Env, signal : Ptr<Signal>, current_time : i64) {
  if let StreamOperation::Timer = signal.operation {
    env.event_loop.update_count += 1;
    push_new_value(env, signal);
    let timer : &mut TimerState = unsafe {
      &mut *(signal.output as *mut TimerState)
    };
    timer.next_tick_millisecond = current_time + timer.millisecond_interval;
  }
  else {
    panic!("expected timer");
  }
}

pub fn start_loop(env : Env) {
  let event_loop = env.event_loop;
  let start = event_loop.start_time;
  loop {
    if event_loop.timers.is_empty() {
      // there will never be another event
      return;
    }
    // figure out which timer will tick next
    let (signal, mut timer) =
      event_loop.timers.iter().map(|s| {
        let sig = event_loop.signals[s];
        let t : Ptr<TimerState> = Ptr::from_ptr(sig.output as *mut TimerState);
        (sig, t)
      })
      .min_by_key(|x| x.1.next_tick_millisecond)
      .unwrap();
    // wait for it to tick
    let now = current_millisecond(start);
    let wait_time = timer.next_tick_millisecond - now;
    if wait_time > 0 {
      spin_sleep::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    let now = current_millisecond(start);
    timer.next_tick_millisecond = now + timer.millisecond_interval;
    handle_timer_pulse(env, signal, now);
  }
}

pub fn register_signal(env : Env, signal : Ptr<Signal>, cell : CellUid) {
  let mut el = env.event_loop;
  el.signals.insert(signal.id, signal);
  el.signal_cell_map.insert(signal.id, cell);
}

fn alloc_val<V>(v : V) -> *mut () {
  Ptr::to_ptr(perm(v)) as *mut ()
}

fn alloc_bytes(bytes : usize, initial_value : Option<*const ()>) -> *mut () {
  let layout = Layout::from_size_align(bytes, 8).unwrap();
  unsafe {
    let ptr = alloc(layout) as *mut ();
    if let Some(v) = initial_value {
      std::ptr::copy_nonoverlapping(v as *const u8, ptr as *mut u8, bytes);
    }
    ptr as *mut ()
  }
}

fn create_regen_signal(
  env : Env,
  output_type : TypeHandle,
  initial_value : Option<*const ()>,
  operation : StreamOperation,
) -> Ptr<Signal>
{
  let output = alloc_bytes(output_type.size_of as usize, initial_value);
  let signal = create_signal(env.event_loop, output_type, output, operation);
  env::register_signal(env, signal.id);
  signal
}

// ----------- Define FFI to call from regen -------------

pub mod ffi {
  use super::*;

  pub extern "C" fn register_tick_signal(
    env : Env,
    millisecond_interval : i64,
  ) -> Ptr<Signal>
  {
    let now = current_millisecond(env.event_loop.start_time);
    let signal = create_timer(env, now, millisecond_interval);
    env::register_signal(env, signal.id);
    signal
  }

  pub extern "C" fn register_state_signal(
    env : Env,
    input_signal : Ptr<Signal>,
    state_type : TypeHandle,
    initial_state : *const (),
    update_function : *const Function,
  ) -> Ptr<Signal>
  {
    let op = StreamOperation::State(input_signal, RegenCallback { f: update_function });
    let signal = create_regen_signal(env, state_type, Some(initial_state), op);
    add_observer(env.event_loop, input_signal, signal);
    signal
  }

  pub extern "C" fn register_poll_signal(
    env : Env,
    input_signal : Ptr<Signal>,
    event_source_type : TypeHandle,
    event_source : *const (),
    event_type : TypeHandle,
    poll_function : *const Function,
  ) -> Ptr<Signal>
  {
    let event_source = alloc_bytes(event_source_type.size_of as usize, Some(event_source));
    let op = StreamOperation::Poll {
      input_signal,
      event_source,
      poll_function: RegenCallback { f: poll_function },
    };
    let signal = create_regen_signal(env, event_type, None, op);
    add_observer(env.event_loop, input_signal, signal);
    signal
  }
}
