
use std::{hash::Hash};
use std::time::{Duration, Instant};
use std::collections::HashMap;
use std::alloc::{alloc, Layout};

use crate::{
  types::TypeHandle,
  compile::Function, env::{self, Env}, interpret,
  perm_alloc::{Ptr, perm},
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SignalId(pub u64);

#[derive(Copy, Clone)]
pub struct Signal {
  pub id : SignalId,
  pub event_loop : Ptr<EventLoop>,
  pub output : *mut (),
  pub operation : StreamOperation,
  pub last_update : u64,
}

#[derive(Copy, Clone)]
struct Observer {
  signal : Ptr<Signal>,
  push_changes : bool,
}

#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  observers : HashMap<SignalId, Vec<Observer>>,
  timers : Vec<Ptr<Signal>>,
  update_count : u64,
}

#[derive(Copy, Clone)]
pub struct TimerState {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
}

#[derive(Clone, Copy)]
pub struct RegenCallback { env : Env, f : *const Function }

#[derive(Clone, Copy)]
pub enum StreamOperation {
  Timer,
  Poll{ input_signal: Ptr<Signal>, event_source : *mut (), poll_function : RegenCallback },
  State(Ptr<Signal>, RegenCallback),
  Map(Ptr<Signal>, RegenCallback),
  Sample(Ptr<Signal>),
  Merge(Ptr<Signal>, Ptr<Signal>),

  NativePoll{
    input_signal: Ptr<Signal>,
    event_source : *mut (),
    poll_function : fn(*mut (), *mut (), *const ()) -> bool,
  },
  NativeState(Ptr<Signal>, fn(*mut (), *const ())),
}

pub fn native_poll_signal<EventSource, OutputEvent, InputEvent>(
  el : Ptr<EventLoop>,
  input_signal : Ptr<Signal>,
  event_source : EventSource,
  f : fn(&mut EventSource, &mut OutputEvent, &InputEvent) -> bool
) -> Ptr<Signal>
{
  let event_source = alloc_val(event_source);
  let op = StreamOperation::NativePoll {
    input_signal,
    event_source,
    poll_function: unsafe { std::mem::transmute(f as *const ()) },
  };
  let output = alloc_bytes(std::mem::size_of::<OutputEvent>(), None);
  let s = create_signal(el, output, op);
  add_push_observer(el, input_signal, s);
  s
}

pub fn native_state_signal<State, Event>(
  el : Ptr<EventLoop>,
  parent : Ptr<Signal>,
  state : State,
  f : fn(&mut State, &Event)
) -> Ptr<Signal>
{
  let state = alloc_val(state);
  let op = StreamOperation::NativeState(parent, unsafe { std::mem::transmute(f as *const ()) });
  let s = create_signal(el, state, op);
  add_push_observer(el, parent, s);
  s
}


fn update_signal(update_number : u64, mut signal : Ptr<Signal>) -> bool {
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
      let env = poll_function.env;
      interpret::interpret_function(
        f,
        &[event_source as u64, signal.output as u64, event as u64],
        env, Some((&mut return_val) as *mut bool as *mut ()));
      return_val
    }
    NativePoll { input_signal, event_source, poll_function } => {
      poll_function(event_source, signal.output, input_signal.output)
    }
    State(parent, f) => {
      let event = parent.output;
      interpret::interpret_function(
        f.f as *const Function,
        &[signal.output as u64, event as u64],
        f.env, None);
      true
    }
    NativeState(parent, f) => {
      let event = parent.output;
      f(signal.output, event);
      true
    }
    Map(parent, f) => {
      let event = parent.output;
      interpret::interpret_function(
        f.f as *const Function,
        &[signal.output as u64, event as u64],
        f.env, None);
      true
    }
    Sample(sample_from) => {
      // this can just be done once when the signal is created,
      // as long as signals never reallocate their state pointers?
      // what happens if the state being sampled from doesn't _have_
      // a value yet?
      let TODO = ();
      signal.output = sample_from.output;
      true
    }
    Merge(signal_a, signal_b) => {
      // check if signal A has a new event
      if signal_a.last_update == update_number {
        signal.output = signal_a.output;
        return true;
      }
      // check if signal B has a new event
      if signal_b.last_update == update_number {
        signal.output = signal_b.output;
        return true;
      }
      // panic if neither of them do
      panic!("merge was triggered, but no events are available")
    }
  }
}

pub fn create_event_loop() -> Ptr<EventLoop> {
  perm(EventLoop {
    start_time: Instant::now(),
    id_counter: 0,
    observers: HashMap::new(),
    timers: vec![],
    update_count: 0,
  })
}

fn next_id(mut el : Ptr<EventLoop>) -> SignalId {
  let id = SignalId(el.id_counter);
  el.id_counter += 1;
  id
}

fn create_signal(
  el : Ptr<EventLoop>,
  output_addr : *mut (),
  operation : StreamOperation,
) -> Ptr<Signal>
{
  let signal = perm(Signal {
    id : next_id(el),
    event_loop: el,
    output: output_addr,
    operation,
    last_update: 0,
  });
  signal
}

pub fn destroy_signal(mut el : Ptr<EventLoop>, id : SignalId) {
  el.observers.remove(&id);
  for (_, obs) in el.observers.iter_mut() {
    obs.retain(|ob| ob.signal.id != id);
  }
}

pub fn create_timer(mut el : Ptr<EventLoop>, current_millisecond : i64, millisecond_interval : i64) -> Ptr<Signal> {
  let timer_state = TimerState {
    millisecond_interval,
    next_tick_millisecond: current_millisecond + millisecond_interval,
  };
  let state = alloc_val(timer_state);
  let s = create_signal(el, state, StreamOperation::Timer);
  el.timers.push(s);
  s
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_signal_input(event_loop : Ptr<EventLoop>, signal : Ptr<Signal>) {
  let TODO = (); // doesn't solve the diamond/glitch problem yet
  let should_push = update_signal(event_loop.update_count, signal);
  if should_push {
    push_to_observers(event_loop, signal);
  }
}

fn push_to_observers(event_loop : Ptr<EventLoop>, signal : Ptr<Signal>) {
  if let Some(observers) = event_loop.observers.get(&signal.id) {
    for &ob in observers {
      if ob.push_changes {
        handle_signal_input(event_loop, ob.signal);
      }
    }
  }
}

fn handle_timer_pulse(mut event_loop : Ptr<EventLoop>, signal : Ptr<Signal>, current_time : i64) {
  if let StreamOperation::Timer = signal.operation {
    event_loop.update_count += 1;
    push_to_observers(event_loop, signal);
    let timer : &mut TimerState = unsafe {
      &mut *(signal.output as *mut TimerState)
    };
    timer.next_tick_millisecond = current_time + timer.millisecond_interval;
  }
  else {
    panic!("expected timer");
  }
}

pub fn start_loop(event_loop : Ptr<EventLoop>) {
  let start = event_loop.start_time;
  loop {
    if event_loop.timers.is_empty() {
      // there will never be another event
      return;
    }
    // figure out which timer will tick next
    let (signal, mut timer) =
      event_loop.timers.iter().map(|&s| {
        let t : Ptr<TimerState> = Ptr::from_ptr(s.output as *mut TimerState);
        (s, t)
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
    handle_timer_pulse(event_loop, signal, now);
  }
}

fn add_push_observer(mut el : Ptr<EventLoop>, parent : Ptr<Signal>, signal : Ptr<Signal>) {
  let ob = Observer { signal, push_changes: true };
  el.observers.entry(parent.id).or_insert(vec![]).push(ob);
}

fn add_sample_observer(mut el : Ptr<EventLoop>, parent : Ptr<Signal>, signal : Ptr<Signal>) {
  let ob = Observer { signal, push_changes: false };
  el.observers.entry(parent.id).or_insert(vec![]).push(ob);
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
  el : Ptr<EventLoop>,
  output_type : TypeHandle,
  initial_value : Option<*const ()>,
  operation : StreamOperation,
) -> Ptr<Signal>
{
  let output = alloc_bytes(output_type.size_of as usize, initial_value);
  let signal = perm(Signal {
    id: next_id(el),
    event_loop: el,
    output,
    operation,
    last_update: 0,
  });
  env::register_signal(env, signal.id);
  signal
}

// ----------- Define FFI to call from regen -------------

pub mod ffi {
  use super::*;

  pub extern "C" fn register_tick_signal(
    env : Env,
    el : Ptr<EventLoop>,
    millisecond_interval : i64,
  ) -> Ptr<Signal>
  {
    let now = current_millisecond(el.start_time);
    let signal = create_timer(el, now, millisecond_interval);
    env::register_signal(env, signal.id);
    signal
  }

  pub extern "C" fn register_state_signal(
    env : Env,
    el : Ptr<EventLoop>,
    input_signal : Ptr<Signal>,
    state_type : TypeHandle,
    initial_state : *const (),
    update_function : *const Function,
  ) -> Ptr<Signal>
  {
    let op = StreamOperation::State(input_signal, RegenCallback { env, f: update_function });
    let signal = create_regen_signal(env, el, state_type, Some(initial_state), op);
    add_push_observer(el, input_signal, signal);
    signal
  }

  pub extern "C" fn register_poll_signal(
    env : Env,
    el : Ptr<EventLoop>,
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
      poll_function: RegenCallback { env, f: poll_function },
    };
    let signal = create_regen_signal(env, el, event_type, None, op);
    add_push_observer(el, input_signal, signal);
    signal
  }

  pub extern "C" fn register_map_signal(
    env : Env,
    el : Ptr<EventLoop>,
    input_signal : Ptr<Signal>,
    output_type : TypeHandle,
    map_function : *const Function,
  ) -> Ptr<Signal>
  {
    let op = StreamOperation::Map(input_signal, RegenCallback { env, f: map_function });
    let signal = create_regen_signal(env, el, output_type, None, op);
    add_push_observer(el, input_signal, signal);
    signal
  }

  pub extern "C" fn register_merge_signal(
    env : Env,
    el : Ptr<EventLoop>,
    signal_a : Ptr<Signal>,
    signal_b : Ptr<Signal>,
    output_type : TypeHandle,
  ) -> Ptr<Signal>
  {
    let op = StreamOperation::Merge(signal_a, signal_b);
    let signal = create_regen_signal(env, el, output_type, None, op);
    add_push_observer(el, signal_a, signal);
    add_push_observer(el, signal_b, signal);
    signal
  }

  pub extern "C" fn register_sample_signal(
    env : Env,
    el : Ptr<EventLoop>,
    trigger_signal : Ptr<Signal>,
    state_signal : Ptr<Signal>,
    output_type : TypeHandle,
  ) -> Ptr<Signal>
  {
    let op = StreamOperation::Sample(state_signal);
    let signal = create_regen_signal(env, el, output_type, None, op);
    add_push_observer(el, trigger_signal, signal);
    add_sample_observer(el, state_signal, signal);
    signal
  }
}
