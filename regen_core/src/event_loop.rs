
use std::{hash::Hash};
use std::time::{Duration, Instant};
use std::collections::HashMap;
use std::alloc::{alloc, Layout};

use crate::{compile::Function, env::{CellUid, Env, RegenValue}, hotload, interpret, perm_alloc::{Ptr, perm}, types::{self, TypeHandle}};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct LoopId(pub u64);

#[derive(Clone, Copy)]
pub enum ReactiveConstructor {
  Timer{ millisecond_interval : i64 },
  Poll{
    input : SignalInput,
    value_type : TypeHandle,
    initial_value : Option<*const ()>,
    poll_function : *const Function,
  }
}

#[derive(Copy, Clone)]
pub enum UpdateVariant {
  Timer,
  Container,
}

pub type SignalInput = Ptr<CellUid>;

#[derive(Copy, Clone)]
pub struct CellMapping {
  id : LoopId,
  variant : UpdateVariant,
  cell : CellUid,
}

#[derive(Copy, Clone)]
pub struct Observer {
  state : RegenValue,
  poll_function : *const Function,
}

#[derive(Copy, Clone)]
pub struct NativeHook {
  state : *mut (),
  callback: fn(Env, *mut ()),
}


#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  cell_mappings : HashMap<LoopId, Ptr<CellMapping>>,
  native_hooks : HashMap<LoopId, NativeHook>,
  observers : HashMap<LoopId, Observer>,
  timers : HashMap<LoopId, TimerState>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TimerState {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
}

pub fn update_container(env : Env, id : LoopId, input : RegenValue) -> bool {
  let c = env.event_loop.observers[&id];
  let returns_bool = {
    let f = unsafe { &*c.poll_function };
    let return_type = types::type_as_function(&f.t).unwrap().returns;
    return_type == env.c.bool_tag
  };
  if returns_bool {
    let mut return_val = true;
    interpret::interpret_function(
      c.poll_function,
      &[c.state.ptr as u64, input.ptr as u64],
      Some((&mut return_val) as *mut bool as *mut ()));
    return_val
  }
  else {
    interpret::interpret_function(
      c.poll_function,
      &[c.state.ptr as u64, input.ptr as u64],
      None);
    true
  }
}

pub fn create_event_loop() -> Ptr<EventLoop> {
  perm(EventLoop {
    start_time: Instant::now(),
    id_counter: 0,
    observers: HashMap::new(),
    cell_mappings: HashMap::new(),
    native_hooks: HashMap::new(),
    timers: HashMap::new(),
  })
}

fn next_id(mut el : Ptr<EventLoop>) -> LoopId {
  let id = LoopId(el.id_counter);
  el.id_counter += 1;
  id
}

pub fn register_reactive_cell(env : Env, cell : CellUid, constructor : Ptr<ReactiveConstructor>) -> RegenValue {
  let mut el = env.event_loop;
  match *constructor {
    ReactiveConstructor::Timer { millisecond_interval } => {
      let id = create_timer(env, millisecond_interval);
      create_cell_mapping(el, id, UpdateVariant::Timer, cell);
      let now : i64 = current_millisecond(el.start_time);
      RegenValue { t: env.c.i64_tag, ptr: alloc_val(now) }
    }
    ReactiveConstructor::Poll { input, value_type, initial_value, poll_function } => {
      let state = RegenValue {
        t: value_type,
        ptr: alloc_bytes(value_type.size_of as usize, initial_value),
      };
      let id = next_id(el);
      let o = Observer {
        state,
        poll_function,
      };
      el.observers.insert(id, o);
      create_cell_mapping(el, id, UpdateVariant::Container, cell);
      state
    }
  }
}

pub fn remove_signal(mut el : Ptr<EventLoop>, id : LoopId) {
  el.observers.remove(&id);
  el.cell_mappings.remove(&id);
  el.timers.remove(&id);
}

pub fn register_native_hook<Data>(
  env : Env,
  millisecond_interval : i64,
  data : Ptr<Data>,
  f : fn(Env, Ptr<Data>),
)
{
  let timer_id = create_timer(env, millisecond_interval);
  let mut el = env.event_loop;
  let hook = {
    let state = Ptr::to_ptr(data) as *mut ();
    let callback = unsafe { std::mem::transmute(f as *const ()) };
    NativeHook{ state, callback }
  };
  el.native_hooks.insert(timer_id, hook);
}

pub fn create_timer(env : Env, millisecond_interval : i64) -> LoopId {
  let now = current_millisecond(env.event_loop.start_time);
  let timer_state = TimerState {
    millisecond_interval,
    next_tick_millisecond: now + millisecond_interval,
  };
  let mut el = env.event_loop;
  let id = next_id(el);
  el.timers.insert(id, timer_state);
  id
}

pub fn create_cell_mapping(mut el : Ptr<EventLoop>, id : LoopId, variant : UpdateVariant, cell : CellUid) -> Ptr<CellMapping> {
  let s = perm(CellMapping { id, variant, cell });
  el.cell_mappings.insert(id, s);
  s
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_timer_pulse(env : Env, id : LoopId) {
  let mut el = env.event_loop;
  let now = current_millisecond(el.start_time);
  let timer = el.timers.get_mut(&id).unwrap();
  timer.next_tick_millisecond = now + timer.millisecond_interval;
  if let Some(nh) = el.native_hooks.get(&id) {
    (nh.callback)(env, nh.state);
  }
  if let Some(signal) = env.event_loop.cell_mappings.get(&id) {
    hotload::update_timer_cell(env, signal.cell, now);
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
    let (&id, timer) =
      event_loop.timers.iter()
      .min_by_key(|x| x.1.next_tick_millisecond)
      .unwrap();
    // wait for it to tick
    let now = current_millisecond(start);
    let wait_time = timer.next_tick_millisecond - now;
    if wait_time > 0 {
      spin_sleep::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    handle_timer_pulse(env, id);
  }
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

// ----------- Define FFI to call from regen -------------

pub mod ffi {
  use super::*;

  pub extern "C" fn new_timer_constructor(
    millisecond_interval : i64,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor::Timer { millisecond_interval })
  }

  pub extern "C" fn new_state_constructor(
    input : SignalInput,
    value_type : TypeHandle,
    initial_value : *const (),
    poll_function : *const Function,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor::Poll {
      input,
      value_type,
      initial_value: Some(initial_value),
      poll_function,
    })
  }

  pub extern "C" fn new_poll_constructor(
    input : SignalInput,
    value_type : TypeHandle,
    poll_function : *const Function,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor::Poll {
      input,
      value_type,
      initial_value: None,
      poll_function,
    })
  }
}
