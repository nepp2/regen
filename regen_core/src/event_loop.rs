
use std::{hash::Hash};
use std::time::{Duration, Instant};
use std::collections::HashMap;

use crate::{compile::Function, env::{CellIdentifier, CellUid, Env}, hotload, perm_alloc::{Ptr, perm}, types::TypeHandle};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TriggerId(pub u64);

#[derive(Clone, Copy)]
pub struct ReactiveConstructor {
  pub value_type : TypeHandle,
  pub variant : ConstructorVariant,
}

#[derive(Clone, Copy)]
pub enum ConstructorVariant {
  Timer{ millisecond_interval : i64 },
  Poll{
    input : SignalInput,
    initial_value : Option<*const ()>,
    poll_function : *const Function,
  }
}

#[derive(Copy, Clone)]
pub enum UpdateVariant {
  Timer,
  Container,
}

pub type SignalInput = Ptr<CellIdentifier>;

#[derive(Copy, Clone)]
pub struct NativeHook {
  state : *mut (),
  callback: fn(Env, *mut ()),
}


#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  cell_mappings : HashMap<TriggerId, CellUid>,
  native_hooks : HashMap<TriggerId, NativeHook>,
  timers : HashMap<TriggerId, TimerState>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct TimerState {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
}

pub fn create_event_loop() -> Ptr<EventLoop> {
  perm(EventLoop {
    start_time: Instant::now(),
    id_counter: 0,
    cell_mappings: HashMap::new(),
    native_hooks: HashMap::new(),
    timers: HashMap::new(),
  })
}

fn next_id(mut el : Ptr<EventLoop>) -> TriggerId {
  let id = TriggerId(el.id_counter);
  el.id_counter += 1;
  id
}

pub fn remove_trigger(mut el : Ptr<EventLoop>, id : TriggerId) {
  el.cell_mappings.remove(&id);
  el.timers.remove(&id);
}

pub fn register_cell_timer(mut el : Ptr<EventLoop>, cell : CellUid, millisecond_interval : i64) -> TriggerId {
  let id = create_timer(el, millisecond_interval);
  el.cell_mappings.insert(id, cell);
  id
}

pub fn register_native_hook<Data>(
  mut el : Ptr<EventLoop>,
  millisecond_interval : i64,
  data : Ptr<Data>,
  f : fn(Env, Ptr<Data>),
)
{
  let timer_id = create_timer(el, millisecond_interval);
  let hook = {
    let state = Ptr::to_ptr(data) as *mut ();
    let callback = unsafe { std::mem::transmute(f as *const ()) };
    NativeHook{ state, callback }
  };
  el.native_hooks.insert(timer_id, hook);
}

fn create_timer(mut el : Ptr<EventLoop>, millisecond_interval : i64) -> TriggerId {
  let now = current_millisecond(el);
  let timer_state = TimerState {
    millisecond_interval,
    next_tick_millisecond: now + millisecond_interval,
  };
  let id = next_id(el);
  el.timers.insert(id, timer_state);
  id
}

pub fn current_millisecond(el : Ptr<EventLoop>) -> i64 {
  Instant::now().duration_since(el.start_time).as_millis() as i64
}

fn handle_timer_pulse(env : Env, id : TriggerId) {
  let mut el = env.event_loop;
  let now = current_millisecond(el);
  let timer = el.timers.get_mut(&id).unwrap();
  timer.next_tick_millisecond = now + timer.millisecond_interval;
  if let Some(nh) = el.native_hooks.get(&id) {
    (nh.callback)(env, nh.state);
  }
  if let Some(&cell) = env.event_loop.cell_mappings.get(&id) {
    hotload::update_timer_cell(env, cell, now);
  }
}

pub fn start_loop(env : Env) {
  let event_loop = env.event_loop;
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
    let now = current_millisecond(event_loop);
    let wait_time = timer.next_tick_millisecond - now;
    if wait_time > 0 {
      spin_sleep::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    handle_timer_pulse(env, id);
  }
}

// ----------- Define FFI to call from regen -------------

pub mod ffi {
  use super::*;

  pub extern "C" fn new_timer_constructor(
    env : Env,
    millisecond_interval : i64,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor {
      value_type: env.c.i64_tag,
      variant : ConstructorVariant::Timer { millisecond_interval }
    })
  }

  pub extern "C" fn new_state_constructor(
    input : SignalInput,
    value_type : TypeHandle,
    initial_value : *const (),
    poll_function : *const Function,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor {
      value_type,
      variant: ConstructorVariant::Poll {
        input,
        initial_value: Some(initial_value),
        poll_function,
      }
    })
  }

  pub extern "C" fn new_poll_constructor(
    input : SignalInput,
    value_type : TypeHandle,
    poll_function : *const Function,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor {
      value_type,
      variant: ConstructorVariant::Poll {
        input,
        initial_value: None,
        poll_function,
      }
    })
  }
}
