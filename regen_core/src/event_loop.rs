
use std::{hash::Hash};
use std::time::{Duration, Instant};
use std::collections::HashMap;

use crate::{compile::Function, env::{CellIdentifier, CellUid, Env}, ffi_libs::RegenString, hotload, perm_alloc::{Ptr, perm}, types::TypeHandle};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TimerId(pub u64);

#[derive(Clone, Copy)]
pub struct ReactiveConstructor {
  pub value_type : TypeHandle,
  pub variant : ConstructorVariant,
}

#[derive(Clone, Copy)]
pub enum ConstructorVariant {
  Watcher { file_path : RegenString },
  Timer { millisecond_interval : i64 },
  Poll {
    input : SignalInput,
    initial_value : Option<*const ()>,
    poll_function : Ptr<Function>,
  }
}

#[derive(Copy, Clone)]
pub enum UpdateVariant {
  Timer,
  Container,
}

pub type SignalInput = Ptr<CellIdentifier>;

#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  timers : HashMap<TimerId, Timer>,
  watchers : HashMap<RegenString, Vec<CellUid>>,
  watch_requests : Vec<RegenString>,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Timer {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
  pub cell : CellUid,
}

pub fn create_event_loop() -> Ptr<EventLoop> {
  perm(EventLoop {
    start_time: Instant::now(),
    id_counter: 0,
    timers: HashMap::new(),
    watchers: HashMap::new(),
    watch_requests: vec![],
  })
}

fn next_id(mut el : Ptr<EventLoop>) -> TimerId {
  let id = TimerId(el.id_counter);
  el.id_counter += 1;
  id
}

pub fn remove_timer(mut el : Ptr<EventLoop>, id : TimerId) {
  el.timers.remove(&id);
}

pub fn register_timer(mut el : Ptr<EventLoop>, cell : CellUid, millisecond_interval : i64) -> TimerId {
  let now = current_millisecond(el);
  let timer_state = Timer {
    millisecond_interval,
    next_tick_millisecond: now + millisecond_interval,
    cell,
  };
  let id = next_id(el);
  el.timers.insert(id, timer_state);
  id
}

pub fn register_watcher(mut el : Ptr<EventLoop>, cell : CellUid, file_path : RegenString) {
  el.watchers.entry(file_path).or_insert(vec![]).push(cell);
  el.watch_requests.push(file_path);
}

pub fn remove_watcher(mut el : Ptr<EventLoop>, cell : CellUid, file_path : RegenString) {
  let cs = el.watchers.entry(file_path).or_insert(vec![]);
  if let Some(i) = cs.iter().position(|&uid| uid == cell) {
    cs.remove(i);
  }
}

pub fn current_millisecond(el : Ptr<EventLoop>) -> i64 {
  Instant::now().duration_since(el.start_time).as_millis() as i64
}

fn handle_timer_pulse(env : Env, id : TimerId) {
  let mut el = env.event_loop;
  let now = current_millisecond(el);
  let timer = el.timers.get_mut(&id).unwrap();
  timer.next_tick_millisecond = now + timer.millisecond_interval;
  hotload::update_timer_cell(env, timer.cell, now);
}

pub fn process_watch_requests(mut env : Env, mut process : impl FnMut(RegenString)) {
  for s in env.event_loop.watch_requests.drain(..) {
    process(s);
  }
}

pub fn handle_watch_event(env : Env, file_path : RegenString) {
  let event_loop = env.event_loop;
  if let Some(cells) = event_loop.watchers.get(&file_path) {
    hotload::update_watcher_cells(env, cells);
  }
}

pub fn handle_next_timer(env : Env, max_wait_millis : i64) {
  let event_loop = env.event_loop;
  // handle watcher event
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
  if wait_time < max_wait_millis {
    if wait_time > 0 {
      spin_sleep::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    handle_timer_pulse(env, id);
  }
  else {
    spin_sleep::sleep(Duration::from_millis(max_wait_millis as u64));
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

  pub extern "C" fn new_watcher_constructor(
    env : Env,
    file_path : Ptr<RegenString>,
  ) -> Ptr<ReactiveConstructor>
  {
    perm(ReactiveConstructor {
      value_type: env.c.string_tag,
      variant : ConstructorVariant::Watcher { file_path: *file_path }
    })
  }

  pub extern "C" fn new_state_constructor(
    input : SignalInput,
    value_type : TypeHandle,
    initial_value : *const (),
    poll_function : Ptr<Function>,
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
    poll_function : Ptr<Function>,
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
