
use std::{hash::Hash, thread};
use std::time;
use std::collections::HashMap;
use perm_alloc::{Ptr, perm};

use time::{Duration, Instant};

use crate::{compile::Function, env::{self, Env}, interpret, perm_alloc};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct StreamId(pub u64);


#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  observers : HashMap<StreamId, Vec<Ptr<Stream>>>,
  timers : Vec<Ptr<Stream>>,
}

#[derive(Copy, Clone)]
pub struct TimerState {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
}

#[derive(Copy, Clone)]
pub struct Stream {
  pub id : StreamId,
  pub event_loop : Ptr<EventLoop>,
  pub state : *mut (),
  pub handle_event : Handler,
}

#[derive(Clone, Copy)]
pub enum HandlerType { Regen(Env), Native }

#[derive(Clone, Copy)]
pub struct Handler {
  handler_type : HandlerType,
  handler_ptr : *const (),
  is_polling_handler : bool,
}

impl Handler {
  fn regen(env : Env, handler : *const Function, is_polling_handler : bool) -> Self {
    Handler {
      handler_type: HandlerType::Regen(env),
      handler_ptr: handler as *const (),
      is_polling_handler,
    }
  }

  fn native(handler_ptr : *const (), is_polling_handler : bool) -> Self {
    Handler {
      handler_type: HandlerType::Native,
      handler_ptr,
      is_polling_handler,
    }
  }

  fn handle(&self, state : *mut (), event : *const ()) -> bool {
    use HandlerType::*;
    match self.handler_type {
      Regen(env) => {
        if self.is_polling_handler {
          panic!();
        }
        interpret::interpret_function(
          self.handler_ptr as *const Function,
          &[state as u64, event as u64],
          env, None);
        true
      }
      Native => {
        if self.is_polling_handler {
          let handler : extern "C" fn(*mut (), *const ()) -> bool =
            unsafe { std::mem::transmute(self.handler_ptr as *const ()) };
          handler(state, event)
        }
        else {
          let handler : extern "C" fn(*mut (), *const ()) =
            unsafe { std::mem::transmute(self.handler_ptr as *const ()) };
          handler(state, event);
          true
        }
      }
    }
  }
}

#[derive(Clone, Copy)]
struct Observer {
  stream : Ptr<Stream>,
  push_events : bool,
}

pub fn create_event_loop() -> Ptr<EventLoop> {
  perm(EventLoop {
    start_time: Instant::now(),
    id_counter: 0,
    observers: HashMap::new(),
    timers: vec![],
  })
}

fn next_id(mut el : Ptr<EventLoop>) -> StreamId {
  let id = StreamId(el.id_counter);
  el.id_counter += 1;
  id
}

fn create_orphan_stream<Event, State>(
  el : Ptr<EventLoop>, state : State,
  handler : fn(&mut State, &Event) -> bool,
) -> Ptr<Stream>
{
  let stream = perm(Stream {
    id : next_id(el),
    event_loop: el,
    state: Ptr::to_ptr(perm(state)) as *mut (),
    handle_event: Handler::native(handler as *const (), true),
  });
  stream
}

pub fn create_stream<Event, State>(
  mut el : Ptr<EventLoop>,
  parent : Ptr<Stream>,
  state : State,
  handler : fn(&mut State, &Event) -> bool,
) -> Ptr<Stream>
{
  let s = create_orphan_stream(el, state, handler);
  el.observers.entry(parent.id).or_insert(vec![]).push(s);
  s
}

pub fn destroy_stream(mut el : Ptr<EventLoop>, id : StreamId) {
  el.observers.remove(&id);
  for (_, obs) in el.observers.iter_mut() {
    obs.retain(|s| s.id != id);
  }
}

pub fn create_timer(mut el : Ptr<EventLoop>, current_millisecond : i64, millisecond_interval : i64) -> Ptr<Stream> {
  let timer_state = TimerState {
    millisecond_interval,
    next_tick_millisecond: current_millisecond + millisecond_interval,
  };
  let s = create_orphan_stream(el, timer_state, |timer, current_time : &i64| {
    timer.next_tick_millisecond = *current_time + timer.millisecond_interval;
    true
  });
  el.timers.push(s);
  s
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_stream_input(event_loop : Ptr<EventLoop>, stream : Ptr<Stream>, event : *const ()) {
  let TODO = (); // doesn't solve the diamond/glitch problem yet
  let push_to_observers = stream.handle_event.handle(stream.state, event);
  if push_to_observers {
    if let Some(observers) = event_loop.observers.get(&stream.id) {
      for &o in observers {
        handle_stream_input(event_loop, o, stream.state);
      }
    }
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
    let (stream, mut timer) =
      event_loop.timers.iter().map(|&s| {
        let t : Ptr<TimerState> = Ptr::from_ptr(s.state as *mut TimerState);
        (s, t)
      })
      .min_by_key(|x| x.1.next_tick_millisecond)
      .unwrap();
    // wait for it to tick
    let now = current_millisecond(start);
    let wait_time = timer.next_tick_millisecond - now;
    if wait_time > 0 {
      let TODO = (); // this sleep function seems to be extremely inaccurate
      spin_sleep::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    let now = current_millisecond(start);
    timer.next_tick_millisecond = now + timer.millisecond_interval;
    handle_stream_input(event_loop, stream, (&now) as *const i64 as *const ());
  }
}

// ----------- Define FFI to call from regen -------------

pub mod ffi {
  use super::*;
  use crate::{compile::Function, types::TypeHandle};

  pub extern "C" fn register_tick_stream(
    env : Env,
    el : Ptr<EventLoop>,
    millisecond_interval : i64,
  ) -> Ptr<Stream>
  {
    let now = current_millisecond(el.start_time);
    let stream = create_timer(el, now, millisecond_interval);
    env::register_stream(env, stream.id);
    stream
  }

  pub extern "C" fn register_state_stream(
    env : Env,
    mut el : Ptr<EventLoop>,
    input_stream : Ptr<Stream>,
    state_type : TypeHandle,
    initial_state : *const (),
    update_function : *const Function,
  ) -> Ptr<Stream>
  {
    use std::alloc::{alloc, Layout};
    let layout = Layout::from_size_align(state_type.size_of as usize, 8).unwrap();
    let state = unsafe {
      let ptr = alloc(layout) as *mut ();
      std::ptr::copy_nonoverlapping(initial_state, ptr, state_type.size_of as usize);
      ptr as *mut ()
    };
    let stream = perm(Stream {
      id: next_id(el),
      event_loop: el,
      state,
      handle_event: Handler::regen(env, update_function, false),
    });
    el.observers.entry(input_stream.id).or_insert(vec![]).push(stream);
    env::register_stream(env, stream.id);
    stream
  }
}
