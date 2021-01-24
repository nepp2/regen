
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
  pub operation : StreamOperation,
}

#[derive(Clone, Copy)]
pub struct RegenCallback { env : Env, f : *const Function }

#[derive(Clone, Copy)]
pub enum StreamOperation {
  Filter(RegenCallback),
  State(RegenCallback),
  // Sample,
  // Merge,
  NativeFilter(fn(*mut (), *const ()) -> bool),
  NativeState(fn(*mut (), *const ())),
}

pub fn native_filter_op<State, Event>(f : fn(&mut State, &Event) -> bool) -> StreamOperation {
  StreamOperation::NativeFilter(unsafe { std::mem::transmute(f as *const ()) })
}

pub fn native_state_op<State, Event>(f : fn(&mut State, &Event)) -> StreamOperation {
  StreamOperation::NativeState(unsafe { std::mem::transmute(f as *const ()) })
}

impl StreamOperation {
  fn process(&self, state : *mut (), event : *const ()) -> bool {
    use StreamOperation::*;
    match self {
      Filter(f) => {
        let mut return_val = true;
        interpret::interpret_function(
          f.f as *const Function,
          &[state as u64, event as u64],
          f.env, Some((&mut return_val) as *mut bool as *mut ()));
        return_val
      }
      State(f) => {
        interpret::interpret_function(
          f.f as *const Function,
          &[state as u64, event as u64],
          f.env, None);
        true
      }
      NativeFilter(f) => {
        f(state, event)
      }
      NativeState(f) => {
        f(state, event);
        true
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

fn create_orphan_stream<State>(
  el : Ptr<EventLoop>, state : State,
  operation : StreamOperation,
) -> Ptr<Stream>
{
  let stream = perm(Stream {
    id : next_id(el),
    event_loop: el,
    state: Ptr::to_ptr(perm(state)) as *mut (),
    operation,
  });
  stream
}

pub fn create_stream<State>(
  mut el : Ptr<EventLoop>,
  parent : Ptr<Stream>,
  state : State,
  operation : StreamOperation,
) -> Ptr<Stream>
{
  let s = create_orphan_stream(el, state, operation);
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
  let op = native_state_op(|timer : &mut TimerState, current_time : &i64| {
    timer.next_tick_millisecond = *current_time + timer.millisecond_interval;
  });
  let s = create_orphan_stream(el, timer_state, op);
  el.timers.push(s);
  s
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_stream_input(event_loop : Ptr<EventLoop>, stream : Ptr<Stream>, event : *const ()) {
  let TODO = (); // doesn't solve the diamond/glitch problem yet
  let push_to_observers = stream.operation.process(stream.state, event);
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
      spin_sleep::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    let now = current_millisecond(start);
    timer.next_tick_millisecond = now + timer.millisecond_interval;
    handle_stream_input(event_loop, stream, (&now) as *const i64 as *const ());
  }
}

fn register_regen_stream(
  env : Env,
  mut el : Ptr<EventLoop>,
  input_stream : Ptr<Stream>,
  state_type : TypeHandle,
  initial_state : *const (),
  operation : StreamOperation,
) -> Ptr<Stream>
{
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
    operation,
  });
  el.observers.entry(input_stream.id).or_insert(vec![]).push(stream);
  env::register_stream(env, stream.id);
  stream
}

// ----------- Define FFI to call from regen -------------

pub mod ffi {
  use super::*;

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
    el : Ptr<EventLoop>,
    input_stream : Ptr<Stream>,
    state_type : TypeHandle,
    initial_state : *const (),
    update_function : *const Function,
  ) -> Ptr<Stream>
  {
    let op = StreamOperation::State(RegenCallback { env, f: update_function });
    register_regen_stream(env, el, input_stream, state_type, initial_state, op)
  }

  pub extern "C" fn register_filter_stream(
    env : Env,
    el : Ptr<EventLoop>,
    input_stream : Ptr<Stream>,
    state_type : TypeHandle,
    initial_state : *const (),
    update_function : *const Function,
  ) -> Ptr<Stream>
  {
    let op = StreamOperation::State(RegenCallback { env, f: update_function });
    register_regen_stream(env, el, input_stream, state_type, initial_state, op)
  }

  // pub extern "C" fn register_merge_stream(
  //   env : Env,
  //   el : Ptr<EventLoop>,
  //   merge_type : TypeHandle,
  //   stream_a : Ptr<Stream>,
  //   handler_a : *const Function,
  //   stream_b : Ptr<Stream>,
  //   handler_b : *const Function,
  // ) -> Ptr<Stream>
  // {
  //   let handle_event = Handler::regen(env, handler_a, true);
  //   let layout = Layout::from_size_align(merge_type.size_of as usize, 8).unwrap();
  //   let state = unsafe { alloc(layout) as *mut () };
  //   let stream = perm(Stream {
  //     id: next_id(el),
  //     event_loop: el,
  //     state,
  //     handle_event,
  //   });
  //   el.observers.entry(input_stream.id).or_insert(vec![]).push(stream);
  //   env::register_stream(env, stream.id);
  //   stream
  //   panic!("merge streams not yet supported")
  // }

  pub extern "C" fn sample_stream(
    env : Env,
    el : Ptr<EventLoop>,
    push_stream : Ptr<Stream>,
    sample_stream : Ptr<Stream>,
  ) -> Ptr<Stream>
  {
    panic!("sample streams not yet supported")
  }
}
