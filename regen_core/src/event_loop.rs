
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

#[derive(Copy, Clone)]
pub struct Stream {
  pub id : StreamId,
  pub event_loop : Ptr<EventLoop>,
  pub output : *mut (),
  pub operation : StreamOperation,
  pub last_update : u64,
}

#[derive(Copy, Clone)]
struct Observer {
  stream : Ptr<Stream>,
  push_changes : bool,
}

#[derive(Clone)]
pub struct EventLoop {
  start_time : Instant,
  id_counter : u64,
  observers : HashMap<StreamId, Vec<Observer>>,
  timers : Vec<Ptr<Stream>>,
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
  Poll{ input_stream: Ptr<Stream>, event_source : *mut (), poll_function : RegenCallback },
  State(Ptr<Stream>, RegenCallback),
  Map(Ptr<Stream>, RegenCallback),
  Sample(Ptr<Stream>),
  Merge(Ptr<Stream>, Ptr<Stream>),

  NativePoll{
    input_stream: Ptr<Stream>,
    event_source : *mut (),
    poll_function : fn(*mut (), *mut (), *const ()) -> bool,
  },
  NativeState(Ptr<Stream>, fn(*mut (), *const ())),
}

pub fn native_poll_stream<EventSource, OutputEvent, InputEvent>(
  el : Ptr<EventLoop>,
  input_stream : Ptr<Stream>,
  event_source : EventSource,
  f : fn(&mut EventSource, &mut OutputEvent, &InputEvent) -> bool
) -> Ptr<Stream>
{
  let event_source = alloc_val(event_source);
  let op = StreamOperation::NativePoll {
    input_stream,
    event_source,
    poll_function: unsafe { std::mem::transmute(f as *const ()) },
  };
  let output = alloc_bytes(std::mem::size_of::<OutputEvent>(), None);
  let s = create_stream(el, output, op);
  add_push_observer(el, input_stream, s);
  s
}

pub fn native_state_stream<State, Event>(
  el : Ptr<EventLoop>,
  parent : Ptr<Stream>,
  state : State,
  f : fn(&mut State, &Event)
) -> Ptr<Stream>
{
  let state = alloc_val(state);
  let op = StreamOperation::NativeState(parent, unsafe { std::mem::transmute(f as *const ()) });
  let s = create_stream(el, state, op);
  add_push_observer(el, parent, s);
  s
}


fn update_stream(update_number : u64, mut stream : Ptr<Stream>) -> bool {
  use StreamOperation::*;
  stream.last_update = update_number;
  match stream.operation {
    Timer => {
      panic!("timer streams should never be triggered by other streams");
    }
    Poll { input_stream, event_source, poll_function } => {
      let event = input_stream.output;
      let mut return_val = true;
      let f = poll_function.f as *const Function;
      let env = poll_function.env;
      interpret::interpret_function(
        f,
        &[event_source as u64, stream.output as u64, event as u64],
        env, Some((&mut return_val) as *mut bool as *mut ()));
      return_val
    }
    NativePoll { input_stream, event_source, poll_function } => {
      poll_function(event_source, stream.output, input_stream.output)
    }
    State(parent, f) => {
      let event = parent.output;
      interpret::interpret_function(
        f.f as *const Function,
        &[stream.output as u64, event as u64],
        f.env, None);
      true
    }
    NativeState(parent, f) => {
      let event = parent.output;
      f(stream.output, event);
      true
    }
    Map(parent, f) => {
      let event = parent.output;
      interpret::interpret_function(
        f.f as *const Function,
        &[stream.output as u64, event as u64],
        f.env, None);
      true
    }
    Sample(sample_from) => {
      // this can just be done once when the stream is created,
      // as long as streams never reallocate their state pointers?
      // what happens if the state being sampled from doesn't _have_
      // a value yet?
      let TODO = ();
      stream.output = sample_from.output;
      true
    }
    Merge(stream_a, stream_b) => {
      // check if stream A has a new event
      if stream_a.last_update == update_number {
        stream.output = stream_a.output;
        return true;
      }
      // check if stream B has a new event
      if stream_b.last_update == update_number {
        stream.output = stream_b.output;
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

fn next_id(mut el : Ptr<EventLoop>) -> StreamId {
  let id = StreamId(el.id_counter);
  el.id_counter += 1;
  id
}

fn create_stream(
  el : Ptr<EventLoop>,
  output_addr : *mut (),
  operation : StreamOperation,
) -> Ptr<Stream>
{
  let stream = perm(Stream {
    id : next_id(el),
    event_loop: el,
    output: output_addr,
    operation,
    last_update: 0,
  });
  stream
}

pub fn destroy_stream(mut el : Ptr<EventLoop>, id : StreamId) {
  el.observers.remove(&id);
  for (_, obs) in el.observers.iter_mut() {
    obs.retain(|ob| ob.stream.id != id);
  }
}

pub fn create_timer(mut el : Ptr<EventLoop>, current_millisecond : i64, millisecond_interval : i64) -> Ptr<Stream> {
  let timer_state = TimerState {
    millisecond_interval,
    next_tick_millisecond: current_millisecond + millisecond_interval,
  };
  let state = alloc_val(timer_state);
  let s = create_stream(el, state, StreamOperation::Timer);
  el.timers.push(s);
  s
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_stream_input(event_loop : Ptr<EventLoop>, stream : Ptr<Stream>) {
  let TODO = (); // doesn't solve the diamond/glitch problem yet
  let should_push = update_stream(event_loop.update_count, stream);
  if should_push {
    push_to_observers(event_loop, stream);
  }
}

fn push_to_observers(event_loop : Ptr<EventLoop>, stream : Ptr<Stream>) {
  if let Some(observers) = event_loop.observers.get(&stream.id) {
    for &ob in observers {
      if ob.push_changes {
        handle_stream_input(event_loop, ob.stream);
      }
    }
  }
}

fn handle_timer_pulse(mut event_loop : Ptr<EventLoop>, stream : Ptr<Stream>, current_time : i64) {
  if let StreamOperation::Timer = stream.operation {
    event_loop.update_count += 1;
    push_to_observers(event_loop, stream);
    let timer : &mut TimerState = unsafe {
      &mut *(stream.output as *mut TimerState)
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
    let (stream, mut timer) =
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
    handle_timer_pulse(event_loop, stream, now);
  }
}

fn add_push_observer(mut el : Ptr<EventLoop>, parent : Ptr<Stream>, stream : Ptr<Stream>) {
  let ob = Observer { stream, push_changes: true };
  el.observers.entry(parent.id).or_insert(vec![]).push(ob);
}

fn add_sample_observer(mut el : Ptr<EventLoop>, parent : Ptr<Stream>, stream : Ptr<Stream>) {
  let ob = Observer { stream, push_changes: false };
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

fn create_regen_stream(
  env : Env,
  el : Ptr<EventLoop>,
  output_type : TypeHandle,
  initial_value : Option<*const ()>,
  operation : StreamOperation,
) -> Ptr<Stream>
{
  let output = alloc_bytes(output_type.size_of as usize, initial_value);
  let stream = perm(Stream {
    id: next_id(el),
    event_loop: el,
    output,
    operation,
    last_update: 0,
  });
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
    let op = StreamOperation::State(input_stream, RegenCallback { env, f: update_function });
    let stream = create_regen_stream(env, el, state_type, Some(initial_state), op);
    add_push_observer(el, input_stream, stream);
    stream
  }

  pub extern "C" fn register_poll_stream(
    env : Env,
    el : Ptr<EventLoop>,
    input_stream : Ptr<Stream>,
    event_source_type : TypeHandle,
    event_source : *const (),
    event_type : TypeHandle,
    poll_function : *const Function,
  ) -> Ptr<Stream>
  {
    let event_source = alloc_bytes(event_source_type.size_of as usize, Some(event_source));
    let op = StreamOperation::Poll {
      input_stream,
      event_source,
      poll_function: RegenCallback { env, f: poll_function },
    };
    let stream = create_regen_stream(env, el, event_type, None, op);
    add_push_observer(el, input_stream, stream);
    stream
  }

  pub extern "C" fn register_map_stream(
    env : Env,
    el : Ptr<EventLoop>,
    input_stream : Ptr<Stream>,
    output_type : TypeHandle,
    map_function : *const Function,
  ) -> Ptr<Stream>
  {
    let op = StreamOperation::Map(input_stream, RegenCallback { env, f: map_function });
    let stream = create_regen_stream(env, el, output_type, None, op);
    add_push_observer(el, input_stream, stream);
    stream
  }

  pub extern "C" fn register_merge_stream(
    env : Env,
    el : Ptr<EventLoop>,
    stream_a : Ptr<Stream>,
    stream_b : Ptr<Stream>,
    output_type : TypeHandle,
  ) -> Ptr<Stream>
  {
    let op = StreamOperation::Merge(stream_a, stream_b);
    let stream = create_regen_stream(env, el, output_type, None, op);
    add_push_observer(el, stream_a, stream);
    add_push_observer(el, stream_b, stream);
    stream
  }

  pub extern "C" fn register_sample_stream(
    env : Env,
    el : Ptr<EventLoop>,
    trigger_stream : Ptr<Stream>,
    state_stream : Ptr<Stream>,
    output_type : TypeHandle,
  ) -> Ptr<Stream>
  {
    let op = StreamOperation::Sample(state_stream);
    let stream = create_regen_stream(env, el, output_type, None, op);
    add_push_observer(el, trigger_stream, stream);
    add_sample_observer(el, state_stream, stream);
    stream
  }
}
