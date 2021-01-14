
use std::{hash::Hash, thread};
use std::time;
use std::collections::HashMap;
use perm_alloc::{Perm, perm};

use time::{Duration, Instant};

use crate::perm_alloc;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct StreamId(u64);


#[derive(Clone)]
pub struct StreamGraph {
  id_counter : u64,
  observers : HashMap<StreamId, Vec<Perm<Stream>>>,
  timers : Vec<Perm<Stream>>,
}

#[derive(Copy, Clone)]
pub struct TimerState {
  pub millisecond_interval : i64,
  pub next_tick_millisecond : i64,
}

#[derive(Copy, Clone)]
pub struct Stream {
  pub id : StreamId,
  pub state : *mut (),
  pub handle_event : unsafe fn (state : *mut (), event : *const ()) -> bool,
}

#[derive(Clone, Copy)]
struct Observer {
  stream : Perm<Stream>,
  push_events : bool,
}

impl StreamGraph {
  pub fn new() -> Self {
    StreamGraph {
      id_counter: 0,
      observers: HashMap::new(),
      timers: vec![],
    }
  }

  fn create_orphan_stream<Event, State>(
    &mut self, state : State,
    handler : fn(&mut State, &Event) -> bool,
  ) -> Perm<Stream>
  {
    let id = StreamId(self.id_counter);
    self.id_counter += 1;
    let stream = perm(Stream {
      id,
      state: Perm::to_ptr(perm(state)) as *mut (),
      handle_event: unsafe { std::mem::transmute(handler as *const ()) },
    });
    stream
  }

  pub fn create_stream<Event, State>(
    &mut self, parent : Perm<Stream>, state : State,
    handler : fn(&mut State, &Event) -> bool,
  ) -> Perm<Stream>
  {
    let s = self.create_orphan_stream(state, handler);
    self.observers.entry(parent.id).or_insert(vec![]).push(s);
    s
  }
  
  pub fn create_timer(&mut self, current_millisecond : i64, millisecond_interval : i64) -> Perm<Stream> {
    let timer_state = TimerState {
      millisecond_interval,
      next_tick_millisecond: current_millisecond + millisecond_interval,
    };
    let s = self.create_orphan_stream(timer_state, |timer, current_time : &i64| {
      timer.next_tick_millisecond = *current_time + timer.millisecond_interval;
      true
    });
    self.timers.push(s);
    s
  }
}

fn current_millisecond(start : Instant) -> i64 {
  Instant::now().duration_since(start).as_millis() as i64
}

fn handle_stream_input(graph : Perm<StreamGraph>, stream : Perm<Stream>, event : *const ()) {
  let TODO = (); // doesn't solve the diamond/glitch problem yet
  let handler = stream.handle_event;
  let push_to_observers = unsafe { handler(stream.state, event) };
  if push_to_observers {
    if let Some(observers) = graph.observers.get(&stream.id) {
      for &o in observers {
        handle_stream_input(graph, o, stream.state);
      }
    }
  }
}

pub fn start_loop(graph : Perm<StreamGraph>) {
  let start = Instant::now();
  loop {
    if graph.timers.is_empty() {
      // there will never be another event
      return;
    }
    // figure out which timer will tick next
    let (stream, mut timer) =
      graph.timers.iter().map(|&s| {
        let t : Perm<TimerState> = Perm::from_ptr(s.state as *mut TimerState);
        (s, t)
      })
      .min_by_key(|x| x.1.next_tick_millisecond)
      .unwrap();
    // wait for it to tick
    let now = current_millisecond(start);
    let wait_time = timer.next_tick_millisecond - now;
    if wait_time > 0 {
      let TODO = (); // this sleep function seems to be extremely inaccurate
      thread::sleep(Duration::from_millis(wait_time as u64));
    }
    // handle the timer event
    let now = current_millisecond(start);
    timer.next_tick_millisecond = now + timer.millisecond_interval;
    handle_stream_input(graph, stream, (&now) as *const i64 as *const ());
  }
}
