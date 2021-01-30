
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use std::{sync::mpsc::{Receiver, TryRecvError, channel}};
use std::time::Duration;

use crate::ffi_libs;

use regen_core::{
  new_env,
  env::{Env, get_event_loop},
  hotload::{self, HotloadState},
  event_loop::{
    self, TimerState,
    native_poll_stream,
    native_state_stream,
  },
};
use std::fs;

fn hotload_file(env : Env, hs : &mut HotloadState, path : &str) {
  println!();
  println!();
  println!("-----------------------------------");
  println!("Hotloading file '{}'", path);
  println!("-----------------------------------");
  println!();
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  hotload::hotload_changes(path, &code, env, hs);
}

struct WatchState {
  rx : Receiver<DebouncedEvent>,
  watcher : RecommendedWatcher,
}

struct CompilerState {
  env : Env,
  hs : HotloadState,
  path : String,
}

pub fn watch_file(path : &str) {
  let env = new_env();
  ffi_libs::bind_libs(env);

  let (tx, rx) = channel();
  let mut watcher = watcher(tx, Duration::from_millis(500)).unwrap();
  watcher.watch(path, RecursiveMode::Recursive).unwrap();
  let watch_state = WatchState { rx, watcher };

  let event_loop = get_event_loop(env);

  let timer = event_loop::create_timer(event_loop, 0, 10);
  let file_changes = native_poll_stream(event_loop, timer, watch_state,
    |ws : &mut WatchState, _output : &mut i64, _input : &TimerState| {
      match ws.rx.try_recv() {
        Ok(event) => {
          match event {
            DebouncedEvent::Write(_) => {
              true
            }
            _ => false,
          }
        },
        Err(e) => match e {
          TryRecvError::Disconnected => {
            println!("watch error: {:?}", e);
            false
          }
          TryRecvError::Empty => false,
        },
      }
    }
  );

  let mut hs = HotloadState::new();
  hotload_file(env, &mut hs, path);
  let cs = CompilerState { env, hs, path: path.to_string() };

  native_state_stream(event_loop, file_changes, cs,
    |cs : &mut CompilerState, _ : &WatchState| {
      hotload_file(cs.env, &mut cs.hs, &cs.path);
    }
  );
  
  event_loop::start_loop(event_loop);
}

