
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use std::{sync::mpsc::{Receiver, TryRecvError, channel}};
use std::time::Duration;

use crate::ffi_libs;

use regen_core::{
  new_env,
  env::Env,
  hotload,
  perm_alloc::{perm, Ptr},
  event_loop,
};
use std::fs;

fn hotload_file(env : Env, path : &str) {
  println!();
  println!();
  println!("-----------------------------------");
  println!("Hotloading file '{}'", path);
  println!("-----------------------------------");
  println!();
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  hotload::hotload_live_module(env, path, &code);
}

struct WatchState {
  rx : Receiver<DebouncedEvent>,
  watcher : RecommendedWatcher,
  path : String,
}

pub fn watch_file(path : &str) {
  let env = new_env();
  ffi_libs::bind_libs(env);

  let (tx, rx) = channel();
  let mut watcher = watcher(tx, Duration::from_millis(500)).unwrap();
  watcher.watch(path, RecursiveMode::Recursive).unwrap();
  let watch_state = perm(
    WatchState { rx, watcher, path: path.to_string() }
  );
  event_loop::register_native_hook(env, 10, watch_state,
    |env, ws : Ptr<WatchState>| {
      match ws.rx.try_recv() {
        Ok(event) => {
          match event {
            DebouncedEvent::Write(_) => {
              hotload_file(env, &ws.path);
            }
            _ => (),
          }
        },
        Err(e) => match e {
          TryRecvError::Disconnected => {
            println!("watch error: {:?}", e);
          }
          TryRecvError::Empty => (),
        },
      }
    }
  );

  hotload_file(env, path);
  
  event_loop::start_loop(env);
}

