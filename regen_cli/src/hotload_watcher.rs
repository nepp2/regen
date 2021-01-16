
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use std::{path::PathBuf, sync::mpsc::{Receiver, TryRecvError, channel}};
use std::time::Duration;

use crate::bind_libs;

use regen_core::{
  new_env,
  env::{Env, get_event_loop},
  hotload,
  hotload::HotloadState,
  event_loop,
  event_loop::{EventLoop, TimerState},
  perm_alloc::perm,
  
};
use std::fs;
use std::path::Path;

fn hotload_file(env : Env, hs : &mut HotloadState, path : impl AsRef<Path>) {
  println!("Hotloading file '{}'", path.as_ref().display());
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  hotload::hotload_changes(&code, env, hs);
}

struct WatchState {
  rx : Receiver<DebouncedEvent>,
  watcher : RecommendedWatcher,
}

struct CompilerState {
  env : Env,
  hs : HotloadState,
  path : PathBuf,
}

pub fn watch_file(path : impl AsRef<Path>) {
  let env = new_env();
  bind_libs::bind_libs(env);

  let (tx, rx) = channel();
  let mut watcher = watcher(tx, Duration::from_millis(500)).unwrap();
  watcher.watch(path.as_ref(), RecursiveMode::Recursive).unwrap();
  let watch_state = WatchState { rx, watcher };

  let mut event_loop = get_event_loop(env);

  let timer = event_loop.create_timer(0, 10);
  let file_changes = event_loop.create_stream(timer, watch_state, |ws, _ : &TimerState| {
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
  });

  let mut hs = HotloadState::new();
  hotload_file(env, &mut hs, path.as_ref());
  let cs = CompilerState { env, hs, path: path.as_ref().to_path_buf() };

  event_loop.create_stream(file_changes, cs, |cs, _ : &WatchState| {
    hotload_file(cs.env, &mut cs.hs, &cs.path);
    true
  });
  
  event_loop::start_loop(event_loop);
}

