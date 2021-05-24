
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};
use std::{collections::HashMap, path::{PathBuf}, sync::mpsc::{Receiver, TryRecvError, channel}};
use std::time::Duration;

use crate::ffi_libs;

use compiler::{
  new_env,
  env::Env,
  hotload,
  event_loop,
};
use std::fs;

fn print_hotload_message(path : &str) {
  println!();
  println!();
  println!("-----------------------------------");
  println!("Hotloading file '{}'", path);
  println!("-----------------------------------");
  println!();
}

fn hotload_file(env : Env, path : &str) {
  let code =
    fs::read_to_string(path)
    .expect("Something went wrong reading the file");
  hotload::hotload_module(env, path, &code);
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
  let mut ws = WatchState { rx, watcher, path: path.to_string() };
  hotload_file(env, path);

  let root = PathBuf::from(path).canonicalize().unwrap();

  let mut watched_files = HashMap::new();
  
  loop {
    // check for watch requests
    event_loop::process_watch_requests(env, |path| {
      let canonical = PathBuf::from(path.as_str()).canonicalize().unwrap();
      if watched_files.insert(canonical.clone(), path).is_none() {
        ws.watcher.watch(canonical, RecursiveMode::Recursive).unwrap();
      }
    });

    // check for watcher events
    match ws.rx.try_recv() {
      Ok(event) => {
        match event {
          DebouncedEvent::Write(path_buf) => {
            let path_buf = path_buf.canonicalize().unwrap();
            if path_buf == root {
              print_hotload_message(&ws.path);
              hotload_file(env, &ws.path);
            }
            else if let Some(watched_path) = watched_files.get(&path_buf) {
              print_hotload_message(watched_path.as_str());
              event_loop::handle_watch_event(env, *watched_path);
            }
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
    // handle timer
    event_loop::handle_next_timer(env, 10);
  }
}

