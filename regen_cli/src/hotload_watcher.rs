
use notify::{Watcher, RecursiveMode, watcher, DebouncedEvent};
use std::sync::mpsc::{channel, TryRecvError};
use std::time::Duration;
use std::thread;

use crate::bind_libs;

use regen_core::{
  new_env,
  env::Env,
  hotload,
  hotload::HotloadState,
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

pub fn watch_file(path : impl AsRef<Path>) {
  let env = new_env();
  bind_libs::bind_libs(env);

  let mut hs = HotloadState::new();
  hotload_file(env, &mut hs, path.as_ref());

  // Create a channel to receive the events.
  let (tx, rx) = channel();
  let mut watcher = watcher(tx, Duration::from_millis(500)).unwrap();
  watcher.watch(path.as_ref(), RecursiveMode::Recursive).unwrap();
  loop {
    // Read watch events, to trigger hotloading
    match rx.try_recv() {
      Ok(event) => {
        match event {
          DebouncedEvent::Write(_) => {
            hotload_file(env, &mut hs, path.as_ref());
          }
          _ => {}
        }
      },
      Err(e) => match e {
        TryRecvError::Disconnected =>
          println!("watch error: {:?}", e),
        TryRecvError::Empty => (),
      },
    }
    thread::sleep(Duration::from_millis(10));
  }
}
