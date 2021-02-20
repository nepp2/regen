
use crate::{
  env, env::Env, interpret, parse, semantic,
  symbols::{Symbol, to_symbol}
};

use std::collections::{HashMap, HashSet};
use std::hash::{Hash};
use std::fmt::Write;
use crc32fast;
use parse::{Expr, ExprShape, ExprTag};

type SymbolGraph = HashMap<Symbol, HashSet<Symbol>>;

pub struct HotloadState {
  defs : HashMap<Symbol, Expr>,
  dependencies : SymbolGraph,
}

impl HotloadState {
  pub fn new() -> Self {
    HotloadState {
      defs: HashMap::new(),
      dependencies: HashMap::new(),
    }
  }
}

#[derive(Clone, Copy, PartialEq)]
enum DefState {
  Changed, Unchanged, Broken,
}

pub fn clear_state(env : Env, hs : &mut HotloadState) {
  hs.dependencies.clear();
  for (name, _) in hs.defs.drain() {
    env::unload_def(env, name);
  }
}

fn unload_def(hs : &mut HotloadState, env : Env, name : Symbol){
  hs.dependencies.remove(&name);
  hs.defs.remove(&name);
  env::unload_def(env, name);
}

fn load_def(
  hs : &mut HotloadState,
  env : Env,
  new_defs : &HashMap<Symbol, DefState>,
  value_expr : Expr,
  name : Symbol
) -> Result<(), ()>
{
  let TODO = (); // using catch unwind is very ugly. Replace with proper error handling.
  let global_references = std::panic::catch_unwind(|| {
    let info = semantic::get_semantic_info(value_expr);
    let global_references = info.global_set();
    // check dependencies are available
    for n in global_references.iter() {
      match new_defs.get(n) {
        Some(DefState::Broken) => {
          panic!("Def `{}` depends on broken def `{}`", name, n);
        }
        None => {
          if hs.defs.contains_key(n) {
            panic!("Def `{}` is missing dependency `{}`", name, n);
          }
        }
        _ => (),
      }
    }
    interpret::interpret_def_expr(name, value_expr, env, info);
    global_references
  }).map_err(|_| ())?;
  hs.defs.insert(name, value_expr);
  hs.dependencies.insert(name, global_references);
  Ok(())
}

fn is_external_dependency(hs : &HotloadState, env : Env, new_defs : &mut HashMap<Symbol, DefState>, name : Symbol) -> bool {
  if !new_defs.contains_key(&name) {
    if !hs.defs.contains_key(&name) {
      if env::get_entry(&env, name).is_some() {
        return true;
      }
    }
  }
  false
}

fn hotload_def(
  hs : &mut HotloadState,
  env : Env,
  name : Symbol,
  value_expr : Expr,
  new_defs : &mut HashMap<Symbol, DefState>,
)
{
  let mut new_def_state : DefState = (|| {
    // if already defined
    if let Some(def) = hs.defs.iter().find(|d| *d.0 == name) {
      // check if expression has changed
      let mut changed = def.1 != &value_expr;
      // check dependencies
      for n in hs.dependencies[&name].iter() {
        if !is_external_dependency(hs, env, new_defs, *n) {
          if new_defs.get(n) != Some(&DefState::Unchanged) {
            changed = true;
            break;
          }
        }
      }
      if changed {
        unload_def(hs, env, name);
        DefState::Changed
      }
      else {
        DefState::Unchanged
      }
    }
    else {
      DefState::Changed
    }
  }) ();
  if new_def_state == DefState::Changed {
    println!("def '{}' changed", name);
    let r = load_def(hs, env, new_defs, value_expr, name);
    if r.is_err() {
      new_def_state = DefState::Broken;
    }
  }
  if new_def_state == DefState::Unchanged {
    // Update nodes so that their text locations are correct
    hs.defs.insert(name, value_expr);
  }
  new_defs.insert(name, new_def_state);
}

pub fn interpret_module(module_name : &str, code : &str, env : Env) {
  let mut hs = HotloadState::new();
  hotload_changes(module_name, &code, env, &mut hs);
}

pub fn hotload_changes(module_name : &str, code : &str, env : Env, hs : &mut HotloadState) {
  // Parse file
  let exprs = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };

  // Find new and unchanged defs
  let mut symbol_buffer = "".to_string();
  let mut new_defs = HashMap::new();
  for e in exprs {
    match e.shape() {
      ExprShape::List(ExprTag::Def, &[name, _args, _defs, value_expr]) => {
        let name = name.as_symbol();
        hotload_def(hs, env, name, value_expr, &mut new_defs);
      }
      _ => {
        let mut i = 0;
        let checksum = {
          let mut hasher = crc32fast::Hasher::new();
          e.hash(&mut hasher);
          hasher.finalize()
        };
        loop {
          symbol_buffer.clear();
          write!(symbol_buffer, "__toplevel__{}_{}", checksum, i).unwrap();
          let name = to_symbol(env.st, &symbol_buffer);
          if !new_defs.contains_key(&name) {
            hotload_def(hs, env, name, e, &mut new_defs);
            break;
          }
          i += 1;
        }
      }
    }
  }

  // unload any defs that were deleted or broken
  let deletion_list : Vec<_> =
    hs.defs.keys().filter(|def| match new_defs.get(def) {
      None | Some(DefState::Broken) => true,
      _ => false,
    })
    .cloned().collect();
  for def in deletion_list {
    unload_def(hs, env, def)
  }
}
