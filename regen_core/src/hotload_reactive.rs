use crate::env::{CellUid, Env, CellGraph};
use std::collections::HashMap;

#[derive(Copy, Clone)]
struct CellFlag {
  complete : bool,
  active : bool,
}

impl CellFlag {
  fn new() -> Self {
    CellFlag{ complete: false, active: false }
  }
}

/// Returns a valid topological ordering if the graph is a DAG. Returns an error if the graph contains cycles.
fn valid_topological_ordering(g : &CellGraph, roots : impl Iterator<Item=CellUid>) -> Result<Vec<CellUid>, ()> {
  fn visit(
    ordering : &mut Vec<CellUid>,
    flags : &mut HashMap<CellUid, CellFlag>,
    g : &CellGraph,
    uid : CellUid
  ) -> Result<(), ()> {
    let flag = flags.entry(uid).or_insert(CellFlag::new());
    if flag.complete { return Ok(()) };
    if flag.active { return Err(()) };
    flag.active = true;
    if let Some(obs) = g.observers(uid) {
      for &observer_uid in obs {
        visit(ordering, flags, g, observer_uid)?;
      }
    }
    let flag = flags.get_mut(&uid).unwrap();
    flag.active = false;
    flag.complete = true;
    ordering.push(uid);
    Ok(())
  }

  let mut ordering = vec![];
  let mut flags = HashMap::new();
  for uid in roots {
    visit(&mut ordering, &mut flags, g, uid)?;
  }
  Ok(ordering)
}

/// Called by the event loop when the value of a reactive cell has changed
pub fn update_reactive_cell(uid : CellUid, env : Env) {

  // get a topologically ordered list of all affected cells
  let ordering = {
    let roots = env.graph.observers(uid).unwrap().iter().cloned();
    valid_topological_ordering(&env.graph, roots).unwrap()
  };
  // recalculate their values in order
  for uid in ordering.iter().rev() {
    // is the cell a def or a const_expr? does it matter?
    // is the cell reactive? (no clue)
    // does the cell depend on something that's broken? (no clue)
    println!("update cell {}", uid);
  }

  // handling "embed" is complicated
  // may have to introduce defs
  // may have to delete defs
  // initially ignore?
  let TODO = ();
}