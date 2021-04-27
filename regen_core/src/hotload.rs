
use crate::{compile, dependencies::{self, CellDependencies}, env::{self, CellUid, CellValue, Env, Namespace, ReactiveCell, RegenValue}, error::{Error, err, error}, event_loop::{self, ConstructorVariant}, interpret, parse::{self, Expr, ExprShape, ExprTag, SrcLocation}, perm_alloc::{Ptr, perm, perm_slice_from_vec}, symbols::{Symbol, to_symbol}, types};

use std::{alloc::{Layout, alloc}, collections::{HashMap, HashSet}};

use CellUid::*;
use event_loop::ReactiveConstructor;

pub type DependencyValues = HashMap<CellUid, CellValue>;

pub struct CompileContext<'l> {
  pub env : Env,
  deps : &'l DependencyValues,
}

impl <'l> CompileContext<'l> {
  fn new(
    env : Env,
    deps : &'l DependencyValues,
  ) -> Self {
    CompileContext { env, deps }
  }

  pub fn cell_value(&self, uid : CellUid) -> Option<&CellValue> {
    self.deps.get(&uid)
  }
}

fn get_dependency_value(env : Env, hs : &HotloadState, uid : CellUid) -> Option<CellValue> {
  if let Some(&cv) = env.cell_values.get(&uid) {
    if cv.initialised {
      // Accept defs that appear before this one in the current pass
      if hs.visited_cells.contains(&uid) {
        return Some(cv);
      }
      // Accept defs that aren't part of the active module
      if env.cell_exprs[&uid].loc().module.name != hs.active_module {
        return Some(cv);
      }
    }
  }
  None
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn load_cell_value(
  env : Env,
  uid : CellUid,
  value_expr : Expr,
  deps : &DependencyValues,
  reactive_cell : bool,
) -> Result<CellValue, Error>
{
  let v = eval_expr(env, value_expr, deps)?;
  let cv = {
    if reactive_cell {
      register_reactive_cell(env, uid, value_expr, v)?
    }
    else {
      CellValue{ t: v.t, ptr: v.ptr, initialised: true }
    }
  };
  Ok(cv)
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn update_cell(
  mut env : Env,
  hs : &HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  reactive_cell : bool,
) -> bool
{
  env::unload_cell(env, uid);
  // update expr
  env.cell_exprs.insert(uid, value_expr);
  // update dependencies
  let mut graph_deps = HashSet::new();
  let mut all_deps = vec![];
  for &e in &deps.const_exprs {
    let dep_uid = CellUid::ExprCell(e);
    graph_deps.insert(dep_uid);
    all_deps.push(dep_uid);
  }
  for &dep_uid in &deps.def_refs {
    graph_deps.insert(dep_uid);
    all_deps.push(dep_uid);
  }
  for &dep_uid in &deps.observer_refs {
    all_deps.push(dep_uid);
  }
  env.graph.set_cell_dependencies(uid, graph_deps);
  // get dependency values
  let mut dep_values = HashMap::new();
  for dep_uid in all_deps {
    let v = get_dependency_value(env, hs, dep_uid);
    if let Some(v) = v {
      dep_values.insert(dep_uid, v);
    }
    else {
      println!("dependency {} not found at ({})", dep_uid, value_expr.loc());
      return false;
    }
  }
  // load cell value
  let r = load_cell_value(env, uid, value_expr, &dep_values, reactive_cell);
  match r {
    Ok(v) => {
      env.cell_values.insert(uid, v);
      v.initialised
    }
    Err(e) => {
      println!("{}", e.display());
      false
    }
  }
}

fn register_reactive_cell(
  mut env : Env,
  uid : CellUid,
  value_expr : Expr,
  v : RegenValue,
) -> Result<CellValue, Error>
{
  let constructor = to_reactive_constructor(env, value_expr, v)?;
  match constructor.variant {
    ConstructorVariant::Timer { millisecond_interval } => {
      let id = event_loop::register_cell_timer(env.event_loop, uid, millisecond_interval);
      let now : i64 = event_loop::current_millisecond(env.event_loop);
      env.timers.insert(uid, id);
      let v = CellValue { t: env.c.i64_tag, ptr: alloc_val(now), initialised: false };
      Ok(v)
    }
    ConstructorVariant::Poll { input, initial_value, poll_function } => {
      let t = constructor.value_type;
      let v = CellValue {
        t,
        ptr: alloc_bytes(t.size_of as usize, initial_value),
        initialised: initial_value.is_some()
      };
      let ob = ReactiveCell { input: *input, update_handler: poll_function };
      env.reactive_cells.insert(uid, ob);
      Ok(v)
    }
  }
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

fn to_reactive_constructor(env : Env, value_expr : Expr, constructor_val : RegenValue)
  -> Result<Ptr<ReactiveConstructor>, Error>
{
  if let Some(poly) = types::type_as_poly(&constructor_val.t) {
    if poly.t == env.c.reactive_constructor_tag {
      let c = unsafe { 
        *(constructor_val.ptr as *const Ptr<ReactiveConstructor>)
      };
      if c.value_type == poly.param {
        return Ok(c);
      }
    }
  }
  if constructor_val.t == env.c.reactive_constructor_tag {
    let c = unsafe { 
      *(constructor_val.ptr as *const Ptr<ReactiveConstructor>)
    };
    return Ok(c);
  }
  err(value_expr.loc(), format!("expected reactive constructor, found {}", constructor_val.t))
}

fn eval_expr(
  env : Env,
  expr : Expr,
  deps : &DependencyValues,
) -> Result<RegenValue, Error>
{
  let ctx = CompileContext::new(env, deps);
  let f = compile::compile_expr_to_function(&ctx, expr)?;
  let expr_type = types::type_as_function(&f.t).unwrap().returns;
  // TODO: it's wasteful to allocate for types that are 64bits wide or smaller
  let ptr = {
    let layout = std::alloc::Layout::from_size_align(expr_type.size_of as usize, 8).unwrap();
    unsafe { std::alloc::alloc(layout) as *mut () }
  };
  std::panic::catch_unwind(|| {
    interpret::interpret_function(&f, &[], Some(ptr));
  }).map_err(|_|
    error(SrcLocation::zero(expr.loc().module), "regen eval panic!")
  )?;
  let v = RegenValue { t: expr_type, ptr };
  Ok(v)
}

fn does_cell_require_update(
  env : Env,
  hs : &HotloadState,
  uid : CellUid,
  value_expr : Expr
) -> bool
{
  if let Some(&prev_expr) = env.cell_exprs.get(&uid) {
    if prev_expr != value_expr {
      return true;
    }
  }
  else {
    return true;
  }
  if hs.stale_cells.get(&uid) == Some(&StaleMarker::Value) {
    return true;
  }
  false
}

fn get_cell_value_mut (env : &mut Env, uid : CellUid) -> &mut CellValue {
  env.cell_values.get_mut(&uid).unwrap()
}

fn update_observer(mut env : Env, hs : &mut HotloadState, uid : CellUid, rc : ReactiveCell) -> bool {
  let state_val = env.cell_values.get(&uid).unwrap();
  let input_val = {
    if let Some(v) = get_dependency_value(env, hs, rc.input) {
      v
    }
    else {
      return false;
    }
  };
  let returns_bool = {
    let f = unsafe { &*rc.update_handler };
    let return_type = types::type_as_function(&f.t).unwrap().returns;
    return_type == env.c.bool_tag
  };
  if returns_bool {
    let mut value_changed = true;
    interpret::interpret_function(
      rc.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      Some((&mut value_changed) as *mut bool as *mut ()));
    if value_changed {
      get_cell_value_mut(&mut env, uid).initialised = true;
      true
    }
    else {
      false
    }
  }
  else {
    interpret::interpret_function(
      rc.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      None);
      get_cell_value_mut(&mut env, uid).initialised = true;
    true
  }
}

fn poll_observer_input(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid
)
{
  if let Some(rc) = env.reactive_cells.get(&uid) {
    if let Some(cv) = env.cell_values.get(&uid) {
      if hs.stale_cells.contains_key(&uid) || !cv.initialised {
        if update_observer(env, hs, uid, *rc) {
          mark_outputs(env, hs, uid);
        }
      }
    }
  }
}

fn hotload_cell(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  reactive_cell : bool,
)
{
  if hs.visited_cells.contains(&uid) {
    match uid {
      DefCell(_, _) => {
        println!("def {} defined twice!", uid);
        return;
      }
      ExprCell(_) => {
        return;
      }
    }
  }
  // check whether the def needs to be loaded/reloaded
  let requires_update = does_cell_require_update(env, hs, uid, value_expr);
  if requires_update {
    update_cell(env, hs, uid, value_expr, deps, reactive_cell);
    mark_outputs(env, hs, uid);
  }
  else {
    // Update exprs so that their text locations are correct
    env.cell_exprs.insert(uid, value_expr);
  }
  hs.visited_cells.insert(uid);
}

pub fn create_nested_namespace(n : Namespace, name : Symbol) -> Namespace {
  let mut names = Vec::with_capacity(n.len() + 1);
  names.extend_from_slice(n.as_slice());
  names.push(name);
  perm_slice_from_vec(names)
}

pub fn append_namespace(a : Namespace, b : Namespace) -> Namespace {
  let mut names = Vec::with_capacity(a.len() + b.len());
  names.extend_from_slice(a.as_slice());
  names.extend_from_slice(b.as_slice());
  perm_slice_from_vec(names)
}

fn hotload_embedded(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  embeds : &[Expr],
)
{
  hotload_cells(env, hs, namespace, embeds);
  for &e in embeds {
    let uid = CellUid::expr(e);
    let TODO = (); // handle this better
    let cv = env.cell_values.get(&uid).unwrap();
    if cv.t == env.c.expr_tag {
      let e = unsafe { *(cv.ptr as *const Expr) };
      let deps = dependencies::get_cell_dependencies(env.st, e);
      hotload_nested_cells(env, hs, namespace, &deps);
    }
    else {
      println!("expected expression of type 'expr', found {}", cv.t);
    }
  }
}

fn hotload_nested_cells(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  deps : &CellDependencies,
) {
  hotload_cells(env, hs, namespace, &deps.defs);
  hotload_cells(env, hs, namespace, &deps.const_exprs);
  hotload_embedded(env, hs, namespace, &deps.embeds);
}

fn hotload_def(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  name : Symbol,
  value_expr : Expr,
  reactive_cell : bool,
)
{
  // hotload nested cells
  let deps = dependencies::get_cell_dependencies(env.st, value_expr);
  let nested_namespace = create_nested_namespace(namespace, name);
  hotload_nested_cells(env, hs, nested_namespace, &deps);
  // hotload the def initialiser
  let uid = CellUid::def(namespace, name);
  hotload_cell(env, hs, uid, value_expr, &deps, reactive_cell);
  // update observer
  poll_observer_input(env, hs, uid);
}

fn hotload_cells(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  cell_exprs : &[Expr],
)
{
  for &e in cell_exprs {
    match e.shape() {
      ExprShape::List(ExprTag::Def, &[name, value_expr]) => {
        hotload_def(env, hs, namespace, name.as_symbol(), value_expr, false);
      }
      ExprShape::List(ExprTag::Reactive, &[name, value_expr]) => {
        hotload_def(env, hs, namespace, name.as_symbol(), value_expr, true);
      }
      _ => {
        // hotload nested cells
        let deps = dependencies::get_cell_dependencies(env.st, e);
        hotload_nested_cells(env, hs, namespace, &deps);
        // hotload the const expression initialiser
        let uid = CellUid::expr(e);
        hotload_cell(env, hs, uid, e, &deps, false);
      }
    }
  }
}

#[derive(Clone, Copy, PartialEq)]
enum StaleMarker {
  Reactive,
  Value,
}

struct HotloadState {
  active_module : Symbol,
  stale_cells : HashMap<CellUid, StaleMarker>,
  visited_cells : HashSet<CellUid>,
}

fn mark_outputs(env : Env, hs : &mut HotloadState, uid : CellUid) {
  if let Some(outputs) = env.graph.outputs(uid) {
    for &output_uid in outputs {
      hs.stale_cells.insert(output_uid, StaleMarker::Value);
    }
  }
  if let Some(outputs) = env.reactive_outputs.get(&uid) {
    for &output_uid in outputs {
      if hs.stale_cells.get(&output_uid) != Some(&StaleMarker::Value) {
        hs.stale_cells.insert(output_uid, StaleMarker::Reactive);
      }
    }
  }
}

fn hotload_module(env : Env, hs : &mut HotloadState, exprs : &[Expr]) {
  // Find new and unchanged cells
  hotload_cells(env, hs, env::new_namespace(&[]), exprs);

  // unload any cells that were deleted
  for (id, e) in &env.cell_exprs {
    if e.loc().module.name == hs.active_module {
      if !hs.visited_cells.contains(id) {
        env::unload_cell(env, *id);
      }
    }
  }
}

pub fn hotload_live_module(mut env : Env, module_name : &str, code : &str) {
  let module_name = to_symbol(env.st, module_name);
  // Parse file
  let exprs = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };
  let mut hs = HotloadState {
    active_module: module_name,
    stale_cells: HashMap::new(),
    visited_cells: HashSet::new(),
  };
  hotload_module(env, &mut hs, &exprs);
  env.live_exprs = exprs;
}

pub fn interpret_module(env : Env, module_name : &str, code : &str) {
  let module_name = to_symbol(env.st, module_name);
  // Parse file
  let exprs = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };
  let mut hs = HotloadState {
    active_module: module_name,
    stale_cells: HashMap::new(),
    visited_cells: HashSet::new(),
  };
  hotload_module(env, &mut hs, &exprs);
}

/// Recalculate cell values in response to an updated cell
fn cell_updated(env : Env, uid : CellUid) {
  let module_name = env.cell_exprs[&uid].loc().module.name;
  let mut hs = HotloadState {
    active_module: module_name,
    stale_cells: HashMap::new(),
    visited_cells: HashSet::new(),
  };
  mark_outputs(env, &mut hs, uid);
  hotload_module(env, &mut hs, &env.live_exprs);
}

/// Called by the event loop when the value of a reactive cell has changed
pub fn update_timer_cell(mut env : Env, uid : CellUid, millisecond : i64) {
  let c = get_cell_value_mut(&mut env, uid);
  unsafe {
    *(c.ptr as *mut i64) = millisecond;
  }
  c.initialised = true;
  cell_updated(env, uid);
}
