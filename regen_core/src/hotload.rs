
use crate::{compile, dependencies::{self, CellDependencies}, env::{self, CellCompile, CellIdentifier, CellUid, CellValue, DependencyType, Env, Namespace, ReactiveCell, RegenValue}, error::{Error, err, error}, event_loop::{self, ConstructorVariant}, interpret, parse::{self, Expr, ExprShape, ExprTag}, perm_alloc::{Ptr, perm, perm_slice_from_vec}, symbols::{Symbol, to_symbol}, types::{self, TypeHandle}};

use std::{alloc::{Layout, alloc}, collections::{HashMap, HashSet}};

use CellIdentifier::*;
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

  pub fn cell_value(&self, id : CellIdentifier) -> Option<&CellValue> {
    self.deps.get(&id.uid(self.env))
  }
}

fn get_cell_value(
  env : Env,
  hs : &HotloadState,
  uid : CellUid,
  require_initialised : bool,
) -> Option<CellValue>
{
  if let Some(&cv) = env.cell_values.get(&uid) {
    if !require_initialised || cv.initialised {
      if is_cell_visible(env, hs, uid) {
        return Some(cv);
      }
    }
  }
  None
}

fn is_cell_visible(
  env : Env,
  hs : &HotloadState,
  uid : CellUid,
) -> bool
{
  // Accept defs that appear before this one in the current pass
  if hs.visited_cells.contains(&uid) {
    return true;
  }
  // Accept defs that aren't part of the active module
  if let Some(e) = env.cell_exprs.get(&uid) {
    if e.loc().module.name != hs.active_module {
      return true;
    }
  }
  false
}

enum LinkingStatus {
  ReuseAllocation,
  NewAllocation,
}

fn allocate_cell(env : Env, uid : CellUid, t : TypeHandle)
  -> (*mut (), LinkingStatus)
{
  if let Some(cc) = env.cell_compiles.get(&uid) {
    if cc.allocation.t.size_of == t.size_of {
      return (cc.allocation.ptr, LinkingStatus::ReuseAllocation);
    }
  }
  let layout = std::alloc::Layout::from_size_align(t.size_of as usize, 8).unwrap();
  let ptr = unsafe { std::alloc::alloc(layout) as *mut () };
  (ptr, LinkingStatus::NewAllocation)
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn compile_cell(
  mut env : Env,
  hs : &HotloadState,
  uid : CellUid,
  value_expr : Expr,
) -> Result<LinkingStatus, Option<Error>>
{
  // get/check dependency values
  let mut dep_values = HashMap::new();
  for &dep_uid in env.graph.dependencies(uid).unwrap().keys() {
    let v = get_cell_value(env, hs, dep_uid, true);
    if let Some(v) = v {
      dep_values.insert(dep_uid, v);
    }
    else {
      if !is_cell_visible(env, hs, dep_uid) {
        let e =
          error(value_expr,
            format!("{} has dependency {}, but it was not found",
              uid.id(env), dep_uid.id(env)));
        return Err(Some(e));
      }
      return Err(None);
    }
  }
  // compile the expression
  let ctx = CompileContext::new(env, &dep_values);
  let function = {
    let f = compile::compile_expr_to_function(&ctx, value_expr)?;
    perm(f)
  };
  let expr_type = types::type_as_function(&function.t).unwrap().returns;
  let (ptr, link_status) = allocate_cell(env, uid, expr_type);
  env.cell_compiles.insert(uid, CellCompile {
    allocation: RegenValue { t: expr_type, ptr },
    function,
  });
  return Ok(link_status)
}

fn evaluate_cell(
  mut env : Env,
  uid : CellUid,
  value_expr : Expr,
  reactive_cell : bool,
) -> Result<(), Option<Error>>
{
  env::unload_cell_value(env, uid);
  let cc = env.cell_compiles.get(&uid).unwrap();
  std::panic::catch_unwind(|| {
    interpret::interpret_function(
      Ptr::to_ptr(cc.function),
      &[],
      Some(cc.allocation.ptr)
    );
  }).map_err(|_| {
    error(value_expr, "regen eval panic!")
  })?;
  let v = cc.allocation;
  let cv = {
    if reactive_cell {
      register_reactive_cell(env, uid, value_expr, v)?
    }
    else {
      CellValue{ t: v.t, ptr: v.ptr, initialised: true }
    }
  };
  env.cell_values.insert(uid, cv);
  Ok(())
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
      let v = CellValue {
        t: env.c.i64_tag,
        ptr: alloc_val(now),
        initialised: false,
      };
      Ok(v)
    }
    ConstructorVariant::Poll { input, initial_value, poll_function } => {
      let t = constructor.value_type;
      let v = CellValue {
        t,
        ptr: alloc_bytes(t.size_of as usize, initial_value),
        initialised: initial_value.is_some(),
      };
      let input = input.uid(env);
      let ob = ReactiveCell { input, update_handler: poll_function };
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

/// returns true if expression changed
fn update_value_expression(
  mut env : Env,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
) -> bool
{
  if let Some(&prev_expr) = env.cell_exprs.get(&uid) {
    if prev_expr == value_expr {
      // Always update the value expr, to keep valid source locations
      env.cell_exprs.insert(uid, value_expr);
      return false;
    }
  }
  env.cell_exprs.insert(uid, value_expr);
  // update dependencies
  let mut graph_deps = HashMap::new();
  if let Some(observe_id) = deps.observe_ref {
    graph_deps.insert(observe_id.uid(env), DependencyType::Reactive);
  }
  for &dep_id in &deps.def_refs {
    graph_deps.insert(dep_id.uid(env), DependencyType::Value);
  }
  for &e in &deps.const_exprs {
    let dep_uid = CellIdentifier::expr(e).uid(env);
    graph_deps.insert(dep_uid, DependencyType::Code);
  }
  env.graph.set_cell_dependencies(uid, graph_deps);
  true
}

fn get_cell_value_mut (env : &mut Env, uid : CellUid) -> &mut CellValue {
  env.cell_values.get_mut(&uid).unwrap()
}

fn update_observer(mut env : Env, hs : &mut HotloadState, uid : CellUid, rc : ReactiveCell) -> bool {
  if hs.change_flags.get(&rc.input).is_none() {
    return false;
  }
  let state_val =
    if let Some(v) = get_cell_value(env, hs, uid, false) { v }
    else { return false; };
  let input_val =
    if let Some(v) = get_cell_value(env, hs, rc.input, true) { v }
    else { return false; };
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

fn hotload_cell(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  reactive_cell : bool,
)
{
  let r = try_hotload_cell(env, hs, uid, value_expr, deps, reactive_cell);
  if let Err(Some(e)) = r {
    println!("{}", e.display());
  }
}

fn try_hotload_cell(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  reactive_cell : bool,
) -> Result<(), Option<Error>>
{
  if hs.root_cell == Some(uid) {
    // This cell has already been updated
    return Ok(());
  }
  // Check if this cell has already been visited
  if hs.visited_cells.contains(&uid) {
    let id = uid.id(env);
    match id {
      DefCell(_, _) => {
        let e = error(value_expr, format!("def {} defined twice!", id));
        return Err(Some(e));
      }
      ExprCell(_) => {
        return Ok(());
      }
    }
  }
  hs.visited_cells.insert(uid);
  // Update the value expression
  use DependencyType::*;
  let mut update_compile = false;
  let mut update_value = false;
  if update_value_expression(env, uid, value_expr, deps) {
    update_compile = true;
  }
  // Check if this cell has been flagged for change
  if !update_compile {
    if let Some(dt) = hs.change_flags.get(&uid) {
      match dt {
        Code => {
          update_compile = true;
        }
        Value => {
          update_value = true;
        }
        Reactive => {
          // reactive changes only trigger cell value
          // updates when the cell hasn't been evaluated yet
          if get_cell_value(env, hs, uid, false).is_none() {
            update_value = true;
          }
        }
      }
    }
  }
  if let Some(observe_id) = deps.observe_ref {
    // value changes shouldn't affect stream cells
    // what about container cells??
    let TODO = ();
    // don't bother compiling if the reactive input hasn't changed
    let observe_uid = observe_id.uid(env);
    if !hs.change_flags.contains_key(&observe_uid) {
      return Ok(());
    }
  }
  // update the cell
  if update_compile {
    let link_status = compile_cell(env, hs, uid, value_expr)?;
    evaluate_cell(env, uid, value_expr, reactive_cell)?;
    mark_outputs(env, hs, uid, link_status);
  }
  else if update_value {
    let link_status = if !env.cell_compiles.contains_key(&uid) {
      compile_cell(env, hs, uid, value_expr)?
    }
    else {
      LinkingStatus::ReuseAllocation
    };
    evaluate_cell(env, uid, value_expr, reactive_cell)?;
    mark_outputs(env, hs, uid, link_status);
  }
  Ok(())
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
    let uid = CellIdentifier::expr(e).uid(env);
    let TODO = (); // handle this better
    let cv = get_cell_value(env, hs, uid, true).unwrap();
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
  let uid = CellIdentifier::def(namespace, name).uid(env);
  hotload_cell(env, hs, uid, value_expr, &deps, reactive_cell);
  // update observer
  if let Some(rc) = env.reactive_cells.get(&uid) {
    if update_observer(env, hs, uid, *rc) {
      mark_outputs(env, hs, uid, LinkingStatus::ReuseAllocation);
    }
  }
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
        let uid = CellIdentifier::expr(e).uid(env);
        hotload_cell(env, hs, uid, e, &deps, false);
      }
    }
  }
}

struct HotloadState {
  active_module : Symbol,
  change_flags : HashMap<CellUid, DependencyType>,
  visited_cells : HashSet<CellUid>,
  root_cell : Option<CellUid>,
}

fn dependency_precedence(a : DependencyType, b : DependencyType) -> DependencyType {
  use DependencyType::*;
  match (a, b) {
    (Code, _) | (_, Code) => Code,
    (Value, _) | (_, Value) => Value,
    (Reactive, Reactive) => Reactive,
  }
}

fn mark_outputs(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  link_status : LinkingStatus,
) {
  if let Some(outputs) = env.graph.outputs(uid) {
    for (&output_uid, &dt) in outputs {
      let new_dt = match link_status {
        LinkingStatus::NewAllocation => DependencyType::Code,
        LinkingStatus::ReuseAllocation => {
          if let Some(&prev_dt) = hs.change_flags.get(&output_uid) {
            dependency_precedence(dt, prev_dt)
          }
          else { dt }
        }
      };
      hs.change_flags.insert(output_uid, new_dt);
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
    change_flags: HashMap::new(),
    visited_cells: HashSet::new(),
    root_cell: None,
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
    change_flags: HashMap::new(),
    visited_cells: HashSet::new(),
    root_cell: None,
  };
  hotload_module(env, &mut hs, &exprs);
}

/// Recalculate cell values in response to an updated cell
fn cell_updated(env : Env, uid : CellUid) {
  let module_name = env.cell_exprs[&uid].loc().module.name;
  let mut hs = HotloadState {
    active_module: module_name,
    change_flags: HashMap::new(),
    visited_cells: HashSet::new(),
    root_cell: Some(uid),
  };
  hs.change_flags.insert(uid, DependencyType::Value);
  hs.visited_cells.insert(uid);
  mark_outputs(env, &mut hs, uid, LinkingStatus::ReuseAllocation);
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
