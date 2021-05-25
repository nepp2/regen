/// This module is responsible for performing live updates to the running program,
/// taking dependencies into account. Updates happen in response to file changes,
/// and in response to a core event loop.

use crate::{codegen::{self, Function}, dependencies::{self, CellDependencies}, env::{self, BuildStatus, CellCompile, CellIdentifier, CellSrc, CellUid, CellValue, DependencyType, Env, Namespace, ReactiveCell, RegenValue}, error::{Error, err, error}, event_loop::{self, ConstructorVariant}, ffi_libs::RegenString, interpret, parse::{self, Expr, ExprShape, ExprTag}, regen_alloc::{Ptr, alloc, alloc_slice}, symbols::{Symbol, to_symbol}, types::{self, TypeHandle}};

use std::collections::{HashMap, HashSet};

use CellIdentifier::*;
use event_loop::ReactiveConstructor;

pub type DependencyValues = HashMap<CellUid, CellValue>;

pub struct CompileContext<'l> {
  pub env : Env,
  pub deps : &'l DependencyValues,
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

enum HotloadError {
  Visible(Error, BuildStatus),
  Silent,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
enum ChangeType {
  Unchanged = 0,
  Value = 1,
  Address = 2,
  Deleted = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum FlushLevel {
  Compile = 0,
  Evaluate = 1,
  Observer = 2,
  NoFlush = 3,
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
  if env.cell_src.contains_key(&uid) {
    if !is_cell_active(hs, uid) {
      return true;
    }
  }
  false
}

fn alloc_type(t : TypeHandle) -> *mut ()
{
  let layout = std::alloc::Layout::from_size_align(t.size_of as usize, 8).unwrap();
  unsafe { std::alloc::alloc(layout) as *mut () }
}

fn get_cell_allocation(prev_cc : Option<CellCompile>, t : TypeHandle)
  -> (*mut (), ChangeType)
{
  use ChangeType::*;
  if let Some(cc) = prev_cc {
    let ptr =
      if cc.allocation.t.size_of == t.size_of { cc.allocation.ptr }
      else { alloc_type(t) };
    let flag =
      if cc.allocation.t == t { Value }
      else { Address };
    (ptr, flag)
  }
  else {
    (alloc_type(t), Address)
  }
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn compile_cell(
  mut env : Env,
  hs : &HotloadState,
  uid : CellUid,
  src : CellSrc,
) -> Result<ChangeType, HotloadError>
{
  let prev_cc = env.cell_compiles.get(&uid).cloned();
  // get/check dependency values
  let mut dep_values = HashMap::new();
  for &dep_uid in env.graph.inputs(uid).unwrap().keys() {
    let r = get_cell_value(env, hs, dep_uid, true);
    if let Some(cv) = r {
      dep_values.insert(dep_uid, cv);
    }
    else {
      if is_cell_visible(env, hs, dep_uid) {
        // fail silently, as we are waiting for initialisation
        return Err(HotloadError::Silent);
      }
      else {
        // Proceed without the cell value.
        // This will cause a compile error with location information.
      }
    }
  }
  // compile the expression
  let ctx = CompileContext::new(env, &dep_values);
  let function = {
    match codegen::compile_expr_to_function(&ctx, src.value_expr) {
      Ok(f) => alloc(f),
      Err(e) => {
        return Err(HotloadError::Visible(e, BuildStatus::Empty));
      }
    }
  };
  let expr_type = types::type_as_function(function.t).unwrap().returns;
  let (flag, cc) = {
    if src.is_reactive {
      let inner =
        get_reactive_inner_type(env, expr_type)
        .ok_or_else(|| {
          let e = error(src.value_expr, "expected reactive constructor");
          HotloadError::Visible(e, BuildStatus::Empty)
        })?;
      let (ptr, flag) = get_cell_allocation(prev_cc, inner);
      let cc = CellCompile {
        reactive_constructor: Some(RegenValue { t: expr_type, ptr: alloc_type(expr_type) }),
        allocation: RegenValue { t: inner, ptr },
        function,
      };
      (flag, cc)
    }
    else {
      let (ptr, flag) = get_cell_allocation(prev_cc, expr_type);
      let cc = CellCompile {
        reactive_constructor: None,
        allocation: RegenValue { t: expr_type, ptr },
        function,
      };
      (flag, cc)
    }
  };
  env.cell_compiles.insert(uid, cc);
  Ok(flag)
}

fn try_eval(env : Env, uid : CellUid, f : Ptr<Function>, args : &[u64], return_ptr : Option<*mut ()>)
  -> Result<(), HotloadError>
{
  std::panic::catch_unwind(|| {
    interpret::interpret_function(
      Ptr::to_ptr(f),
      args,
      return_ptr
    );
  }).map_err(|_| {
    let e = error(uid.loc(env), "regen eval panic!");
    HotloadError::Visible(e, BuildStatus::Compiled)
  })?;
  Ok(())
}

fn evaluate_cell(
  mut env : Env,
  uid : CellUid,
) -> Result<ChangeType, HotloadError>
{
  env::unload_cell_value(env, uid);
  let cc = env.cell_compiles.get(&uid).unwrap();
  let cv = {
    if let Some(rc) = cc.reactive_constructor {
      try_eval(env, uid, cc.function, &[], Some(rc.ptr))?;
      register_reactive_cell(env, uid, rc, cc.allocation)
        .map_err(|e| HotloadError::Visible(e, BuildStatus::Compiled))?
    }
    else {
      let v = cc.allocation;
      try_eval(env, uid, cc.function, &[], Some(v.ptr))?;
      CellValue{ t: v.t, ptr: v.ptr, initialised: true }
    }
  };
  env.cell_values.insert(uid, cv);
  Ok(ChangeType::Value)
}

fn register_reactive_cell(
  mut env : Env,
  uid : CellUid,
  constructor_val : RegenValue,
  val : RegenValue,
) -> Result<CellValue, Error>
{
  let constructor = to_reactive_constructor(env, uid, constructor_val)?;
  match constructor.variant {
    ConstructorVariant::Watcher { file_path } => {
      if val.t != env.c.string_tag {
        return err(uid.loc(env), "expected reactive string");
      }
      event_loop::register_watcher(env.event_loop, uid, file_path);
      env.watchers.insert(uid, file_path);
      unsafe {
        *(val.ptr as *mut RegenString) = file_path;
      }
      let v = CellValue {
        t: val.t,
        ptr: val.ptr,
        initialised: true,
      };
      Ok(v)
    }
    ConstructorVariant::Timer { millisecond_interval } => {
      if val.t != env.c.i64_tag {
        return err(uid.loc(env), "expected reactive i64");
      }
      let id = event_loop::register_timer(env.event_loop, uid, millisecond_interval);
      let now : i64 = event_loop::current_millisecond(env.event_loop);
      env.timers.insert(uid, id);
      unsafe {
        *(val.ptr as *mut i64) = now;
      }
      let v = CellValue {
        t: val.t,
        ptr: val.ptr,
        initialised: false,
      };
      Ok(v)
    }
    ConstructorVariant::Poll { input, initial_value, poll_function } => {
      if let Some(iv) = initial_value {
        unsafe {
          std::ptr::copy_nonoverlapping(
            iv as *const u8, val.ptr as *mut u8, val.t.size_of as usize);
        }
      }
      let v = CellValue {
        t: val.t,
        ptr: val.ptr,
        initialised: initial_value.is_some(),
      };
      let input = input.uid(env);
      let ob = ReactiveCell { input, update_handler: poll_function };
      env.reactive_cells.insert(uid, ob);
      Ok(v)
    }
  }
}

fn get_reactive_inner_type(env : Env, t : TypeHandle) -> Option<TypeHandle> {
  if let Some(poly) = types::type_as_poly(t) {
    if poly.t == env.c.reactive_constructor_tag {
      return Some(poly.param);
    }
  }
  None
}

fn to_reactive_constructor(env : Env, uid : CellUid, constructor_val : RegenValue)
  -> Result<Ptr<ReactiveConstructor>, Error>
{
  if let Some(t) = get_reactive_inner_type(env, constructor_val.t) {
    let c = unsafe { 
      *(constructor_val.ptr as *const Ptr<ReactiveConstructor>)
    };
    if c.value_type == t {
      return Ok(c);
    }
    return err(uid.loc(env), format!("expected {}, found {}", c.value_type, t));
  }
  err(uid.loc(env), format!("expected reactive constructor, found {}", constructor_val.t))
}

/// returns true if expression changed
fn update_cell_source(
  mut env : Env,
  uid : CellUid,
  new_src : CellSrc,
  deps : &CellDependencies,
) -> bool
{
  if let Some(&prev_src) = env.cell_src.get(&uid) {
    if prev_src == new_src {
      // Always update the value expr, to keep valid source locations
      env.cell_src.insert(uid, new_src);
      return false;
    }
  }
  env.cell_src.insert(uid, new_src);
  // update dependencies
  let mut graph_deps = HashMap::new();
  for &dep_id in &deps.ids {
    let dt = match dep_id {
      CellIdentifier::DefCell(_, _) => DependencyType::Value,
      CellIdentifier::ExprCell(_) => DependencyType::Code,
      CellIdentifier::EmbedCell(_) => DependencyType::Code,
    };
    graph_deps.insert(dep_id.uid(env), dt);
  }
  if let Some(observe_id) = deps.observe_id {
    graph_deps.insert(observe_id.uid(env), DependencyType::Observer);
  }
  for &e in &deps.embeds {
    let dep_uid = CellIdentifier::EmbedCell(e).uid(env);
    graph_deps.insert(dep_uid, DependencyType::Code);
  }
  env.graph.set_cell_dependencies(uid, graph_deps);
  true
}

fn get_cell_value_mut (env : &mut Env, uid : CellUid) -> &mut CellValue {
  env.cell_values.get_mut(&uid).unwrap()
}

fn update_observer(mut env : Env, hs : &mut HotloadState, uid : CellUid, flush_level : FlushLevel)
  -> Result<ChangeType, HotloadError>
{
  use ChangeType::*;
  // check if there is an observer
  let rc = {
    if let Some(rc) = env.reactive_cells.get(&uid) {
      *rc
    }
    else {
      return Ok(Unchanged);
    }
  };
  // only update if either this cell or the input cell changed in this pass
  let requires_update =
    flush_level < FlushLevel::Observer
    || get_change(hs, rc.input) != Unchanged;
  if !requires_update {
    return Ok(Unchanged);
  }
  let state_val =
    if let Some(v) = get_cell_value(env, hs, uid, false) { v }
    else { return Ok(Unchanged); };
  let input_val =
    if let Some(v) = get_cell_value(env, hs, rc.input, true) { v }
    else { return Ok(Unchanged); };
  let returns_bool = {
    let f = rc.update_handler;
    let return_type = types::type_as_function(f.t).unwrap().returns;
    return_type == env.c.bool_tag
  };
  if returns_bool {
    let mut value_changed = true;
    try_eval(
      env, uid, rc.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      Some((&mut value_changed) as *mut bool as *mut ()))?;
    if value_changed {
      get_cell_value_mut(&mut env, uid).initialised = true;
      Ok(Value)
    }
    else {
      Ok(Unchanged)
    }
  }
  else {
    try_eval(
      env, uid, rc.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      None)?;
      get_cell_value_mut(&mut env, uid).initialised = true;
      Ok(Value)
  }
}

fn check_for_input_changes(env : Env, hs : &mut HotloadState, uid : CellUid)
  -> FlushLevel
{
  let mut aggregate_flush = FlushLevel::NoFlush;
  if let Some(inputs) = env.graph.inputs(uid) {
    for (&input_uid, &input_type) in inputs {
      let input_change = get_change(hs, input_uid);
      let input_flush = check_input(input_change, input_type);
      aggregate_flush = std::cmp::min(aggregate_flush, input_flush);
    }
  }
  aggregate_flush
}

fn check_input(input_change : ChangeType, input_type : DependencyType)
  -> FlushLevel
{
  let v = match input_change {
    ChangeType::Unchanged => FlushLevel::NoFlush,
    ChangeType::Value => {
      match input_type {
        DependencyType::Observer => FlushLevel::Observer,
        DependencyType::Value => FlushLevel::Evaluate,
        DependencyType::Code => FlushLevel::Compile,
      }
    }
    ChangeType::Address => FlushLevel::Compile,
    ChangeType::Deleted => FlushLevel::Compile,
  };
  v
}

fn apply_update(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  src : CellSrc,
  flush_level : FlushLevel,
) -> Result<ChangeType, HotloadError>
{
  use FlushLevel::*;
  let mut agg_change = ChangeType::Unchanged;
  if flush_level <= Compile {
    let c = compile_cell(env, hs, uid, src)?;
    agg_change = std::cmp::max(agg_change, c);
  }
  if flush_level <= Evaluate {
    let c = evaluate_cell(env, uid)?;
    agg_change = std::cmp::max(agg_change, c);
  }
  if flush_level <= Observer {
    let c = update_observer(env, hs, uid, flush_level)?;
    agg_change = std::cmp::max(agg_change, c);
  }
  Ok(agg_change)
}

fn visited_check(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  src : CellSrc,
) -> Result<bool, HotloadError>
{
  // Check if this cell has already been visited
  if hs.visited_cells.contains(&uid) {
    let id = uid.id(env);
    match id {
      DefCell(_, _) => {
        if env.cell_src[&uid] != src {
          let e = error(src.value_expr, format!("def {} defined twice!", id));
          return Err(HotloadError::Visible(e, BuildStatus::Empty));
        }
        else {
          return Ok(true);
        }
      }
      ExprCell(_) => {
        return Ok(true);
      }
      EmbedCell(_) => {
        return Ok(true);
      }
    }
  }
  Ok(false)
}

fn hotload_cell_value(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  is_reactive : bool,
)
{
  let src = CellSrc { value_expr, is_reactive };
  let r = try_hotload_cell_value(env, hs, uid, src, deps);
  if let Err(he) = r {
    if let HotloadError::Visible(e, build_status) = he {
      env.broken_cells.insert(uid);
      env.cell_build_status.insert(uid, build_status);
      println!("{}", e.display());
    }
  }
}

fn try_hotload_cell_value(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  src : CellSrc,
  deps : &CellDependencies,
) -> Result<(), HotloadError>
{
  if visited_check(env, hs, uid, src)? {
    return Ok(())
  }
  else {
    hs.visited_cells.insert(uid);
  }

  // Update the value expression
  let expr_flush = {
    if update_cell_source(env, uid, src, deps) {
      FlushLevel::Compile
    }
    else { FlushLevel::NoFlush }
  };

  hotload_input_changes(env, hs, uid, src, expr_flush)
}

fn try_hotload_from_existing_expr(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
) -> Result<(), HotloadError>
{
  let src = env.cell_src[&uid];
  if visited_check(env, hs, uid, src)? {
    return Ok(())
  }
  else {
    hs.visited_cells.insert(uid);
  }
  hotload_input_changes(env, hs, uid, src, FlushLevel::NoFlush)
}

fn hotload_from_existing_expr(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
)
{
  let r = try_hotload_from_existing_expr(env, hs, uid);
  if let Err(he) = r {
    if let HotloadError::Visible(e, build_status) = he {
      env.broken_cells.insert(uid);
      env.cell_build_status.insert(uid, build_status);
      println!("{}", e.display());
    }
  }
}

fn hotload_input_changes(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  src : CellSrc,
  forced_flush : FlushLevel,
) -> Result<(), HotloadError>
{
  // Check the cell's dependencies for changes
  let input_flush = check_for_input_changes(env, hs, uid);
  
  let change_flush = std::cmp::min(forced_flush, input_flush);

  // Don't update unless the build level can support the flush level
  let incompleteness_flush = {
    let status = env.cell_build_status.get(&uid).cloned().unwrap_or(BuildStatus::Empty);
    match status {
      BuildStatus::Empty => FlushLevel::Compile,
      BuildStatus::Compiled => FlushLevel::Evaluate,
      BuildStatus::Evaluated => FlushLevel::NoFlush,
    }
  };
  
  // Check if the cell already failed to update
  if env.broken_cells.contains(&uid) {
    // Only try updating it again if something actually changed
    if change_flush <= incompleteness_flush {
      env.broken_cells.remove(&uid);
    }
    else {
      return Ok(());
    }
  }
  
  let flush_level = std::cmp::min(change_flush, incompleteness_flush);

  if flush_level < FlushLevel::NoFlush {
    let change = apply_update(env, hs, uid, src, flush_level)?;
    env.cell_build_status.insert(uid, BuildStatus::Evaluated);
    hs.cell_changes.insert(uid, change);
  }
  Ok(())
}

pub fn create_nested_namespace(n : Namespace, name : Symbol) -> Namespace {
  let mut names = Vec::with_capacity(n.len() + 1);
  names.extend_from_slice(n.as_slice());
  names.push(name);
  alloc_slice(names)
}

pub fn append_namespace(a : Namespace, b : Namespace) -> Namespace {
  let mut names = Vec::with_capacity(a.len() + b.len());
  names.extend_from_slice(a.as_slice());
  names.extend_from_slice(b.as_slice());
  alloc_slice(names)
}

fn hotload_embedded(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  const_expr : Expr,
)
{
  let embed_uid = CellIdentifier::EmbedCell(const_expr).uid(env);
  if hs.visited_cells.contains(&embed_uid) {
    return;
  }
  let const_expr_id = CellIdentifier::expr(const_expr);
  hotload_cell(env, hs, namespace, const_expr);
  if let Some(cv) = get_cell_value(env, hs, const_expr_id.uid(env), true) {
    if cv.t == env.c.expr_tag {
      let e = unsafe { *(cv.ptr as *const Expr) };
      let mut deps = dependencies::get_cell_dependencies(env.st, e);
      hotload_nested_cells(env, hs, namespace, &deps);
      // make sure the embed depends on the const expr
      deps.ids.push(const_expr_id);
      // hotload the const expression initialiser
      hotload_cell_value(env, hs, embed_uid, e, &deps, false);
    }
    else {
      let e =
        error(const_expr,
          format!("expected expression of type 'expr', found {}", cv.t));
      println!("{}", e.display());
    }
  }
}

fn hotload_nested_cells(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  deps : &CellDependencies,
) {
  hotload_cells(env, hs, namespace, &deps.inner_cells);
  for &e in &deps.embeds {
    hotload_embedded(env, hs, namespace, e);
  }
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
  hotload_cell_value(env, hs, uid, value_expr, &deps, reactive_cell);
}

fn hotload_cell(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  cell_expr : Expr,
) {
  match cell_expr.shape() {
    ExprShape::List(ExprTag::Def, &[name, value_expr]) => {
      hotload_def(env, hs, namespace, name.as_symbol(), value_expr, false);
    }
    ExprShape::List(ExprTag::Reactive, &[name, value_expr]) => {
      hotload_def(env, hs, namespace, name.as_symbol(), value_expr, true);
    }
    ExprShape::List(ExprTag::Cells, cell_src) => {
      hotload_cells(env, hs, namespace, cell_src);
    }
    _ => {
      // hotload nested cells
      let deps = dependencies::get_cell_dependencies(env.st, cell_expr);
      hotload_nested_cells(env, hs, namespace, &deps);
      // hotload the const expression initialiser
      let uid = CellIdentifier::expr(cell_expr).uid(env);
      hotload_cell_value(env, hs, uid, cell_expr, &deps, false);
    }
  }
}

fn hotload_cells(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  cell_src : &[Expr],
)
{
  for &e in cell_src {
    hotload_cell(env, hs, namespace, e)
  }
}

struct HotloadState {
  cell_changes : HashMap<CellUid, ChangeType>,
  visited_cells : HashSet<CellUid>,
  active_cells : HashSet<CellUid>,
}

impl HotloadState {
  fn new() -> Self {
    Self {
      cell_changes: HashMap::new(),
      visited_cells: HashSet::new(),
      active_cells: HashSet::new(),
    }
  }
}

fn is_cell_active(hs : &HotloadState, uid : CellUid) -> bool {
  hs.active_cells.contains(&uid)
}

fn get_change(hs : &HotloadState, uid : CellUid) -> ChangeType {
  if hs.visited_cells.contains(&uid) {
    hs.cell_changes.get(&uid).cloned().unwrap_or(ChangeType::Unchanged)
  }
  else if is_cell_active(hs, uid) {
    ChangeType::Deleted
  }
  else {
    ChangeType::Unchanged
  }
}

fn root_diff_update(mut env : Env, mut hs : HotloadState, module_expr : Expr) {
  // Clearing all update lists is expensive, particularly if it's triggered
  // regularly by a timer. Would be more efficient to clear specific lists when
  // they are affected. But how?
  // A list is affected when a cell in it changes, or a new cell depends on something
  // in it. This reverses the dependency graph.
  let TODO = ();
  env.update_lists.clear();
  env.broken_cells.clear();
  
  std::mem::swap(&mut env.active_cells, &mut hs.active_cells);

  // Find new and unchanged cells
  hotload_cell(env, &mut hs, env::new_namespace(&[]), module_expr);

  std::mem::swap(&mut env.active_cells, &mut hs.visited_cells);

  // unload any cells that were deleted
  for uid in hs.active_cells.drain() {
    if !env.active_cells.contains(&uid) {
      env::unload_cell(env, uid);
    }
  }
}

pub fn preload_module(env : Env, module_name : &str, code : &str) {
  let module_name = to_symbol(env.st, module_name);
  // Parse file
  let module_expr = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };
  let mut hs = HotloadState::new();
  hotload_cell(env, &mut hs, env::new_namespace(&[]), module_expr);
}

pub fn hotload_module(mut env : Env, module_name : &str, code : &str) {
  let module_name = to_symbol(env.st, module_name);
  // Parse file
  let module_expr = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };
  let hs = HotloadState::new();
  // this was to force all errors to repeat when a file is edited
  root_diff_update(env, hs, module_expr);
  env.root_expr = Some(module_expr);
}

/// Called by the event loop when a file watcher pulses
pub fn update_watcher_cells(env : Env, cells : &[CellUid]) {
  let mut hs = HotloadState::new();
  for &uid in cells {
    hs.cell_changes.insert(uid, ChangeType::Value);
    hs.visited_cells.insert(uid);
  }
  root_diff_update(env, hs, env.root_expr.unwrap());
}

/// Called by the event loop when a timer pulses
pub fn update_timer_cell(mut env : Env, uid : CellUid, millisecond : i64) {
  let c = get_cell_value_mut(&mut env, uid);
  unsafe {
    *(c.ptr as *mut i64) = millisecond;
  }
  c.initialised = true;
  // Update affected cells
  let mut hs = HotloadState::new();
  hs.cell_changes.insert(uid, ChangeType::Value);
  hs.visited_cells.insert(uid);
  // Try and get an update list
  if !env.update_lists.contains_key(&uid) {
    if let Some(l) = topological_update_list(env, &[uid]) {
      env.update_lists.insert(uid, l);
    }
  }
  match env.update_lists.get(&uid) {
    Some(update_list) => {
      for &cell in update_list {
        hs.active_cells.insert(cell);
      }
      for &cell in update_list.iter() {
        hotload_from_existing_expr(env, &mut hs, cell);
      }
    }
    None => {
      root_diff_update(env, hs, env.root_expr.unwrap());
    }
  }
}

#[derive(Copy, Clone)]
struct VertFlag {
  complete : bool,
  active : bool,
}

fn topological_update_list(env : Env, roots : &[CellUid])
  -> Option<Vec<CellUid>>
{
  fn visit(
    ordering : &mut Vec<CellUid>,
    flags : &mut HashMap<CellUid, VertFlag>,
    env : Env,
    uid : CellUid
  ) -> Option<()>
  {
    if let CellIdentifier::EmbedCell(_) = uid.id(env) {
      // Can't use a baked-in update list if updateding an embed cell
      return None;
    }
    let flag = flags.entry(uid).or_insert(VertFlag{ active: false, complete: false});
    if flag.complete { return Some(()) }
    if flag.active { 
      panic!("cyclic dependency detected at {}!", uid.id(env));
    };
    flag.active = true;
    if let Some(o) = env.graph.outputs(uid) {
      for &output_uid in o.keys() {
        visit(ordering, flags, env, output_uid)?;
      }
    }
    flags.insert(uid, VertFlag{ active: false, complete: true });
    ordering.push(uid);
    Some(())
  }

  let mut ordering = vec![];
  let mut flags = HashMap::new();
  for &uid in roots {
    visit(&mut ordering, &mut flags, env, uid)?;
  }
  ordering.reverse();
  Some(ordering)
}
