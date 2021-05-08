
use crate::{compile::{self, Function}, dependencies::{self, CellDependencies}, env::{self, CellCompile, CellIdentifier, CellUid, CellValue, DependencyType, Env, Namespace, ReactiveCell, RegenValue, UpdateLevel}, error::{Error, err, error}, event_loop::{self, ConstructorVariant}, ffi_libs::RegenString, interpret, parse::{self, Expr, ExprShape, ExprTag, SrcLocation}, perm_alloc::{Ptr, perm, perm_slice_from_vec}, symbols::{Symbol, to_symbol}, types::{self, TypeHandle}};

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
  Visible(Error, UpdateLevel),
  Silent,
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
  if env.cell_exprs.contains_key(&uid) {
    if !is_cell_active(env, uid) {
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
  value_expr : Expr,
  reactive : bool,
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
    match compile::compile_expr_to_function(&ctx, value_expr) {
      Ok(f) => perm(f),
      Err(e) => {
        return Err(HotloadError::Visible(e, UpdateLevel::Compile));
      }
    }
  };
  let expr_type = types::type_as_function(&function.t).unwrap().returns;
  let (flag, cc) = {
    if reactive {
      let inner =
        get_reactive_inner_type(env, expr_type)
        .ok_or_else(|| {
          let e = error(value_expr, "expected reactive constructor");
          HotloadError::Visible(e, UpdateLevel::Compile)
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

fn try_eval<L>(loc : L, f : Ptr<Function>, return_ptr : *mut ())
  -> Result<(), HotloadError>
    where L : Into<SrcLocation>
{
  std::panic::catch_unwind(|| {
    interpret::interpret_function(
      Ptr::to_ptr(f),
      &[],
      Some(return_ptr)
    );
  }).map_err(|_| {
    let e = error(loc, "regen eval panic!");
    HotloadError::Visible(e, UpdateLevel::Evaluate)
  })?;
  Ok(())
}

fn evaluate_cell(
  mut env : Env,
  uid : CellUid,
  value_expr : Expr,
) -> Result<ChangeType, HotloadError>
{
  env::unload_cell_value(env, uid);
  let cc = env.cell_compiles.get(&uid).unwrap();
  let cv = {
    if let Some(rc) = cc.reactive_constructor {
      try_eval(value_expr, cc.function, rc.ptr)?;
      register_reactive_cell(env, uid, value_expr, rc, cc.allocation)
        .map_err(|e| HotloadError::Visible(e, UpdateLevel::Evaluate))?
    }
    else {
      let v = cc.allocation;
      try_eval(value_expr, cc.function, v.ptr)?;
      CellValue{ t: v.t, ptr: v.ptr, initialised: true }
    }
  };
  env.cell_values.insert(uid, cv);
  Ok(ChangeType::Value)
}

fn register_reactive_cell(
  mut env : Env,
  uid : CellUid,
  value_expr : Expr,
  constructor_val : RegenValue,
  val : RegenValue,
) -> Result<CellValue, Error>
{
  let constructor = to_reactive_constructor(env, value_expr, constructor_val)?;
  match constructor.variant {
    ConstructorVariant::Watcher { file_path } => {
      if val.t != env.c.string_tag {
        return err(value_expr, "expected reactive string");
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
        return err(value_expr, "expected reactive i64");
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
  if let Some(poly) = types::type_as_poly(&t) {
    if poly.t == env.c.reactive_constructor_tag {
      return Some(poly.param);
    }
  }
  None
}

fn to_reactive_constructor(env : Env, value_expr : Expr, constructor_val : RegenValue)
  -> Result<Ptr<ReactiveConstructor>, Error>
{
  if let Some(t) = get_reactive_inner_type(env, constructor_val.t) {
    let c = unsafe { 
      *(constructor_val.ptr as *const Ptr<ReactiveConstructor>)
    };
    if c.value_type == t {
      return Ok(c);
    }
    return err(value_expr.loc(), format!("expected {}, found {}", c.value_type, t));
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

fn update_observer(mut env : Env, hs : &mut HotloadState, uid : CellUid) -> ChangeType {
  use ChangeType::*;
  // check if there is an observer
  let rc = {
    if let Some(rc) = env.reactive_cells.get(&uid) {
      *rc
    }
    else {
      return Unchanged;
    }
  };
  // only update if either this cell or the input cell changed in this pass
  let requires_update =
    get_change(env, hs, rc.input) != Unchanged
    || get_change(env, hs, uid) != Unchanged;
  if !requires_update {
    return Unchanged;
  }
  let state_val =
    if let Some(v) = get_cell_value(env, hs, uid, false) { v }
    else { return Unchanged; };
  let input_val =
    if let Some(v) = get_cell_value(env, hs, rc.input, true) { v }
    else { return Unchanged; };
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
      Value
    }
    else {
      Unchanged
    }
  }
  else {
    interpret::interpret_function(
      rc.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      None);
      get_cell_value_mut(&mut env, uid).initialised = true;
    Value
  }
}

fn hotload_cell_value(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  reactive_cell : bool,
)
{
  let r = try_hotload_cell_value(env, hs, uid, value_expr, deps, reactive_cell);
  if let Err(he) = r {
    if let HotloadError::Visible(e, ut) = he {
      env.broken_cells.insert(uid, ut);
      println!("{}", e.display());
    }
  }
}

fn get_input_update_level(env : Env, hs : &mut HotloadState, uid : CellUid)
  -> Option<UpdateLevel>
{
  let mut update = UpdateLevel::NoUpdate;
  if let Some(inputs) = env.graph.inputs(uid) {
    for (&input_uid, &input_type) in inputs {
      let input_change = get_change(env, hs, input_uid);
      let r = required_output_update(input_change, input_type)?;
      update = update.precedence(r);
    }
  }
  Some(update)
}

fn get_required_update(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
) -> UpdateLevel
{
  use UpdateLevel::*;
  // Update the value expression
  if update_value_expression(env, uid, value_expr, deps) {
    env.broken_cells.remove(&uid);
    return Compile;
  }
  // Check the cell's inputs
  let input_change_level = {
    let r = get_input_update_level(env, hs, uid);
    if let Some(update_level) = r {
      // Check whether the broken flag can be removed
      if let Some(&broken_level) = env.broken_cells.get(&uid) {
        if update_level as u32 >= broken_level as u32 {
          env.broken_cells.remove(&uid);
        }
      }
      update_level
    }
    else {
      Compile
    }
  };
  // Check if the cell hasn't been compiled
  if !env.cell_compiles.contains_key(&uid) {
    return Compile;
  }
  // Check if the cell has been evaluated
  if !env.cell_values.contains_key(&uid) {
    return input_change_level.precedence(UpdateLevel::Evaluate);
  }
  input_change_level
}

fn apply_update(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  reactive_cell : bool,
  update : UpdateLevel,
) -> Result<ChangeType, HotloadError>
{
  use UpdateLevel::*;
  match update {
    NoUpdate => Ok(ChangeType::Unchanged),
    Compile => {
      let mut change =
        compile_cell(env, hs, uid, value_expr, reactive_cell)?;
      change.accumulate(
        evaluate_cell(env, uid, value_expr)?);
      Ok(change)
    }
    Evaluate => {
      let mut change =
        evaluate_cell(env, uid, value_expr)?;
      change.accumulate(
        update_observer(env, hs, uid));
      Ok(change)
    },
    Observer => {
      let change = update_observer(env, hs, uid);
      Ok(change)
    },
  }
}

fn try_hotload_cell_value(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  deps : &CellDependencies,
  reactive_cell : bool,
) -> Result<(), HotloadError>
{
  // Check if this cell has already been visited
  if hs.visited_cells.contains(&uid) {
    let id = uid.id(env);
    match id {
      DefCell(_, _) => {
        if env.cell_exprs[&uid] != value_expr {
          let e = error(value_expr, format!("def {} defined twice!", id));
          return Err(HotloadError::Visible(e, UpdateLevel::NoUpdate));
        }
        else {
          return Ok(());
        }
      }
      ExprCell(_) => {
        return Ok(());
      }
      EmbedCell(_) => {
        return Ok(());
      }
    }
  }
  hs.visited_cells.insert(uid);
  // Update the value expression
  let update_level = get_required_update(env, hs, uid, value_expr, deps);
  // Check if the cell is broken
  if let Some(&broken_level) = env.broken_cells.get(&uid) {
    if (update_level as u32) <= (broken_level as u32) {
      return Err(HotloadError::Silent);
    }
  }
  let change = apply_update(env, hs, uid, value_expr, reactive_cell, update_level)?;
  hs.cell_changes.insert(uid, change);
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
    ExprShape::List(ExprTag::Cells, cell_exprs) => {
      hotload_cells(env, hs, namespace, cell_exprs);
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
  cell_exprs : &[Expr],
)
{
  for &e in cell_exprs {
    hotload_cell(env, hs, namespace, e)
  }
}

struct HotloadState {
  cell_changes : HashMap<CellUid, ChangeType>,
  visited_cells : HashSet<CellUid>,
}

impl HotloadState {
  fn new() -> Self {
    Self {
      cell_changes: HashMap::new(),
      visited_cells: HashSet::new(),
    }
  }
}

fn is_cell_active(env : Env, uid : CellUid) -> bool {
  env.active_cells.contains(&uid)
}

fn get_change(env : Env, hs : &HotloadState, uid : CellUid) -> ChangeType {
  if let Some(c) = hs.cell_changes.get(&uid).cloned() {
    return c;
  }
  // Accept defs that aren't part of the active module
  if env.cell_exprs.contains_key(&uid) {
    if !is_cell_active(env, uid) {
      return ChangeType::Unchanged;
    }
  }
  ChangeType::Deleted
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ChangeType {
  Unchanged = 0,
  Value = 1,
  Address = 2,
  Deleted = 3,
}

impl ChangeType {
  fn accumulate(&mut self, other : Self) {
    if (*self as i32) < other as i32 {
      *self = other;
    }
  }
}

fn required_output_update(input_change : ChangeType, input_type : DependencyType)
  -> Option<UpdateLevel>
{
  let v = match input_change {
    ChangeType::Unchanged => UpdateLevel::NoUpdate,
    ChangeType::Value => {
      match input_type {
        DependencyType::Observer => UpdateLevel::Observer,
        DependencyType::Value => UpdateLevel::Evaluate,
        DependencyType::Code => UpdateLevel::Compile,
      }
    }
    ChangeType::Address => UpdateLevel::Compile,
    ChangeType::Deleted => return None,
  };
  Some(v)
}

fn update_module(mut env : Env, mut hs : HotloadState, module_expr : Expr) {
  // Find new and unchanged cells
  hotload_cell(env, &mut hs, env::new_namespace(&[]), module_expr);

  // unload any cells that were deleted
  let mut cells = hs.visited_cells;
  std::mem::swap(&mut env.active_cells, &mut cells);
  for uid in cells.drain() {
    if !env.active_cells.contains(&uid) {
      env::unload_cell(env, uid);
    }
  }
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
  env.broken_cells.clear();
  update_module(env, hs, module_expr);
  env.root_expr = Some(module_expr);
}

/// Recalculate cell values in response to an updated cell
fn cells_updated(env : Env, cells : &[CellUid]) {
  let mut hs = HotloadState::new();
  for &uid in cells {
    hs.cell_changes.insert(uid, ChangeType::Value);
    hs.visited_cells.insert(uid);
  }
  update_module(env, hs, env.root_expr.unwrap());
}

/// Called by the event loop when a file watcher pulses
pub fn update_watcher_cells(env : Env, cells : &[CellUid]) {
  cells_updated(env, cells);
}

/// Called by the event loop when a timer pulses
pub fn update_timer_cell(mut env : Env, uid : CellUid, millisecond : i64) {
  let c = get_cell_value_mut(&mut env, uid);
  unsafe {
    *(c.ptr as *mut i64) = millisecond;
  }
  c.initialised = true;
  cells_updated(env, &[uid]);
}
