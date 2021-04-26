
use crate::{compile, env::{self, Cell, CellUid, CellValue, Env, Namespace, ReactiveObserver, RegenValue}, error::{Error, err, error}, event_loop::{self, ConstructorVariant}, interpret, parse::{self, Expr, ExprShape, ExprTag, SrcLocation}, perm_alloc::{Ptr, perm, perm_slice_from_vec}, symbols::{Symbol, to_symbol}, types};

use std::{alloc::{Layout, alloc}, collections::{HashMap, HashSet}};

use CellUid::*;
use event_loop::ReactiveConstructor;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum CellStatus {
  NewValue, UnchangedValue, Broken, AwaitingInput
}

pub struct CompileContext<'l> {
  pub env : Env,
  hs : &'l HotloadState,
}

impl <'l> CompileContext<'l> {

  fn new(
    env : Env,
    hs : &'l HotloadState,
  ) -> Self {
    CompileContext { env, hs }
  }

  pub fn cell_status(&self, uid : CellUid) -> CellStatus {
    if let Some(&s) = self.hs.visited_cells.get(&uid) {
      s
    }
    else if self.hs.active_cells.contains(&uid) {
      CellStatus::Broken
    }
    else {
      CellStatus::UnchangedValue
    }
  }

  pub fn check_uid(&self, uid : CellUid) -> bool {
    if env::get_cell_value(self.env, uid).is_some() {
      // Accept defs that appear before this one in the current pass
      if self.hs.visited_cells.contains_key(&uid) {
        return true;
      }
      // Accept defs that aren't part of the current pass
      if !self.hs.active_cells.contains(&uid) {
        return true;
      }
    }
    false
  }

  pub fn cell_value(&self, uid : CellUid) -> Option<CellValue> {
    env::get_cell_value(self.env, uid)
  }
}

fn get_dependency_status(
  hs : &HotloadState,
  dep_uid : CellUid
) -> CellStatus {
  use CellStatus::*;
  if let Some(&status) = hs.visited_cells.get(&dep_uid) {
    status 
  }
  else if hs.active_cells.contains(&dep_uid) {
    Broken
  }
  else {
    // assume external defs are always initialised
    UnchangedValue
  }
}

fn get_dependencies_status(env : Env, hs : &HotloadState, uid : CellUid) -> CellStatus {
  use CellStatus::*;
  if let Some(deps) = env.graph.dependencies(uid) {
    let mut changed = false;
    for &dep_uid in deps {
      match get_dependency_status(hs, dep_uid) {
        Broken | AwaitingInput => return AwaitingInput,
        NewValue => changed = true,
        UnchangedValue => (),
      }
    }
    if changed { NewValue } else { UnchangedValue }
  }
  else {
    NewValue
  }
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn load_cell_value(
  env : Env,
  hs : &HotloadState,
  uid : CellUid,
  value_expr : Expr,
  reactive_cell : bool,
) -> Result<(CellValue, HashSet<CellUid>), Error>
{
  let (v, deps) = eval_expr(env, hs, value_expr)?;
  let cv = {
    if reactive_cell {
      register_reactive_cell(env, uid, value_expr, v)?
    }
    else {
      CellValue{ t: v.t, ptr: v.ptr, initialised: true }
    }
  };
  Ok((cv, deps))
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn load_cell(
  mut env : Env,
  hs : &HotloadState,
  uid : CellUid,
  value_expr : Expr,
  reactive_cell : bool,
) -> CellStatus
{
  let r = load_cell_value(env, hs, uid, value_expr, reactive_cell);
  let status = match r {
    Ok((v, deps)) => {
      env.cells.insert(uid, Cell {
        value_expr,
        v
      });
      env.graph.set_cell_dependencies(uid, deps);
      if v.initialised { CellStatus::NewValue }
      else { CellStatus::AwaitingInput }

    }
    Err(e) => {
      println!("{}", e.display());
      CellStatus::Broken
    }
  };
  status
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
      let ob = ReactiveObserver { uid, input: *input, update_handler: poll_function };
      env.graph.reactive_observers.insert(uid, ob);
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
  hs : &HotloadState,
  expr : Expr,
) -> Result<(RegenValue, HashSet<CellUid>), Error>
{
  let ctx = CompileContext::new(env, hs);
  let (f, dependencies) = compile::compile_expr_to_function(&ctx, expr)?;
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
  Ok((v, dependencies))
}

fn unload_cell(env : Env, uid : CellUid){
  env::unload_cell(env, uid);
}

fn get_cell_status(
  env : Env,
  hs : &HotloadState,
  uid : CellUid,
  value_expr : Expr
) -> CellStatus
{
  use CellStatus::*;
  // if already defined
  if let Some(cell) = env.cells.get(&uid) {
    // check if expression has changed
    if cell.value_expr != value_expr {
      NewValue
    }
    // check if dependencies have changed
    else {
      get_dependencies_status(env, hs, uid)
    }
  }
  else {
    CellStatus::NewValue
  }
}

fn get_cell_mut (env : &mut Env, uid : CellUid) -> &mut Cell {
  env.cells.get_mut(&uid).unwrap()
}

fn update_observer(mut env : Env, ob : ReactiveObserver) -> CellStatus {
  let state_val = env::get_cell_value(env, ob.uid).unwrap();
  let input_val = env::get_cell_value(env, ob.input).unwrap();
  let returns_bool = {
    let f = unsafe { &*ob.update_handler };
    let return_type = types::type_as_function(&f.t).unwrap().returns;
    return_type == env.c.bool_tag
  };
  if returns_bool {
    let mut return_val = true;
    interpret::interpret_function(
      ob.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      Some((&mut return_val) as *mut bool as *mut ()));
    if return_val {
      get_cell_mut(&mut env, ob.uid).v.initialised = true;
      CellStatus::NewValue
    }
    else if state_val.initialised {
      CellStatus::UnchangedValue
    }
    else {
      CellStatus::AwaitingInput
    }
  }
  else {
    interpret::interpret_function(
      ob.update_handler,
      &[state_val.ptr as u64, input_val.ptr as u64],
      None);
      get_cell_mut(&mut env, ob.uid).v.initialised = true;
    CellStatus::NewValue
  }
}

fn should_update(observer : CellStatus, input : CellStatus) -> bool {
  use CellStatus::*;
  match (observer, input) {
    (Broken, _) => false,
    (_, NewValue) => true,
    (AwaitingInput, UnchangedValue) => true,
    (NewValue, UnchangedValue) => true,
    _ => false,
  }
}

fn poll_observer_input(
  env : Env,
  hs : &mut HotloadState,
  uid : CellUid
)
{
  if let Some(ob) = env.graph.reactive_observers.get(&uid) {
    let observer_status = get_dependency_status(hs, uid);
    let input_status = get_dependency_status(hs, ob.input);
    if should_update(observer_status, input_status) {
      let new_status = update_observer(env, *ob);
      hs.visited_cells.insert(ob.uid, new_status);
    }
  }
}

fn hotload_cell(
  mut env : Env,
  hs : &mut HotloadState,
  uid : CellUid,
  value_expr : Expr,
  reactive_cell : bool,
)
{
  if hs.visited_cells.contains_key(&uid) {
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
  use CellStatus::*;
  let status = get_cell_status(env, hs, uid, value_expr);
  let final_status = match status {
    NewValue => {
      unload_cell(env, uid);
      load_cell(env, hs, uid, value_expr, reactive_cell)
    }
    UnchangedValue => {
      // Update exprs so that their text locations are correct
      let cell = env.cells.get_mut(&uid).unwrap();
      cell.value_expr = value_expr;
      if cell.v.initialised { UnchangedValue } else { AwaitingInput }
    }
    AwaitingInput => {
      AwaitingInput
    }
    Broken => {
      Broken
    }
  };
  hs.visited_cells.insert(uid, final_status);
}

struct NestedCells {
  defs : Vec<Expr>,
  const_exprs : Vec<Expr>,
  embeds : Vec<Expr>,
}

fn get_nested_cells(expr : Expr) -> NestedCells {
  fn find_nested_cells(nested_cells : &mut NestedCells, expr : Expr) {
    use parse::ExprShape::*;
    match expr.shape() {
      List(ExprTag::Def, _) | List(ExprTag::Reactive, _) => {
        nested_cells.defs.push(expr);
        return;
      }
      List(ExprTag::ConstExpr, &[c]) => {
        nested_cells.const_exprs.push(c);
        return;
      }
      List(ExprTag::Embed, &[e]) => {
        nested_cells.embeds.push(e);
        return;
      }
      List(ExprTag::Quote, _) => {
        // expression literals should be ignored!
        return;
      }
      _ => (),
    }
    for &c in expr.children() {
      find_nested_cells(nested_cells, c);
    }
  }

  let mut nested_cells = NestedCells {
    defs: vec![],
    const_exprs: vec![],
    embeds: vec![]
  };
  find_nested_cells(&mut nested_cells, expr);
  nested_cells
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
    let cv = env::get_cell_value(env, uid).unwrap();
    if cv.t == env.c.expr_tag {
      let e = unsafe { *(cv.ptr as *const Expr) };
      let nested = get_nested_cells(e);
      hotload_cells(env, hs, namespace, &nested.defs);
      hotload_cells(env, hs, namespace, &nested.const_exprs);
      hotload_embedded(env, hs, namespace, &nested.embeds);
    }
    else {
      println!("expected expression of type 'expr', found {}", cv.t);
    }
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
  let nested = get_nested_cells(value_expr);
  let nested_namespace = create_nested_namespace(namespace, name);
  hotload_cells(env, hs, nested_namespace, &nested.defs);
  hotload_cells(env, hs, nested_namespace, &nested.const_exprs);
  hotload_embedded(env, hs, nested_namespace, &nested.embeds);
  // hotload the def initialiser
  let uid = CellUid::def(namespace, name);
  hotload_cell(env, hs, uid, value_expr, reactive_cell);
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
        let nested = get_nested_cells(e);
        hotload_cells(env, hs, namespace, &nested.defs);
        hotload_cells(env, hs, namespace, &nested.const_exprs);
        hotload_embedded(env, hs, namespace, &nested.embeds);
        // hotload the const expression initialiser
        let uid = CellUid::expr(e);
        hotload_cell(env, hs, uid, e, false);
      }
    }
  }
}

struct HotloadState {
  active_cells : HashSet<CellUid>,
  visited_cells : HashMap<CellUid, CellStatus>,
}

fn hotload_module(env : Env, module_name : Symbol, exprs : &[Expr], visited_cells : HashMap<CellUid, CellStatus>) {
  let mut active_cells = HashSet::new();
  for (id, v) in &env.cells {
    if v.value_expr.loc().module.name == module_name {
      active_cells.insert(*id);
    }
  }
  let mut hs = HotloadState {
    active_cells,
    visited_cells,
  };

  // Find new and unchanged cells
  hotload_cells(env, &mut hs, env::new_namespace(&[]), exprs);

  // unload any cells that were deleted or broken
  for &id in &hs.active_cells {
    match hs.visited_cells.get(&id) {
      None | Some(CellStatus::Broken) => {
        unload_cell(env, id)
      }
      _ => ()
    };
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
  hotload_module(env, module_name, &exprs, HashMap::new());
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
  hotload_module(env, module_name, &exprs, HashMap::new());
}

/// Recalculate cell values in response to an updated cell
fn cell_updated(env : Env, uid : CellUid) {
  let module_name = env.cells[&uid].value_expr.loc().module.name;
  let mut visited_cells = HashMap::new();
  visited_cells.insert(uid, CellStatus::NewValue);
  hotload_module(env, module_name, &env.live_exprs, visited_cells);
}

/// Called by the event loop when the value of a reactive cell has changed
pub fn update_timer_cell(mut env : Env, uid : CellUid, millisecond : i64) {
  let c = get_cell_mut(&mut env, uid);
  unsafe {
    *(c.v.ptr as *mut i64) = millisecond;
  }
  c.v.initialised = true;
  cell_updated(env, uid);
}
