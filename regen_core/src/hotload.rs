
use crate::{compile, env::{self, CellId, CellUid, CellValue, Env, Namespace, ReactiveObserver, RegenValue}, error::{Error, error, error_raw}, event_loop::{self, ConstructorVariant}, interpret, parse::{self, Expr, ExprShape, ExprTag, SrcLocation}, perm_alloc::{Ptr, perm, perm_slice, perm_slice_from_vec}, symbols::{Symbol, to_symbol}, types};

use std::{alloc::{Layout, alloc}, collections::{HashMap, HashSet}};

use CellId::*;
use event_loop::ReactiveConstructor;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum CellStatus {
  NewValue, UnchangedValue, Broken, AwaitingInput
}

pub struct CellResolver<'l> {
  env : Env,
  namespace : Namespace,
  hs : &'l HotloadState,
}

impl <'l> CellResolver<'l> {

  fn new(
    env : Env,
    namespace : Namespace,
    hs : &'l HotloadState,
  ) -> Self {
    CellResolver { env, namespace, hs }
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

  pub fn resolve_id(&self, id : CellId) -> Option<CellUid> {
    self.resolve_partial_uid(CellUid { id, namespace: perm_slice(&[])})
  }

  pub fn resolve_partial_uid(&self, partial_uid : CellUid) -> Option<CellUid> {
    // Look for the id in the current module
    let mut namespace_prefix = self.namespace;
    loop {
      let namespace = {
        if partial_uid.namespace.len() == 0 {
          namespace_prefix
        }
        else {
          let mut names = vec![];
          names.extend_from_slice(namespace_prefix.as_slice());
          names.extend_from_slice(partial_uid.namespace.as_slice());
          perm_slice_from_vec(names)
        }
      };
      let uid = CellUid { id: partial_uid.id, namespace };
      if let Some(cv) = env::get_cell_value(self.env, uid) {
        // Accept defs that appear before this one in the current pass
        if self.hs.visited_cells.contains_key(&uid) {
          return Some(uid);
        }
        // Accept defs that aren't part of the current pass
        if !self.hs.active_cells.contains(&uid) {
          return Some(uid);
        }
      }
      if namespace_prefix.len() == 0 {
        break;
      }
      namespace_prefix = namespace_prefix.slice_range(0..namespace_prefix.len()-1);
    }
    None
  }

  pub fn cell_value(&self, uid : CellUid) -> Option<CellValue> {
    env::get_cell_value(self.env, uid)
  }

  fn expr_to_partial_uid(&self, mut names : Vec<Symbol>, e : Expr) -> Option<CellUid> {
    use ExprShape::*;
    match e.shape() {
      Sym(name) => {
        Some(CellUid::def(name, perm_slice_from_vec(names)))
      }
      List(ExprTag::Namespace, &[name, tail]) => {
        names.push(name.as_symbol());
        self.expr_to_partial_uid(names, tail)
      },
      _ => {
        None
      }
    }
  }

  pub fn resolve_name(&self, e : Expr) -> Option<CellUid> {
    let partial_uid = self.expr_to_partial_uid(vec![], e)?;
    self.resolve_partial_uid(partial_uid)
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
fn load_cell(
  resolver : &CellResolver,
  uid : CellUid,
  full_expr : Expr,
  value_expr : Expr,
  reactive_cell : bool,
) -> Result<CellStatus, Error>
{
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let (v, deps) = std::panic::catch_unwind(|| {
    eval_expr(resolver, value_expr)
  }).map_err(|_|
    error_raw(SrcLocation::zero(value_expr.loc().module), "regen eval panic!")
  )?;
  if reactive_cell {
    initialise_reactive_cell(resolver.env, uid, full_expr, value_expr, v, deps)
  }
  else {
    initialise_normal_cell(resolver.env, uid, full_expr, value_expr, v, deps);
    Ok(CellStatus::NewValue)
  }
}

fn initialise_normal_cell(
  mut env : Env,
  uid : CellUid,
  full_expr : Expr,
  value_expr : Expr,
  v : RegenValue,
  deps : HashSet<CellUid>,
)
{
  let cv = CellValue {
    full_expr,
    value_expr,
    t: v.t,
    ptr: v.ptr,
    has_value: true,
  };
  env.cells.insert(uid, cv);
  env.graph.set_cell_dependencies(uid, deps);
}

fn initialise_reactive_cell(
  mut env : Env,
  uid : CellUid,
  full_expr : Expr,
  value_expr : Expr,
  v : RegenValue,
  deps : HashSet<CellUid>,
) -> Result<CellStatus, Error>
{
  let constructor = to_reactive_constructor(env, value_expr, v)?;
  let has_value;
  let container_val = match constructor.variant {
    ConstructorVariant::Timer { millisecond_interval } => {
      let id = event_loop::register_cell_timer(env.event_loop, uid, millisecond_interval);
      let now : i64 = event_loop::current_millisecond(env.event_loop);
      env.timers.insert(uid, id);
      has_value = false;
      RegenValue { t: env.c.i64_tag, ptr: alloc_val(now) }
    }
    ConstructorVariant::Poll { input, initial_value, poll_function } => {
      let t = constructor.value_type;
      has_value = initial_value.is_some();
      let state = RegenValue {
        t,
        ptr: alloc_bytes(t.size_of as usize, initial_value),
      };
      let ob = ReactiveObserver { uid, input: *input, update_handler: poll_function };
      env.graph.reactive_observers.insert(uid, ob);
      state
    }
  };
  let cv = CellValue {
    full_expr,
    value_expr,
    t: container_val.t,
    ptr: container_val.ptr,
    has_value,
  };
  env.cells.insert(uid, cv);
  env.graph.set_cell_dependencies(uid, deps);
  let status = 
    if has_value { CellStatus::NewValue }
    else { CellStatus::AwaitingInput };
  Ok(status)
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
  error(value_expr.loc(), format!("expected reactive constructor, found {}", constructor_val.t))
}

fn eval_expr(resolver : &CellResolver, expr : Expr) -> (RegenValue, HashSet<CellUid>) {
  let (f, dependencies) = compile::compile_expr_to_function(resolver.env, &resolver, expr);
  let expr_type = types::type_as_function(&f.t).unwrap().returns;
  // TODO: it's wasteful to allocate for types that are 64bits wide or smaller
  let ptr = {
    let layout = std::alloc::Layout::from_size_align(expr_type.size_of as usize, 8).unwrap();
    unsafe { std::alloc::alloc(layout) as *mut () }
  };
  interpret::interpret_function(&f, &[], Some(ptr));
  let v = RegenValue { t: expr_type, ptr };
  (v, dependencies)
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
  // if already defined
  if let Some(cell) = env.cells.get(&uid) {
    // check if the cell has been flagged for an update
    if hs.forced_updates.contains(&uid) {
      CellStatus::NewValue
    }
    // check if expression has changed
    else if cell.value_expr != value_expr {
      CellStatus::NewValue
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

fn update_observer(mut env : Env, ob : ReactiveObserver) -> CellStatus {
  let state_val = env.cells[&ob.uid];
  let input_val = env.cells[&ob.input];
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
      let state_val = env.cells.get_mut(&ob.uid).unwrap();
      state_val.has_value = true;
      CellStatus::NewValue
    }
    else if state_val.has_value {
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
      let state_val = env.cells.get_mut(&ob.uid).unwrap();
      state_val.has_value = true;
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
  namespace : Namespace,
  uid : CellUid,
  full_expr : Expr,
  value_expr : Expr,
  reactive_cell : bool,
)
{
  if hs.visited_cells.contains_key(&uid) {
    match uid.id {
      DefCell(_) => {
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
      let resolver = CellResolver::new(env, namespace, hs);
      let r = load_cell(&resolver, uid, full_expr, value_expr, reactive_cell);
      match r {
        Ok(s) => s,
        Err(e) => {
          println!("{}", e.display());
          Broken
        }
      }
    }
    UnchangedValue => {
      // Update exprs so that their text locations are correct
      let cell = env.cells.get_mut(&uid).unwrap();
      cell.full_expr = full_expr;
      cell.value_expr = value_expr;
      if cell.has_value { UnchangedValue } else { AwaitingInput }
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
    let uid = CellUid::expr(e, namespace);
    let cell = env::get_cell_value(env, uid).unwrap();
    if cell.t == env.c.expr_tag {
      let e = unsafe { *(cell.ptr as *const Expr) };
      let nested = get_nested_cells(e);
      hotload_cells(env, hs, namespace, &nested.defs);
      hotload_cells(env, hs, namespace, &nested.const_exprs);
      hotload_embedded(env, hs, namespace, &nested.embeds);
    }
    else {
      println!("expected expression of type 'expr', found {}", cell.t);
    }
  }
}

fn hotload_def(
  env : Env,
  hs : &mut HotloadState,
  namespace : Namespace,
  name : Symbol,
  full_expr : Expr,
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
  let uid = CellUid::def(name, namespace);
  hotload_cell(env, hs, nested_namespace, uid, full_expr, value_expr, reactive_cell);
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
        hotload_def(env, hs, namespace, name.as_symbol(), e, value_expr, false);
      }
      ExprShape::List(ExprTag::Reactive, &[name, value_expr]) => {
        hotload_def(env, hs, namespace, name.as_symbol(), e, value_expr, true);
      }
      _ => {
        // hotload nested cells
        let nested = get_nested_cells(e);
        if nested.defs.len() > 0 {
          println!("error: can't define defs inside a constant expression ({})", nested.defs[0].loc());
        }
        if nested.embeds.len() > 0 {
          println!("error: can't embed exprs inside a constant expression ({})", nested.embeds[0].loc());
        }
        hotload_cells(env, hs, namespace, &nested.const_exprs);
        // hotload the const expression initialiser
        let uid = CellUid::expr(e, namespace);
        hotload_cell(env, hs, namespace, uid, e, e, false);
      }
    }
  }
}

struct HotloadState {
  active_cells : HashSet<CellUid>,
  visited_cells : HashMap<CellUid, CellStatus>,
  forced_updates : HashSet<CellUid>,
}

fn hotload_module(env : Env, module_name : Symbol, exprs : &[Expr], forced_updates : HashSet<CellUid>) {
  let mut active_cells = HashSet::new();
  for (id, v) in &env.cells {
    if v.full_expr.loc().module.name == module_name {
      active_cells.insert(*id);
    }
  }
  let mut hs = HotloadState {
    active_cells,
    visited_cells: HashMap::new(),
    forced_updates,
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
  hotload_module(env, module_name, &exprs, HashSet::new());
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
  hotload_module(env, module_name, &exprs, HashSet::new());
}

/// Recalculate cell values in response to an updated cell
fn cell_updated(env : Env, uid : CellUid) {
  let forced_updates = env.graph.outputs(uid).cloned().unwrap_or(HashSet::new());
  let module_name = env.cells[&uid].full_expr.loc().module.name;
  hotload_module(env, module_name, &env.live_exprs, forced_updates);
}

/// Called by the event loop when the value of a reactive cell has changed
pub fn update_timer_cell(mut env : Env, uid : CellUid, millisecond : i64) {
  let cv = env.cells.get_mut(&uid).unwrap();
  unsafe {
    *(cv.ptr as *mut i64) = millisecond;
  }
  cv.has_value = true;
  cell_updated(env, uid);
}
