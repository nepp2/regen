
use crate::{
  compile,
  env::{self, Env},
  error::{Error, error, error_raw},
  interpret,
  parse::{self, CodeModule, Expr, ExprShape, ExprTag, SrcLocation},
  perm_alloc::Ptr,
  semantic::{self, ReferenceInfo},
  symbols::Symbol,
  types::{self, TypeHandle}
};

use std::collections::{HashMap, HashSet};
use std::hash::{Hash};
use std::fmt;

#[derive(Clone, Copy)]
pub struct ConstExprValue {
  pub e : Expr,
  pub t : TypeHandle,
  pub ptr : *const (),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CellId { DefCell(Symbol), ConstCell(Expr) }

use CellId::*;

pub struct CellGraph {
  cells : HashMap<CellId, ConstExprValue>,
  dependencies : HashMap<CellId, HashSet<CellId>>,
}

impl CellGraph {
  pub fn new() -> Self {
    CellGraph {
      cells: HashMap::new(),
      dependencies: HashMap::new(),
    }
  }

  pub fn get_const_value(&self, expr : Expr) -> Option<ConstExprValue> {
    self.cells.get(&ConstCell(expr)).cloned()
  }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum CellStatus {
  New, Changed, Unchanged, Broken,
}

pub fn clear_state(env : Env, cg : &mut CellGraph) {
  cg.dependencies.clear();
  for (id, _) in cg.cells.drain() {
    if let DefCell(name) = id {
      env::unload_def(env, name);
    }
  }
}

fn unload_cell(cg : &mut CellGraph, env : Env, id : CellId){
  cg.dependencies.remove(&id);
  cg.cells.remove(&id);
  if let DefCell(name) = id {
    env::unload_def(env, name);
  }
}

fn check_dependencies(
  cg : &mut CellGraph,
  new_cells : &HashMap<CellId, CellStatus>,
  const_expr : Expr,
  info : &ReferenceInfo,
) -> Result<(), Error>
{
  let id = ConstCell(const_expr);
  // check dependencies are available
  for dep_id in &info.dependencies {
    match new_cells.get(dep_id) {
      Some(CellStatus::Broken) => {
        return error(const_expr.loc(),
          format!("{} depends on broken dependency {}", id, dep_id));
      }
      None => {
        if cg.cells.contains_key(&dep_id) {
          return error(const_expr.loc(),
            format!("{} is missing dependency {}", id, dep_id));
        }
      }
      _ => (),
    }
  }
  Ok(())
}

fn load_expr(
  cg : &mut CellGraph,
  env : Env,
  new_cells : &HashMap<CellId, CellStatus>,
  const_expr : Expr,
) -> Result<(ConstExprValue, ReferenceInfo), Error>
{
  let info = semantic::get_semantic_info(const_expr);
  check_dependencies(cg, new_cells, const_expr, &info)?;
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let v = std::panic::catch_unwind(|| {
    eval_expr(cg, env, const_expr, &info)
  }).map_err(|_| error_raw(SrcLocation::zero(), "regen eval panic!"))?;
  Ok((v, info))
}

fn eval_expr(cg : &CellGraph, env : Env, e : Expr, info : &ReferenceInfo) -> ConstExprValue {
  let f = compile::compile_expr_to_function(env, info, cg, e);
  let expr_type = types::type_as_function(&f.t).unwrap().returns;
  // TODO: it's wasteful to allocate for types that are 64bits wide or smaller
  let ptr = {
    let layout = std::alloc::Layout::from_size_align(expr_type.size_of as usize, 8).unwrap();
    unsafe { std::alloc::alloc(layout) as *mut () }
  };
  interpret::interpret_function(&f, &[], Some(ptr));
  ConstExprValue { e, t: expr_type, ptr }
}

fn load_def(
  cg : &mut CellGraph,
  env : Env,
  new_cells : &mut HashMap<CellId, CellStatus>,
  name : Symbol,
  value_expr : Expr,
) -> Result<(), Error>
{
  env::set_active_definition(env, Some(name));
  let (v, info) = load_expr(cg, env, new_cells, value_expr)?;
  env::set_active_definition(env, None);
  let id = DefCell(name);
  cg.cells.insert(id, v);
  cg.dependencies.insert(id, info.dependencies);
  let module = value_expr.loc().module;
  env::insert_entry(env, module, name, v.t, v.ptr);
  Ok(())
}

fn is_external_dependency(env : Env, current_module : Ptr<CodeModule>, id : CellId) -> bool {
  if let DefCell(name) = id {
    if let Some(entry) = env::get_entry(&env, name) {
      if entry.module.name != current_module.name {
        return true;
      }
    }
  }
  false
}

fn get_dependencies_status(
  cg : &CellGraph,
  env : Env,
  id : CellId,
  current_module : Ptr<CodeModule>,
  new_cells : &HashMap<CellId, CellStatus>,
) -> CellStatus
{
  if let Some(deps) = cg.dependencies.get(&id) {
    let mut changed = false;
    for dep_id in deps {
      if !is_external_dependency(env, current_module, *dep_id) {
        let status = new_cells.get(dep_id);
        if status == Some(&CellStatus::Broken) {
          return CellStatus::Broken
        }
        if status != Some(&CellStatus::Unchanged) {
          changed = true;
        }
      }
    }
    if changed { CellStatus::Changed } else { CellStatus::Unchanged }
  }
  else {
    CellStatus::New
  }
}

fn get_def_status(
  cg : &CellGraph,
  env : Env,
  name : Symbol,
  value_expr : Expr,
  new_cells : &HashMap<CellId, CellStatus>,
) -> CellStatus
{
  // if already defined
  if let Some(cell) = cg.cells.get(&DefCell(name)) {
    // check if expression has changed
    if cell.e != value_expr {
      CellStatus::Changed
    }
    else {
      get_dependencies_status(cg, env, DefCell(name), value_expr.loc().module, new_cells)
    }
  }
  else {
    CellStatus::New
  }
}

fn hotload_nested_const_exprs(
  cg : &mut CellGraph,
  env : Env,
  expr : Expr,
  new_cells : &mut HashMap<CellId, CellStatus>,
) {
  let const_exprs = semantic::get_ordered_const_exprs(expr);
  for e in const_exprs {
    hotload_expr(cg, env, e, new_cells);
  }
}

fn hotload_def(
  cg : &mut CellGraph,
  env : Env,
  name : Symbol,
  value_expr : Expr,
  new_cells : &mut HashMap<CellId, CellStatus>,
)
{
  let id = DefCell(name);
  if new_cells.contains_key(&id) {
    println!("def {} defined twice!", name);
  }
  // make sure any nested const expressions have been loaded
  hotload_nested_const_exprs(cg, env, value_expr, new_cells);
  // check whether the def needs to be updated
  use CellStatus::*;
  let mut new_cell_state = get_def_status(cg, env, name, value_expr, new_cells);
  if new_cell_state == Changed {
    unload_cell(cg, env, id);
  }
  if let New | Changed = new_cell_state {
    let r = load_def(cg, env, new_cells, name, value_expr);
    if r.is_err() {
      new_cell_state = Broken;
    }
  }
  if new_cell_state == Unchanged {
    // Update nodes so that their text locations are correct
    cg.cells.get_mut(&id).unwrap().e = value_expr;
  }
  new_cells.insert(id, new_cell_state);
}

fn hotload_expr(
  cg : &mut CellGraph,
  env : Env,
  expr : Expr,
  new_cells : &mut HashMap<CellId, CellStatus>,
)
{
  let id = ConstCell(expr);
  if new_cells.contains_key(&id) {
    return;
  }
  // make sure any nested const expressions have been loaded
  hotload_nested_const_exprs(cg, env, expr, new_cells);
  // check whether this expr needs to be loaded/reloaded
  use CellStatus::*;
  let mut new_cell_state =
    get_dependencies_status(cg, env, id, expr.loc().module, new_cells);
  if new_cell_state == Changed {
    unload_cell(cg, env, id);
  }
  if let New | Changed = new_cell_state {
    // load the main expr
    if let Ok((v, info)) = load_expr(cg, env, new_cells, expr) {
      cg.cells.insert(id, v);
      cg.dependencies.insert(id, info.dependencies);
    }
    else {
      new_cell_state = Broken;
    }
  }
  if new_cell_state == Unchanged {
    // Update nodes so that their text locations are correct
    cg.cells.get_mut(&id).unwrap().e = expr;
  }
  new_cells.insert(id, new_cell_state);
}

pub fn interpret_module(module_name : &str, code : &str, env : Env) {
  let mut cg = CellGraph::new();
  hotload_changes(module_name, &code, env, &mut cg);
}

pub fn hotload_changes(module_name : &str, code : &str, env : Env, cg : &mut CellGraph) {
  // Parse file
  let exprs = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };

  // Find new and unchanged cells
  let mut new_cells = HashMap::new();
  for e in exprs {
    match e.shape() {
      ExprShape::List(ExprTag::Def, &[name, _args, _defs, value_expr]) => {
        let name = name.as_symbol();
        hotload_def(cg, env, name, value_expr, &mut new_cells);
      }
      _ => {
        let id = ConstCell(e);
        if !new_cells.contains_key(&id) {
          hotload_expr(cg, env, e, &mut new_cells);
        }
      }
    }
  }

  // unload any cells that were deleted or broken
  let deletion_list : Vec<_> =
    cg.cells.keys().filter(|def| match new_cells.get(def) {
      None | Some(CellStatus::Broken) => true,
      _ => false,
    })
    .cloned().collect();
  for def in deletion_list {
    unload_cell(cg, env, def)
  }
}

impl fmt::Display for CellId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DefCell(name) => write!(f, "def {}", name),
      ConstCell(expr) => write!(f, "const expr ({})", expr.loc()),
    }
  }
}
