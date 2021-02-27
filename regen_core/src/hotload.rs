
use crate::{
  compile,
  env::{self, Env, CellId, CellUid, CellValue, Namespace},
  error::{Error, error, error_raw},
  interpret,
  parse::{self, CodeModule, Expr, ExprShape, ExprTag, SrcLocation},
  perm_alloc::Ptr,
  semantic::{self, ReferenceInfo},
  types,
};

use std::collections::{HashMap, HashSet};

use CellId::*;

#[derive(Clone, Copy, PartialEq, Debug)]
enum CellStatus {
  New, Changed, Unchanged, Broken,
}

fn resolve_dependencies(
  env : Env,
  namespace : Namespace,
  new_cells : &HashMap<CellUid, CellStatus>,
  expr : Expr,
  info : &ReferenceInfo,
) -> Result<HashSet<CellUid>, Error>
{
  let mut resolved = HashSet::new();
  let id = CellUid::expr(expr, namespace);
  // check dependencies are available
  for &dep_id in &info.dependencies {
    if let Some((dep_uid, dep_cell)) = env::resolve_cell_uid(env, dep_id, namespace) {
      resolved.insert(dep_uid);
      if let Some(&status) = new_cells.get(&dep_uid) {
        if status == CellStatus::Broken {
          return error(expr.loc(),
            format!("{} depends on broken dependency {}", id, dep_id));
        }
        // dependency found
        continue;
      }
      else if expr.loc().module.name != dep_cell.e.loc().module.name {
        // this is an external dependency
        continue;
      }
    }
    // if this line is reached, the dependency isn't available
    return error(expr.loc(),
      format!("{} is missing dependency {}", id, dep_id));
  }
  Ok(resolved)
}

fn is_external_def(env : Env, current_module : Ptr<CodeModule>, uid : CellUid) -> bool {
  // const expressions should always be local, but their CellIds will clash.
  // long-term fix:
  //    - const expressions are refcounted
  //    - it doesn't matter which module they are loaded from
  let TODO = ();
  if let DefCell(_) = uid.id {
    if let Some(cell) = env.cells.get(&uid) {
      if cell.e.loc().module.name != current_module.name {
        return true;
      }
    }
  }
  false
}

fn get_dependencies_status(
  env : Env,
  uid : CellUid,
  current_module : Ptr<CodeModule>,
  new_cells : &HashMap<CellUid, CellStatus>,
) -> CellStatus
{
  if let Some(deps) = env.dependencies.get(&uid) {
    let mut changed = false;
    for dep_uid in deps {
      if !is_external_def(env, current_module, *dep_uid) {
        let status = new_cells.get(dep_uid);
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


/// Returns the evaluated expr value, and the cell's resolved dependencies
fn load_cell(
  mut env : Env,
  namespace : Namespace,
  new_cells : &HashMap<CellUid, CellStatus>,
  uid : CellUid,
  expr : Expr,
) -> Result<(), Error>
{
  match uid.id {
    DefCell(_) => {
      env::set_active_definition(env, Some(uid));
      let (v, deps) = load_expr(env, namespace, new_cells, expr)?;
      env::set_active_definition(env, None);
      env.cells.insert(uid, v);
      env.dependencies.insert(uid, deps);
    }
    ExprCell(_) => {
      let (v, deps) = load_expr(env, namespace, new_cells, expr)?;
      env.cells.insert(uid, v);
      env.dependencies.insert(uid, deps);
    }
  }
  Ok(())
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn load_expr(
  env : Env,
  namespace : Namespace,
  new_cells : &HashMap<CellUid, CellStatus>,
  expr : Expr,
) -> Result<(CellValue, HashSet<CellUid>), Error>
{
  let info = semantic::get_semantic_info(expr);
  let deps = resolve_dependencies(env, namespace, new_cells, expr, &info)?;
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let v = std::panic::catch_unwind(|| {
    eval_expr(env, namespace, expr, &info)
  }).map_err(|_| error_raw(SrcLocation::zero(), "regen eval panic!"))?;
  Ok((v, deps))
}

fn eval_expr(env : Env, namespace : Namespace, e : Expr, info : &ReferenceInfo) -> CellValue {
  let f = compile::compile_expr_to_function(env, namespace, info, e);
  let expr_type = types::type_as_function(&f.t).unwrap().returns;
  // TODO: it's wasteful to allocate for types that are 64bits wide or smaller
  let ptr = {
    let layout = std::alloc::Layout::from_size_align(expr_type.size_of as usize, 8).unwrap();
    unsafe { std::alloc::alloc(layout) as *mut () }
  };
  interpret::interpret_function(&f, &[], Some(ptr));
  CellValue { e, t: expr_type, ptr }
}

fn load_def(
  mut env : Env,
  namespace : Namespace,
  new_cells : &mut HashMap<CellUid, CellStatus>,
  uid : CellUid,
  value_expr : Expr,
) -> Result<(), Error>
{
  env::set_active_definition(env, Some(uid));
  let (v, deps) = load_expr(env, namespace, new_cells, value_expr)?;
  env::set_active_definition(env, None);
  env.cells.insert(uid, v);
  env.dependencies.insert(uid, deps);
  Ok(())
}

fn unload_cell(env : Env, uid : CellUid){
  env::unload_cell(env, uid);
}

fn get_def_status(
  env : Env,
  uid : CellUid,
  value_expr : Expr,
  new_cells : &HashMap<CellUid, CellStatus>,
) -> CellStatus
{
  // if already defined
  if let Some(cell) = env.cells.get(&uid) {
    // check if expression has changed
    if cell.e != value_expr {
      CellStatus::Changed
    }
    else {
      get_dependencies_status(env, uid, value_expr.loc().module, new_cells)
    }
  }
  else {
    CellStatus::New
  }
}

fn hotload_nested_const_exprs(
  env : Env,
  namespace : Namespace,
  expr : Expr,
  new_cells : &mut HashMap<CellUid, CellStatus>,
) {
  let const_exprs = semantic::get_ordered_const_exprs(expr);
  for e in const_exprs {
    let uid = CellUid::expr(e, namespace);
    hotload_cell(env, namespace, uid, e, new_cells);
  }
}

fn hotload_cell(
  mut env : Env,
  namespace : Namespace,
  uid : CellUid,
  expr : Expr,
  new_cells : &mut HashMap<CellUid, CellStatus>,
)
{
  if new_cells.contains_key(&uid) {
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
  // make sure any nested const expressions have been loaded
  hotload_nested_const_exprs(env, namespace, expr, new_cells);
  // check whether the def needs to be loaded/reloaded
  use CellStatus::*;
  let mut new_cell_state = get_def_status(env, uid, expr, new_cells);
  if new_cell_state == Changed {
    unload_cell(env, uid);
  }
  if let New | Changed = new_cell_state {
    let r = load_cell(env, namespace, new_cells, uid, expr);
    if let Err(e) = r {
      println!("{}", e.display());
      new_cell_state = Broken;
    }
  }
  if new_cell_state == Unchanged {
    // Update exprs so that their text locations are correct
    env.cells.get_mut(&uid).unwrap().e = expr;
  }
  new_cells.insert(uid, new_cell_state);
}

pub fn interpret_module(module_name : &str, code : &str, env : Env) {
  hotload_changes(module_name, &code, env);
}

fn hotload_cells(
  env : Env,
  namespace : Namespace,
  new_cells : &mut HashMap<CellUid, CellStatus>,
  cell_exprs : &[Expr],
)
{
  for &e in cell_exprs {
    match e.shape() {
      ExprShape::List(ExprTag::Def, &[name, _args, defs, value_expr]) => {
        let name = name.as_symbol();
        let uid = CellUid::def(name, namespace);
        // a new nested namespace is only needed if the def has child defs
        let nested_namespace = if defs.children().len() > 0 {
          namespace.extend(name)
        }
        else {
          namespace
        };
        hotload_cells(env, nested_namespace, new_cells, defs.children());
        hotload_cell(env, nested_namespace, uid, value_expr, new_cells);
      }
      _ => {
        let uid = CellUid::expr(e, namespace);
        hotload_cell(env, namespace, uid, e, new_cells);
      }
    }
  }
}

pub fn hotload_changes(module_name : &str, code : &str, env : Env) {
  // Parse file
  let exprs = {
    let r = std::panic::catch_unwind(|| {
      parse::parse_module(env.st, module_name, &code).unwrap()
    });
    if let Ok(n) = r { n } else { return }
  };

  // Find new and unchanged cells
  let mut new_cells = HashMap::new();
  hotload_cells(env, Namespace::new(&[]), &mut new_cells, &exprs);

  // unload any cells that were deleted or broken
  let mut deletion_list = vec![];
  for (id, v) in &env.cells {
    if v.e.loc().module.name == module_name {
      let delete = match new_cells.get(&id) {
        None | Some(CellStatus::Broken) => true,
        _ => false
      };
      if delete {
        deletion_list.push(*id)
      }
    }
  }
  for id in deletion_list {
    unload_cell(env, id)
  }
}
