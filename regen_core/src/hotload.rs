
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
  current_module : Ptr<CodeModule>,
  new_cells : &HashMap<CellUid, CellStatus>,
  expr : Expr,
  info : &ReferenceInfo,
) -> Result<HashSet<CellUid>, Error>
{
  let mut resolved = HashSet::new();
  let id = CellUid::expr(expr, namespace);
  // check dependencies are available
  for &dep_id in &info.dependencies {
    let r = resolve_cell_uid(env, namespace, current_module, new_cells, dep_id);
    if let Some((dep_uid, status)) = r {
      resolved.insert(dep_uid);      
      if status == CellStatus::Broken {
        return error(expr.loc(),
          format!("{} depends on broken dependency {}", id, dep_id));
      }
      // dependency found
      continue;
    }
    return error(expr.loc(),
      format!("{} is missing dependency {}", id, dep_id));
  }
  Ok(resolved)
}

fn resolve_cell_uid(
  env : Env,
  namespace : Namespace,
  current_module : Ptr<CodeModule>,
  new_cells : &HashMap<CellUid, CellStatus>,
  id : CellId,
) -> Option<(CellUid, CellStatus)>
{
  // Look for the id in the current module
  let mut names = namespace.names;
  loop {
    let uid = CellUid { id, namespace: Namespace { names } };
    if let Some(&status) = new_cells.get(&uid) {
      return Some((uid, status));
    }
    if names.len() == 0 {
      break;
    }
    names = names.slice_range(0..names.len()-1);
  }
  // Accept external defs
  if let DefCell(_) = id {
    if let Some((uid, cell)) = env::resolve_cell_uid(env, id, namespace) {
      if cell.e.loc().module.name != current_module.name {
        return Some((uid, CellStatus::Unchanged));
      }
    }
  }
  None
}

fn get_dependencies_status(
  env : Env,
  namespace : Namespace,
  uid : CellUid,
  current_module : Ptr<CodeModule>,
  new_cells : &HashMap<CellUid, CellStatus>,
) -> CellStatus
{
  use CellStatus::*;
  if let Some(deps) = env.dependencies.get(&uid) {
    let mut changed = false;
    for &prev_dep_uid in deps {
      let r = resolve_cell_uid(env, namespace, current_module, new_cells, prev_dep_uid.id);
      if let Some((dep_uid, status)) = r {
        if dep_uid != prev_dep_uid {
          changed = true;
        }
        match status {
          Broken => {
            return Broken;
          }
          Changed | New => {
            changed = true;
          }
          Unchanged => (),
        }
      }
      else {
        changed = true;
      }
    }
    if changed { Changed } else { Unchanged }
  }
  else {
    New
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
  let current_module = expr.loc().module;
  let deps = resolve_dependencies(env, namespace, current_module, new_cells, expr, &info)?;
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let v = std::panic::catch_unwind(|| {
    eval_expr(env, expr, &info, &deps)
  }).map_err(|_| error_raw(SrcLocation::zero(), "regen eval panic!"))?;
  Ok((v, deps))
}

fn eval_expr(env : Env, e : Expr, info : &ReferenceInfo, deps : &HashSet<CellUid>) -> CellValue {
  let mut resolved_deps = HashMap::new();
  for &uid in deps {
    let v = env::get_cell_value(env, uid).unwrap();
    resolved_deps.insert(uid.id, v);
  }
  let f = compile::compile_expr_to_function(env, info, &resolved_deps, e);
  let expr_type = types::type_as_function(&f.t).unwrap().returns;
  // TODO: it's wasteful to allocate for types that are 64bits wide or smaller
  let ptr = {
    let layout = std::alloc::Layout::from_size_align(expr_type.size_of as usize, 8).unwrap();
    unsafe { std::alloc::alloc(layout) as *mut () }
  };
  interpret::interpret_function(&f, &[], Some(ptr));
  CellValue { e, t: expr_type, ptr }
}

fn unload_cell(env : Env, uid : CellUid){
  env::unload_cell(env, uid);
}

fn get_cell_status(
  env : Env,
  namespace : Namespace,
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
      get_dependencies_status(env, namespace, uid, value_expr.loc().module, new_cells)
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
  let mut new_cell_state = get_cell_status(env, namespace, uid, expr, new_cells);
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
