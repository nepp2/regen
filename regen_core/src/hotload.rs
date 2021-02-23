
use crate::{
  compile,
  env::{self, Env, CellId, CellValue},
  error::{Error, error, error_raw},
  interpret,
  parse::{self, CodeModule, Expr, ExprShape, ExprTag, SrcLocation},
  perm_alloc::Ptr,
  semantic::{self, ReferenceInfo},
  symbols::Symbol,
  types,
};

use std::collections::HashMap;

use CellId::*;

#[derive(Clone, Copy, PartialEq, Debug)]
enum CellStatus {
  New, Changed, Unchanged, Broken,
}

fn check_dependencies(
  env : Env,
  new_cells : &HashMap<CellId, CellStatus>,
  expr : Expr,
  info : &ReferenceInfo,
) -> Result<(), Error>
{
  let id = ConstCell(expr);
  // check dependencies are available
  for dep_id in &info.dependencies {
    match new_cells.get(dep_id) {
      Some(CellStatus::Broken) => {
        return error(expr.loc(),
          format!("{} depends on broken dependency {}", id, dep_id));
      }
      None => {
        if let Some(v) = env.cells.get(&dep_id) {
          if expr.loc().module.name != v.e.loc().module.name {
            // this is an external dependency
            continue;
          }
        }
        return error(expr.loc(),
          format!("{} is missing dependency {}", id, dep_id));
      }
      _ => (),
    }
  }
  Ok(())
}

fn is_external_dependency(env : Env, current_module : Ptr<CodeModule>, id : CellId) -> bool {
  // const expressions should always be local, but their CellIds will clash.
  let TODO = ();
  if let DefCell(name) = id {
    if let Some(cell) = env.cells.get(&id) {
      if cell.e.loc().module.name != current_module.name {
        return true;
      }
    }
  }
  false
}

fn get_dependencies_status(
  env : Env,
  id : CellId,
  current_module : Ptr<CodeModule>,
  new_cells : &HashMap<CellId, CellStatus>,
) -> CellStatus
{
  if let Some(deps) = env.dependencies.get(&id) {
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


fn load_expr(
  env : Env,
  new_cells : &HashMap<CellId, CellStatus>,
  expr : Expr,
) -> Result<(CellValue, ReferenceInfo), Error>
{
  let info = semantic::get_semantic_info(expr);
  check_dependencies(env, new_cells, expr, &info)?;
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let v = std::panic::catch_unwind(|| {
    eval_expr(env, expr, &info)
  }).map_err(|_| error_raw(SrcLocation::zero(), "regen eval panic!"))?;
  Ok((v, info))
}

fn eval_expr(env : Env, e : Expr, info : &ReferenceInfo) -> CellValue {
  let f = compile::compile_expr_to_function(env, info, e);
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
  new_cells : &mut HashMap<CellId, CellStatus>,
  name : Symbol,
  value_expr : Expr,
) -> Result<(), Error>
{
  env::set_active_definition(env, Some(name));
  let (v, info) = load_expr(env, new_cells, value_expr)?;
  env::set_active_definition(env, None);
  let id = DefCell(name);
  env.cells.insert(id, v);
  env.dependencies.insert(id, info.dependencies);
  Ok(())
}

fn unload_cell(mut env : Env, id : CellId){
  env::unload_cell(env, id);
}

fn get_def_status(
  env : Env,
  name : Symbol,
  value_expr : Expr,
  new_cells : &HashMap<CellId, CellStatus>,
) -> CellStatus
{
  // if already defined
  if let Some(cell) = env.cells.get(&DefCell(name)) {
    // check if expression has changed
    if cell.e != value_expr {
      CellStatus::Changed
    }
    else {
      get_dependencies_status(env, DefCell(name), value_expr.loc().module, new_cells)
    }
  }
  else {
    CellStatus::New
  }
}

fn hotload_nested_const_exprs(
  env : Env,
  expr : Expr,
  new_cells : &mut HashMap<CellId, CellStatus>,
) {
  let const_exprs = semantic::get_ordered_const_exprs(expr);
  for e in const_exprs {
    hotload_expr(env, e, new_cells);
  }
}

fn hotload_def(
  mut env : Env,
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
  hotload_nested_const_exprs(env, value_expr, new_cells);
  // check whether the def needs to be updated
  use CellStatus::*;
  let mut new_cell_state = get_def_status(env, name, value_expr, new_cells);
  if new_cell_state == Changed {
    unload_cell(env, id);
  }
  if let New | Changed = new_cell_state {
    let r = load_def(env, new_cells, name, value_expr);
    if let Err(e) = r {
      println!("{}", e.display());
      new_cell_state = Broken;
    }
  }
  if new_cell_state == Unchanged {
    // Update nodes so that their text locations are correct
    env.cells.get_mut(&id).unwrap().e = value_expr;
  }
  new_cells.insert(id, new_cell_state);
}

fn hotload_expr(
  mut env : Env,
  expr : Expr,
  new_cells : &mut HashMap<CellId, CellStatus>,
)
{
  let id = ConstCell(expr);
  if new_cells.contains_key(&id) {
    return;
  }
  // make sure any nested const expressions have been loaded
  hotload_nested_const_exprs(env, expr, new_cells);
  // check whether this expr needs to be loaded/reloaded
  use CellStatus::*;
  let mut new_cell_state =
    get_dependencies_status(env, id, expr.loc().module, new_cells);
  if new_cell_state == Changed {
    unload_cell(env, id);
  }
  if let New | Changed = new_cell_state {
    // load the main expr
    match load_expr(env, new_cells, expr) {
      Ok((v, info)) => {
        env.cells.insert(id, v);
        env.dependencies.insert(id, info.dependencies);
      }
      Err(e) => {
        println!("{}", e.display());
        new_cell_state = Broken;
      }
    }
  }
  if new_cell_state == Unchanged {
    // Update nodes so that their text locations are correct
    env.cells.get_mut(&id).unwrap().e = expr;
  }
  new_cells.insert(id, new_cell_state);
}

pub fn interpret_module(module_name : &str, code : &str, env : Env) {
  hotload_changes(module_name, &code, env);
}

fn hotload_cells(
  env : Env,
  new_cells : &mut HashMap<CellId, CellStatus>,
  cell_exprs : &[Expr]
)
{
  for &e in cell_exprs {
    match e.shape() {
      ExprShape::List(ExprTag::Def, &[name, _args, defs, value_expr]) => {
        hotload_cells(env, new_cells, defs.children());
        let name = name.as_symbol();
        hotload_def(env, name, value_expr, new_cells);
      }
      _ => {
        let id = ConstCell(e);
        if !new_cells.contains_key(&id) {
          hotload_expr(env, e, new_cells);
        }
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
  hotload_cells(env, &mut new_cells, &exprs);

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
