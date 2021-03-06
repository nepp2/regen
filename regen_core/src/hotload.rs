
use crate::{symbols::Symbol, compile, env::{self, CellId, CellUid, CellValue, Env, Namespace}, error::{Error, error_raw}, interpret, parse::{self, CodeModule, Expr, ExprShape, ExprTag, SrcLocation}, perm_alloc::{Ptr, perm_slice, perm_slice_from_vec}, types};

use std::{collections::{HashMap, HashSet}};

use CellId::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum CellStatus {
  New, Changed, Unchanged, Broken,
}

pub struct CellResolver<'l> {
  env : Env,
  namespace : Namespace,
  current_module : Ptr<CodeModule>,
  new_cells : &'l HashMap<CellUid, CellStatus>,
}

impl <'l> CellResolver<'l> {

  fn new(
    env : Env,
    namespace : Namespace,
    current_module : Ptr<CodeModule>,
    new_cells : &'l HashMap<CellUid, CellStatus>,
  ) -> Self {
    CellResolver { env, namespace, current_module, new_cells }
  }

  pub fn cell_status(&self, uid : CellUid) -> CellStatus {
    self.new_cells.get(&uid).cloned().unwrap_or(CellStatus::Unchanged)
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
      if let Some(cell) = env::get_cell_value(self.env, uid) {
        // Accept defs that appear before this one in the current module
        if self.new_cells.contains_key(&uid) {
          return Some(uid);
        }
        // Accept external defs
        if cell.e.loc().module.name != self.current_module.name {
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

fn get_dependencies_status(resolver : &CellResolver, uid : CellUid) -> CellStatus {
  use CellStatus::*;
  if let Some(deps) = resolver.env.dependencies.get(&uid) {
    let mut changed = false;
    for &prev_dep_uid in deps {
      let r = resolver.resolve_id(prev_dep_uid.id);
      if let Some(dep_uid) = r {
        if dep_uid != prev_dep_uid {
          changed = true;
        }
        match resolver.cell_status(dep_uid) {
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
  resolver : &CellResolver,
  uid : CellUid,
  expr : Expr,
) -> Result<(), Error>
{
  let mut env = resolver.env;
  match uid.id {
    DefCell(_) => {
      env::set_active_definition(env, Some(uid));
      let (v, deps) = load_expr(resolver, expr)?;
      env::set_active_definition(env, None);
      env.cells.insert(uid, v);
      env.dependencies.insert(uid, deps);
    }
    ExprCell(_) => {
      let (v, deps) = load_expr(resolver, expr)?;
      env.cells.insert(uid, v);
      env.dependencies.insert(uid, deps);
    }
  }
  Ok(())
}

/// Returns the evaluated expr value, and the cell's resolved dependencies
fn load_expr(resolver : &CellResolver, expr : Expr) -> Result<(CellValue, HashSet<CellUid>), Error> {
  // TODO: using catch unwind is very ugly. Replace with proper error handling.
  let v = std::panic::catch_unwind(|| {
    eval_expr(resolver, expr)
  }).map_err(|_| error_raw(SrcLocation::zero(expr.loc().module), "regen eval panic!"))?;
  Ok(v)
}

fn eval_expr(resolver : &CellResolver, e : Expr) -> (CellValue, HashSet<CellUid>) {
  let (f, dependencies) = compile::compile_expr_to_function(resolver.env, &resolver, e);
  let expr_type = types::type_as_function(&f.t).unwrap().returns;
  // TODO: it's wasteful to allocate for types that are 64bits wide or smaller
  let ptr = {
    let layout = std::alloc::Layout::from_size_align(expr_type.size_of as usize, 8).unwrap();
    unsafe { std::alloc::alloc(layout) as *mut () }
  };
  interpret::interpret_function(&f, &[], Some(ptr));
  let v = CellValue { e, t: expr_type, ptr };
  (v, dependencies)
}

fn unload_cell(env : Env, uid : CellUid){
  env::unload_cell(env, uid);
}

fn get_cell_status(resolver : &CellResolver, uid : CellUid, value_expr : Expr) -> CellStatus {
  // if already defined
  if let Some(cell) = resolver.env.cells.get(&uid) {
    // check if expression has changed
    if cell.e != value_expr {
      CellStatus::Changed
    }
    else {
      get_dependencies_status(resolver, uid)
    }
  }
  else {
    CellStatus::New
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
  // check whether the def needs to be loaded/reloaded
  use CellStatus::*;
  let resolver = CellResolver::new(env, namespace, expr.loc().module, new_cells);
  let mut new_cell_state = get_cell_status(&resolver, uid, expr);
  if new_cell_state == Changed {
    unload_cell(env, uid);
  }
  if let New | Changed = new_cell_state {
    let r = load_cell(&resolver, uid, expr);
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

struct NestedCells {
  defs : Vec<Expr>,
  const_exprs : Vec<Expr>,
  embeds : Vec<Expr>,
}

fn get_nested_cells(expr : Expr) -> NestedCells {
  fn find_nested_cells(nested_cells : &mut NestedCells, expr : Expr) {
    use parse::ExprShape::*;
    match expr.shape() {
      List(ExprTag::Def, _) => {
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
  namespace : Namespace,
  new_cells : &mut HashMap<CellUid, CellStatus>,
  embeds : &[Expr],
)
{
  hotload_cells(env, namespace, new_cells, embeds);
  for &e in embeds {
    let uid = CellUid::expr(e, namespace);
    let cell = env::get_cell_value(env, uid).unwrap();
    if cell.t == env.c.expr_tag {
      let e = unsafe { *(cell.ptr as *const Expr) };
      let nested = get_nested_cells(e);
      hotload_cells(env, namespace, new_cells, &nested.defs);
      hotload_cells(env, namespace, new_cells, &nested.const_exprs);
      hotload_embedded(env, namespace, new_cells, &nested.embeds);
    }
    else {
      println!("expected expression of type 'expr', found {}", cell.t);
    }
  }
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
      ExprShape::List(ExprTag::Def, &[name, value_expr]) => {
        let name = name.as_symbol();
        // hotload nested cells
        let nested = get_nested_cells(value_expr);
        // This nested namespace is sometimes wasteful. It causes const expressions such as
        // argument types to be evaluated again in the new namespace, when they very
        // rarely need to be.
        // let nested_namespace = {
        //   if nested.defs.len() > 0 { create_nested_namespace(namespace, name) }
        //   else { namespace }
        // };
        let nested_namespace = create_nested_namespace(namespace, name);
        hotload_cells(env, nested_namespace, new_cells, &nested.defs);
        hotload_cells(env, nested_namespace, new_cells, &nested.const_exprs);
        hotload_embedded(env, nested_namespace, new_cells, &nested.embeds);
        // hotload the def initialiser
        let uid = CellUid::def(name, namespace);
        hotload_cell(env, nested_namespace, uid, value_expr, new_cells);
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
        hotload_cells(env, namespace, new_cells, &nested.const_exprs);
        // hotload the const expression initialiser
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
  hotload_cells(env, env::new_namespace(&[]), &mut new_cells, &exprs);

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
