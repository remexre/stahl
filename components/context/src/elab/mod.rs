mod unify;

use crate::{
    elab::unify::{unify, Constraint},
    types::{UnifEffs, UnifExpr},
    ModContext,
};
use log::debug;
use stahl_ast::{Decl, Effects, Expr, Intrinsic, Literal};
use stahl_cst::{Expr as CstExpr, Value};
use stahl_errors::{Location, Result, ResultExt};
use stahl_util::{genint, SharedString};
use std::{rc::Rc, sync::Arc};

impl ModContext<'_, '_> {
    /// Elaborates a CST expression into an AST expression.
    pub fn elab(&mut self, cst_expr: &CstExpr, ty: &CstExpr) -> Result<(Arc<Expr>, Arc<Expr>)> {
        let mut ty = self.cst_to_unif(ty, &mut Vec::new())?;
        let mut expr = self.cst_to_unif(cst_expr, &mut Vec::new())?;

        self.unify_ty_expr(&mut ty, &mut expr)?;

        let expr = reify(&*expr).chain(|| err!(@cst_expr.loc(), "When elaborating {}", expr))?;
        let chk_ty =
            reify(&*ty).chain(|| err!(@cst_expr.loc(), "Cannot infer the type of {}", cst_expr))?;
        Ok((expr, chk_ty))
    }

    fn cst_to_unif(
        &mut self,
        expr: &CstExpr,
        locals: &mut Vec<SharedString>,
    ) -> Result<Rc<UnifExpr>> {
        let expr = match expr {
            CstExpr::Call(loc, func, args) => {
                let func = self.cst_to_unif(func, locals)?;
                let args = args
                    .iter()
                    .map(|arg| self.cst_to_unif(arg, locals))
                    .collect::<Result<_>>()?;
                UnifExpr::Call(loc.clone(), func, args)
            }
            CstExpr::Const(loc, Value::Int(loc2, n)) => {
                UnifExpr::Const(loc.clone(), Literal::Int(loc2.clone(), *n))
            }
            CstExpr::Const(loc, Value::String(loc2, s)) => {
                UnifExpr::Const(loc.clone(), Literal::String(loc2.clone(), s.clone()))
            }
            CstExpr::Const(loc, Value::Symbol(loc2, s)) => {
                UnifExpr::Const(loc.clone(), Literal::Symbol(loc2.clone(), s.clone()))
            }
            CstExpr::Hole(loc) => return Ok(UnifExpr::hole(loc.clone())),
            CstExpr::Lam(loc, args, body) => {
                let old_len = locals.len();
                locals.extend(args.iter().cloned());
                let body = body
                    .into_iter()
                    .map(|(def_info, expr)| {
                        let (def_name, chk_ty) = match def_info {
                            Some((def_name, None)) => (Some(def_name), None),
                            Some((def_name, Some(ty))) => {
                                let ty = self.cst_to_unif(ty, locals)?;
                                (Some(def_name), Some(ty))
                            }
                            None => (None, None),
                        };
                        let def_name = def_name.cloned();
                        let expr = self.cst_to_unif(expr, locals)?;
                        let ty = chk_ty.unwrap_or_else(|| UnifExpr::hole(expr.loc()));

                        if let Some(def_name) = def_name.clone() {
                            locals.push(def_name);
                        }
                        Ok((def_name, ty, expr, UnifEffs::any()))
                    })
                    .collect::<Result<_>>()?;
                assert!(locals.len() >= old_len);
                locals.truncate(old_len);
                UnifExpr::Lam(loc.clone(), args.clone(), body)
            }
            CstExpr::Pi(loc, args, body, effs) => {
                let old_len = locals.len();
                let args = args
                    .iter()
                    .map(|(name, ty)| {
                        let ty = self.cst_to_unif(ty, locals)?;
                        locals.push(name.clone());
                        Ok((name.clone(), ty))
                    })
                    .collect::<Result<_>>()?;
                let body = self.cst_to_unif(body, locals)?;
                assert!(locals.len() >= old_len);
                locals.truncate(old_len);

                let effs = effs
                    .into_iter()
                    .map(|name| match self.resolve(name.clone()) {
                        Ok((name, Decl::DefEff(_, _, _, _))) => Ok(name),
                        Ok((name, _)) => raise!(@loc.clone(), "{} is not an effect", name),
                        Err(err) => Err(err),
                    })
                    .collect::<Result<_>>()?;
                UnifExpr::Pi(loc.clone(), args, body, UnifEffs(effs, None))
            }
            CstExpr::Var(loc, name) => {
                if locals.contains(&name) {
                    UnifExpr::LocalVar(loc.clone(), name.clone())
                } else {
                    match self.resolve(name.clone()) {
                        Ok((name, _)) => UnifExpr::GlobalVar(loc.clone(), name),
                        Err(err) => {
                            return Err(err.chain(err!(@loc.clone(), "Undefined variable: {}", name)))
                        }
                    }
                }
            }
            _ => todo!(@expr.loc(), "{:?}", expr),
        };
        Ok(Rc::new(expr))
    }

    /// Collects constraints over the AST, assigning types to expressions as it goes. env is a
    /// vector with the condition that on an `Ok(_)` return, it must be the same as when it was
    /// passed in; it is implemented this way for efficiency reasons.
    fn tyck(
        &self,
        expr: &UnifExpr,
        constraints: &mut Vec<Constraint>,
        chk_ty: Option<Rc<UnifExpr>>,
        env: &mut Vec<(SharedString, Rc<UnifExpr>)>,
    ) -> Result<Rc<UnifExpr>> {
        println!(
            "tyck {} <- {}",
            expr,
            chk_ty
                .as_ref()
                .map(|ty| ty.to_string())
                .unwrap_or_else(String::new)
        );
        let inf_ty = match expr {
            UnifExpr::Call(loc, func, args) => match &*self.tyck(func, constraints, None, env)? {
                UnifExpr::Pi(_, arg_tys, body, _) => {
                    if args.len() != arg_tys.len() {
                        raise!(@loc.clone(), "{} takes {} arguments, but {} were provided", func,
                            arg_tys.len(), args.len());
                    }

                    let mut arg_tys = arg_tys.clone();
                    let mut body = body.clone();
                    let old_env_len = env.len();
                    for arg in args {
                        let (name, ty) = arg_tys.remove(0);
                        let ty = self.tyck(arg, constraints, Some(ty), env)?;
                        env.push((name.clone(), ty.clone()));
                        for (n, arg_ty) in &mut arg_tys {
                            if *n == name {
                                break;
                            }
                            UnifExpr::beta(arg_ty, &name, arg.clone())
                        }
                        UnifExpr::beta(&mut body, &name, ty)
                    }
                    env.truncate(old_env_len);

                    body
                }
                ty => raise!(@loc.clone(), "The target of a call must be a function, not {}", ty),
            },
            UnifExpr::Const(loc, val) => match val {
                Literal::Int(loc, _) => {
                    Rc::new(UnifExpr::Intrinsic(loc.clone(), Intrinsic::Fixnum))
                }
                _ => todo!(@loc.clone(), "the type of {} is... I need lang items", val),
            },
            UnifExpr::GlobalVar(loc, name) => match self.get_decl(name.clone()) {
                Some(Decl::Def(_, _, ty, _)) => Rc::new((&**ty).into()),
                Some(Decl::DefEff(_, _, _, _)) => {
                    raise!(@loc.clone(), "{} is an effect, not a value", name)
                }
                None => raise!(
                    @loc.clone(),
                    "Undefined variable {} (although this should've been caught earlier?)",
                    name
                ),
            },
            UnifExpr::Lam(loc, args, body) => {
                let arg_tys = args
                    .iter()
                    .map(|arg| (arg.clone(), UnifExpr::hole(loc.clone())))
                    .collect::<Vec<_>>();

                let old_env_len = env.len();
                env.extend(arg_tys.clone());

                let lam_effs = UnifEffs::any();
                let mut body_tys = body
                    .iter()
                    .map(|(_, ty, expr, effs)| {
                        constraints.push(Constraint::EffSuperset(
                            expr.loc(),
                            lam_effs.clone(),
                            effs.clone(),
                        ));
                        self.tyck(expr, constraints, Some(ty.clone()), env)?;
                        Ok(ty.clone())
                    })
                    .collect::<Result<Vec<_>>>()?;

                env.truncate(old_env_len);

                Rc::new(UnifExpr::Pi(
                    loc.clone(),
                    arg_tys,
                    body_tys.pop().unwrap(),
                    lam_effs,
                ))
            }
            UnifExpr::LocalVar(loc, name) => (0..env.len())
                .rev()
                .find(|&i| env[i].0 == name)
                .map(|i| env[i].1.clone())
                .ok_or_else(|| err!(@loc.clone(), "Undefined local variable: {}", name))?,
            UnifExpr::Pi(loc, _, _, _) => Rc::new(UnifExpr::Type(loc.clone())),
            UnifExpr::Type(loc) => Rc::new(UnifExpr::Intrinsic(
                loc.clone(),
                Intrinsic::TypeOfTypeOfTypes,
            )),
            UnifExpr::Intrinsic(loc, i) => match *i {
                Intrinsic::Fixnum | Intrinsic::String | Intrinsic::Symbol => {
                    Rc::new(UnifExpr::Type(loc.clone()))
                }
                Intrinsic::TypeOfTypeOfTypes => {
                    raise!("The type of type of types shouldn't be typechecked")
                }
            },
            UnifExpr::UnifVar(loc, _) => UnifExpr::hole(loc.clone()),
        };

        if let Some(chk_ty) = chk_ty {
            constraints.push(Constraint::ExprEq(expr.loc(), inf_ty.clone(), chk_ty));
        }
        Ok(inf_ty)
    }

    /// Unifies a (top-level) expression with its type.
    pub fn unify_ty_expr(&self, ty: &mut Rc<UnifExpr>, expr: &mut Rc<UnifExpr>) -> Result<()> {
        let mut constraints = Vec::new();
        if let Err(err) = self.tyck(ty, &mut constraints, None, &mut Vec::new()) {
            if let (UnifExpr::Type(_), UnifExpr::Intrinsic(_, Intrinsic::TypeOfTypeOfTypes)) =
                (&**expr, &**ty)
            {
                // Ignore the error; this is special-cased as legal.
            } else {
                return Err(err);
            }
        }
        self.tyck(expr, &mut constraints, Some(ty.clone()), &mut Vec::new())?;

        debug!("Unifying a declaration:");
        debug!("  ty = {}", ty);
        debug!("expr = {}", expr);
        let mut first = true;
        for c in &constraints {
            let s = if first {
                first = false;
                '['
            } else {
                ','
            };
            debug!("{} {}", s, c);
        }
        debug!("]");

        unify(expr, constraints.clone())?;
        unify(ty, constraints)?;

        Ok(())
    }
}

pub fn reify(expr: &UnifExpr) -> Result<Arc<Expr>> {
    debug!("About to reify {}", expr);
    match expr {
        UnifExpr::Call(loc, func, args) => {
            let func = reify(func).chain(|| err!(@expr.loc(), "in {}", expr))?;
            let args = args
                .iter()
                .map(|arg| reify(arg))
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            Ok(Arc::new(Expr::Call(loc.clone(), func, args)))
        }
        UnifExpr::Const(loc, lit) => Ok(Arc::new(Expr::Const(loc.clone(), lit.clone()))),
        UnifExpr::GlobalVar(loc, name) => Ok(Arc::new(Expr::GlobalVar(loc.clone(), name.clone()))),
        UnifExpr::Intrinsic(loc, i) => Ok(Arc::new(Expr::Intrinsic(loc.clone(), *i))),
        UnifExpr::Lam(loc, args, body) => {
            let body = body
                .iter()
                .map(|(def_name, ty, expr, effs)| {
                    let effs = Effects(effs.0.iter().cloned().collect());
                    let ty =
                        reify(ty).chain(|| err!(@ty.loc(), "in {} (the type of {})", ty, expr))?;
                    let expr = reify(expr).chain(|| err!(@expr.loc(), "in {}", expr))?;
                    Ok((def_name.clone(), ty, expr, effs))
                })
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            Ok(Arc::new(Expr::Lam(loc.clone(), args.clone(), body)))
        }
        UnifExpr::LocalVar(loc, name) => Ok(Arc::new(Expr::LocalVar(loc.clone(), name.clone()))),
        UnifExpr::Pi(loc, args, body, effs) => {
            let args = args
                .iter()
                .map(|(name, ty)| {
                    let ty = reify(ty)?;
                    Ok((name.clone(), ty))
                })
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            let body = reify(body).chain(|| err!(@expr.loc(), "in {}", expr))?;
            let effs = Effects(effs.0.iter().cloned().collect());
            Ok(Arc::new(Expr::Pi(loc.clone(), args, body, effs)))
        }
        UnifExpr::Type(loc) => Ok(Arc::new(Expr::Type(loc.clone()))),
        UnifExpr::UnifVar(loc, _) => raise!(@loc.clone(), "Cannot reify a hole!"),
    }
}

impl UnifExpr {
    fn beta(target: &mut Rc<UnifExpr>, from: &str, to: Rc<UnifExpr>) {
        match *Rc::make_mut(target) {
            UnifExpr::Call(_, ref mut func, ref mut args) => {
                for arg in args {
                    UnifExpr::beta(arg, from, to.clone());
                }
                UnifExpr::beta(func, from, to);
            }
            UnifExpr::Lam(_, ref args, ref mut body) => {
                for arg in args {
                    if arg == from {
                        return;
                    }
                }
                for (ref name, ref mut ty, ref mut expr, _) in body {
                    if let Some(name) = name {
                        if name == from {
                            return;
                        }
                    }
                    UnifExpr::beta(ty, from, to.clone());
                    UnifExpr::beta(expr, from, to.clone());
                }
            }
            UnifExpr::LocalVar(_, ref name) => {
                if name == from {
                    *target = to;
                }
            }
            UnifExpr::Pi(_, ref mut args, ref mut body, _) => {
                for (name, ref mut ty) in args {
                    if name == from {
                        return;
                    }
                    UnifExpr::beta(ty, from, to.clone());
                }
                UnifExpr::beta(body, from, to.clone());
            }
            UnifExpr::Const(_, _)
            | UnifExpr::GlobalVar(_, _)
            | UnifExpr::Intrinsic(_, _)
            | UnifExpr::Type(_)
            | UnifExpr::UnifVar(_, _) => {}
        }
    }

    /// Returns a new hole at the given location.
    pub fn hole(loc: Location) -> Rc<UnifExpr> {
        Rc::new(UnifExpr::UnifVar(loc, genint()))
    }
}
