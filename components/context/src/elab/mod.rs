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
            CstExpr::Hole(loc) => return Ok(hole(loc.clone())),
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
                        let ty = chk_ty.unwrap_or_else(|| hole(expr.loc()));

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
    fn collect_constraints(
        &self,
        expr: &UnifExpr,
        constraints: &mut Vec<Constraint>,
        chk_ty: Rc<UnifExpr>,
        env: &mut Vec<(SharedString, Rc<UnifExpr>)>,
    ) -> Result<()> {
        let inf_ty = match expr {
            UnifExpr::Call(loc, func, args) => {
                let call_id = genint();
                let old_env_len = env.len();

                let ty_args = (0..args.len())
                    .map(|n| {
                        let arg = &args[n];
                        let ty = hole(loc.clone());
                        self.collect_constraints(arg, constraints, ty.clone(), env)?;
                        let name = SharedString::from(format!("#ARG:{}:{}#", call_id, n));
                        env.push((name.clone(), ty.clone()));
                        Ok((name, ty))
                    })
                    .collect::<Result<Vec<_>>>()?;

                let ret_ty = hole(loc.clone());
                self.collect_constraints(
                    func,
                    constraints,
                    Rc::new(UnifExpr::Pi(
                        loc.clone(),
                        ty_args,
                        ret_ty.clone(),
                        UnifEffs::any(),
                    )),
                    env,
                )?;
                env.truncate(old_env_len);

                ret_ty
            }
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
                    .map(|arg| (arg.clone(), hole(loc.clone())))
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
                        self.collect_constraints(expr, constraints, ty.clone(), env)?;
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
                Intrinsic::Fixnum => Rc::new(UnifExpr::Type(loc.clone())),
                Intrinsic::TypeOfTypeOfTypes => {
                    raise!("The type of type of types shouldn't be typechecked")
                }
            },
            UnifExpr::UnifVar(loc, _) => hole(loc.clone()),
        };

        constraints.push(Constraint::ExprEq(expr.loc(), inf_ty, chk_ty));
        Ok(())
    }

    /// Unifies a (top-level) expression with its type.
    pub fn unify_ty_expr(&self, ty: &mut Rc<UnifExpr>, expr: &mut Rc<UnifExpr>) -> Result<()> {
        let mut constraints = Vec::new();
        if let Err(err) =
            self.collect_constraints(ty, &mut constraints, hole(ty.loc()), &mut Vec::new())
        {
            if let (UnifExpr::Type(_), UnifExpr::Intrinsic(_, Intrinsic::TypeOfTypeOfTypes)) =
                (&**expr, &**ty)
            {
                // Ignore the error; this is special-cased as legal.
            } else {
                return Err(err);
            }
        }
        self.collect_constraints(expr, &mut constraints, ty.clone(), &mut Vec::new())?;

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

/// Returns a new hole at the current location.
pub(crate) fn hole(loc: Location) -> Rc<UnifExpr> {
    Rc::new(UnifExpr::UnifVar(loc, genint()))
}
