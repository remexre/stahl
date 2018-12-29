use crate::ModContext;
use log::warn;
use stahl_ast::{Effects, Expr, Literal};
use stahl_cst::{Expr as CstExpr, Value};
use stahl_errors::{Location, Result, ResultExt};
use stahl_util::{fmt_iter, genint, unwrap_rc, unzip_result_iter, SharedString};
use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    iter::once,
    rc::Rc,
};

impl ModContext<'_> {
    /// Elaborates a CST expression into an AST expression.
    pub fn elab(&mut self, cst_expr: &CstExpr, chk_ty: &CstExpr) -> Result<(Expr, Expr)> {
        let mut constraints = Vec::new();
        let mut locals = Vec::new();
        let (chk_ty, _) = self.elab_with_locals(chk_ty, None, &mut constraints, &mut locals)?;
        let (expr, inf_ty) =
            self.elab_with_locals(cst_expr, Some(&chk_ty), &mut constraints, &mut locals)?;
        assert!(locals.is_empty());

        constraints.push(Constraint(chk_ty.loc(), inf_ty, chk_ty.clone()));
        let expr = unify(expr, constraints)?;

        let expr = reify(&*expr).chain(|| err!(@cst_expr.loc(), "When elaborating {}", expr))?;
        let chk_ty = reify(&*chk_ty)
            .chain(|| err!(@cst_expr.loc(), "Cannot infer the type of {}", cst_expr))?;
        Ok((expr, chk_ty))
    }

    fn elab_with_locals(
        &mut self,
        expr: &CstExpr,
        chk_ty: Option<&Rc<UnifExpr>>,
        constraints: &mut Vec<Constraint>,
        locals: &mut Vec<SharedString>,
    ) -> Result<(Rc<UnifExpr>, Rc<UnifExpr>)> {
        let (expr, inf_ty) = match expr {
            CstExpr::Call(loc, func, args) => {
                let (func, _func_ty) = self.elab_with_locals(func, None, constraints, locals)?;
                // TODO: Give the args chk_tys based on func_ty.
                let (args, arg_tys) = unzip_result_iter(
                    args.iter()
                        .map(|arg| self.elab_with_locals(arg, None, constraints, locals)),
                )?;
                let _: Vec<_> = arg_tys;
                (Rc::new(UnifExpr::Call(loc.clone(), func, args)), None)
            }
            CstExpr::Const(loc, Value::Int(loc2, n)) => {
                (
                    Rc::new(UnifExpr::Const(loc.clone(), Literal::Int(loc2.clone(), *n))),
                    None, // TODO Infer a type
                )
            }
            CstExpr::Const(loc, Value::String(loc2, s)) => (
                Rc::new(UnifExpr::Const(
                    loc.clone(),
                    Literal::String(loc2.clone(), s.clone()),
                )),
                None, // TODO Infer a type
            ),
            CstExpr::Const(loc, Value::Symbol(loc2, s)) => (
                Rc::new(UnifExpr::Const(
                    loc.clone(),
                    Literal::Symbol(loc2.clone(), s.clone()),
                )),
                None, // TODO Infer a type
            ),
            CstExpr::Hole(loc) => (Rc::new(UnifExpr::UnifVar(loc.clone(), genint())), None),
            CstExpr::Lam(loc, args, body) => {
                let old_len = locals.len();
                locals.extend(args.iter().cloned());
                let body = body
                    .into_iter()
                    .map(|(def_info, expr)| {
                        let (def_name, chk_ty) = match def_info {
                            Some((def_name, None)) => (Some(def_name), None),
                            Some((def_name, Some(ty))) => {
                                let (ty, _) =
                                    self.elab_with_locals(ty, None, constraints, locals)?;
                                (Some(def_name), Some(ty))
                            }
                            None => (None, None),
                        };
                        let def_name = def_name.cloned();
                        let (expr, inf_ty) =
                            self.elab_with_locals(expr, chk_ty.as_ref(), constraints, locals)?;
                        let ty = chk_ty.unwrap_or_else(|| {
                            let n = genint();
                            let var = Rc::new(UnifExpr::UnifVar(expr.loc(), n));
                            constraints.push(Constraint(expr.loc(), var.clone(), inf_ty.clone()));
                            var
                        });

                        if let Some(def_name) = def_name.clone() {
                            locals.push(def_name);
                        }
                        Ok((def_name, ty, expr))
                    })
                    .collect::<Result<_>>()?;
                assert!(locals.len() >= old_len);
                locals.truncate(old_len);
                (
                    Rc::new(UnifExpr::Lam(loc.clone(), args.clone(), body)),
                    None, // TODO Infer a type
                )
            }
            /*
            CstExpr::Pi(loc, args, body, effs) => unimplemented!(),
            */
            CstExpr::Var(loc, name) => {
                if locals.contains(&name) {
                    (
                        Rc::new(UnifExpr::LocalVar(loc.clone(), name.clone())),
                        None, // TODO Look up the type
                    )
                } else if self.resolve(name).is_ok() {
                    todo!()
                } else {
                    raise!(@loc.clone(), "Undefined variable: {}", name)
                }
            }
            _ => todo!(@expr.loc(), "{:?}", expr),
        };
        Ok((
            expr,
            inf_ty.unwrap_or_else(|| Rc::new(UnifExpr::UnifVar(Location::default(), genint()))),
        ))
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
struct Constraint(
    #[derivative(Debug = "ignore")] Location,
    Rc<UnifExpr>,
    Rc<UnifExpr>,
);

#[derive(Clone, Derivative)]
#[derivative(Debug)]
enum UnifExpr {
    Call(
        #[derivative(Debug = "ignore")] Location,
        Rc<UnifExpr>,
        Vec<Rc<UnifExpr>>,
    ),
    Const(#[derivative(Debug = "ignore")] Location, Literal),
    GlobalVar(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        SharedString,
        SharedString,
    ),
    Lam(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Rc<UnifExpr>, Rc<UnifExpr>)>,
    ),
    LocalVar(#[derivative(Debug = "ignore")] Location, SharedString),
    Pi(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Rc<UnifExpr>)>,
        Rc<UnifExpr>,
        Effects,
    ),
    Type(#[derivative(Debug = "ignore")] Location),
    UnifVar(#[derivative(Debug = "ignore")] Location, usize),
}

impl UnifExpr {
    fn gather_effects(&self) -> Effects {
        match self {
            UnifExpr::Call(_, func, args) => args
                .iter()
                .map(|arg| arg.gather_effects())
                .chain(once(func.gather_effects()))
                .sum(),
            UnifExpr::Pi(_, args, body, effs) => args
                .iter()
                .map(|(_, arg)| arg.gather_effects())
                .chain(once(body.gather_effects()))
                .sum(),
            UnifExpr::Const(_, _)
            | UnifExpr::GlobalVar(_, _, _, _)
            | UnifExpr::Lam(_, _, _)
            | UnifExpr::LocalVar(_, _)
            | UnifExpr::Type(_) => Effects::default(),
            UnifExpr::UnifVar(_, _) => {
                warn!("A unification variable should not be having its effects computed...");
                Effects::default()
            }
        }
    }

    fn loc(&self) -> Location {
        match self {
            UnifExpr::Call(loc, _, _)
            | UnifExpr::Const(loc, _)
            | UnifExpr::GlobalVar(loc, _, _, _)
            | UnifExpr::Lam(loc, _, _)
            | UnifExpr::LocalVar(loc, _)
            | UnifExpr::Pi(loc, _, _, _)
            | UnifExpr::Type(loc)
            | UnifExpr::UnifVar(loc, _) => loc.clone(),
        }
    }

    fn substitute(target: &mut Rc<Self>, var: usize, with: Rc<UnifExpr>, loc: Location) {
        match *Rc::make_mut(target) {
            UnifExpr::Call(_, ref mut func, ref mut args) => {
                args.iter_mut()
                    .for_each(|arg| UnifExpr::substitute(arg, var, with.clone(), loc.clone()));
                UnifExpr::substitute(func, var, with, loc);
            }
            UnifExpr::Lam(_, _, ref mut body) => {
                body.iter_mut().for_each(|(_, ty, expr)| {
                    UnifExpr::substitute(ty, var, with.clone(), loc.clone());
                    UnifExpr::substitute(expr, var, with.clone(), loc.clone());
                });
            }
            UnifExpr::Pi(_, ref mut args, ref mut body, _) => {
                args.iter_mut()
                    .for_each(|(_, arg)| UnifExpr::substitute(arg, var, with.clone(), loc.clone()));
                UnifExpr::substitute(body, var, with, loc);
            }
            UnifExpr::UnifVar(_, n) if var == n => *target = with.clone(),
            _ => {} // Other constructors cannot contain (and are not themselves) UnifVar.
        }
    }
}

impl Display for UnifExpr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            UnifExpr::Call(_, func, args) => {
                write!(fmt, "({} ", func)?;
                fmt_iter(fmt, args)?;
                write!(fmt, ")")
            }
            UnifExpr::Const(_, val) => match val {
                Literal::Int(_, _) | Literal::String(_, _) => write!(fmt, "{}", val),
                Literal::Symbol(_, _) => write!(fmt, "'{}", val),
            },
            UnifExpr::GlobalVar(_, lib_name, mod_name, name) => {
                write!(fmt, "{}/{}/{}", lib_name, mod_name, name,)
            }
            UnifExpr::Lam(_, args, body) => {
                write!(fmt, "(fn (")?;
                fmt_iter(fmt, args)?;
                write!(fmt, ")")?;
                for (name, ty, expr) in body {
                    let name = match name {
                        Some(name) => &*name,
                        None => "_",
                    };
                    write!(fmt, "  (def {} {} {})", name, ty, expr)?;
                }
                write!(fmt, ")")
            }
            UnifExpr::LocalVar(_, name) => write!(fmt, "{}", name),
            UnifExpr::Pi(_, args, body, effs) => {
                write!(fmt, "(pi (")?;
                let mut first = true;
                for (name, expr) in args {
                    if first {
                        first = false;
                    } else {
                        fmt.write_str(" ")?;
                    }
                    write!(fmt, "({} {})", name, expr)?;
                }
                write!(fmt, ") {}", body)?;
                if !effs.0.is_empty() {
                    unimplemented!();
                }
                write!(fmt, ")")
            }
            UnifExpr::Type(_) => write!(fmt, "#TYPE#"),
            UnifExpr::UnifVar(_, n) => write!(fmt, "#VAR:{}#", n),
        }
    }
}

fn unify(mut target: Rc<UnifExpr>, mut constraints: Vec<Constraint>) -> Result<Rc<UnifExpr>> {
    for Constraint(loc, l, r) in constraints {
        match (&*l, &*r) {
            (&UnifExpr::UnifVar(_, n), _) => UnifExpr::substitute(&mut target, n, r, loc),
            (_, &UnifExpr::UnifVar(_, n)) => UnifExpr::substitute(&mut target, n, l, loc),
            (l, r) => raise!(@loc,"Cannot unify {} with {}", l, r),
        }
    }
    Ok(target)
}

fn reify(expr: &UnifExpr) -> Result<Expr> {
    match expr {
        UnifExpr::Lam(loc, args, body) => {
            let body = body
                .iter()
                .map(|(def_name, ty, expr)| {
                    let effs = expr.gather_effects();
                    let ty =
                        reify(ty).chain(|| err!(@ty.loc(), "in {} (the type of {})", ty, expr))?;
                    let expr = todo!("{:?} {} {} {}", def_name, ty, expr, effs);
                    Ok((def_name.clone(), Box::new(ty), expr, effs))
                })
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            Ok(Expr::Lam(loc.clone(), args.clone(), body))
        }
        UnifExpr::UnifVar(loc, _) => raise!(@loc.clone(), "Cannot reify a hole!"),
        expr => todo!(@expr.loc(), "reify({:?})", expr),
    }
}
