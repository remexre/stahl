mod complex_movements;
mod tactical;
mod zipper;

pub use crate::elab::zipper::Zipper;
use crate::ModContext;
use log::{debug, trace};
use stahl_ast::{Effects, Expr, FQName, Literal};
use stahl_cst::{Expr as CstExpr, Value};
use stahl_errors::{Location, Result, ResultExt};
use stahl_util::{fmt_iter, genint, unzip_result_iter, SharedString};
use std::{
    collections::HashSet,
    fmt::{Display, Formatter, Result as FmtResult},
    rc::Rc,
};

impl ModContext<'_, '_> {
    /// Elaborates a CST expression into an AST expression.
    pub fn elab(&mut self, cst_expr: &CstExpr, chk_ty: &CstExpr) -> Result<(Expr, Expr)> {
        let mut constraints = Vec::new();
        let mut locals = Vec::new();
        let (mut chk_ty, _) = self.elab_with_locals(chk_ty, None, &mut constraints, &mut locals)?;
        let (mut expr, inf_ty) =
            self.elab_with_locals(cst_expr, Some(&chk_ty), &mut constraints, &mut locals)?;
        assert!(locals.is_empty());

        constraints.push(Constraint::ExprEq(chk_ty.loc(), inf_ty, chk_ty.clone()));
        unify(&mut expr, constraints.clone())?;
        unify(&mut chk_ty, constraints)?;

        let expr = reify(&*expr).chain(|| err!(@cst_expr.loc(), "When elaborating {}", expr))?;
        let chk_ty = reify(&*chk_ty)
            .chain(|| err!(@cst_expr.loc(), "Cannot infer the type of {}", cst_expr))?;
        Ok((expr, chk_ty))
    }

    fn elab_with_locals(
        &mut self,
        expr: &CstExpr,
        _chk_ty: Option<&Rc<UnifExpr>>,
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
            CstExpr::Hole(loc) => (hole(loc.clone()), None),
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
                            constraints.push(Constraint::ExprEq(
                                expr.loc(),
                                var.clone(),
                                inf_ty.clone(),
                            ));
                            var
                        });

                        if let Some(def_name) = def_name.clone() {
                            locals.push(def_name);
                        }
                        Ok((def_name, ty, expr, unimplemented!()))
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
                } else if let Ok((name, ty)) = self.resolve(name.clone()) {
                    (Rc::new(UnifExpr::GlobalVar(loc.clone(), name)), Some(ty))
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

#[derive(Clone, Derivative)]
#[derivative(Debug)]
enum Constraint {
    /// The first effect set must equal the second.
    EffEq(#[derivative(Debug = "ignore")] Location, UnifEffs, UnifEffs),

    /// The first effect set must be a superset of the second.
    EffSuperset(#[derivative(Debug = "ignore")] Location, UnifEffs, UnifEffs),

    /// The two expressions must be equal.
    ExprEq(
        #[derivative(Debug = "ignore")] Location,
        Rc<UnifExpr>,
        Rc<UnifExpr>,
    ),
}

impl Constraint {
    /// Applies this constraint to the given expression.
    pub fn solve(
        &self,
        target: &mut Rc<UnifExpr>,
        constraints: &mut Vec<Constraint>,
    ) -> Result<()> {
        match self {
            Constraint::EffEq(_loc, l, r) => match (l.1, r.1) {
                (Some(l_tail), Some(r_tail)) => {
                    if l.0 == r.0 {
                        UnifExpr::merge_effs(
                            target,
                            l_tail,
                            UnifEffs(HashSet::new(), Some(r_tail)),
                        );
                        Ok(())
                    } else {
                        unimplemented!()
                    }
                }
                (Some(_l_tail), None) => unimplemented!(),
                (None, Some(_r_tail)) => unimplemented!(),
                (None, None) => unimplemented!(),
            },
            Constraint::EffSuperset(loc, l, r) => {
                if let Some(l_tail) = l.1 {
                    UnifExpr::merge_effs(target, l_tail, r.clone());
                    Ok(())
                } else {
                    todo!(@loc.clone(), "Solve constraint {}", self)
                }
            }
            Constraint::ExprEq(loc, l, r) => match (&**l, &**r) {
                (&UnifExpr::UnifVar(_, n), _) => {
                    UnifExpr::subst_expr(target, n, r.clone());
                    for c in constraints {
                        c.subst_expr(n, r.clone());
                    }
                    Ok(())
                }
                (_, &UnifExpr::UnifVar(_, n)) => {
                    UnifExpr::subst_expr(target, n, l.clone());
                    for c in constraints {
                        c.subst_expr(n, l.clone());
                    }
                    Ok(())
                }
                (
                    &UnifExpr::Pi(_, ref largs, ref lbody, ref leffs),
                    &UnifExpr::Pi(_, ref rargs, ref rbody, ref reffs),
                ) => {
                    if largs.len() != rargs.len() {
                        todo!(@loc.clone())
                    }

                    // TODO: I think alpha-renaming might be required here...
                    for i in 0..largs.len() {
                        constraints.push(Constraint::ExprEq(
                            loc.clone(),
                            largs[i].1.clone(),
                            rargs[i].1.clone(),
                        ));
                    }
                    constraints.push(Constraint::ExprEq(
                        loc.clone(),
                        lbody.clone(),
                        rbody.clone(),
                    ));
                    constraints.push(Constraint::EffEq(loc.clone(), leffs.clone(), reffs.clone()));
                    Ok(())
                }
                (&UnifExpr::TypeOfTypeOfTypes(_), &UnifExpr::TypeOfTypeOfTypes(_)) => Ok(()),
                _ => todo!(@loc.clone(), "Solve constraint {}", self),
            },
        }
    }

    /// Applies a substitution of an expression to this constraint.
    pub fn subst_expr(&mut self, var: usize, with: Rc<UnifExpr>) {
        match self {
            Constraint::EffEq(_, _, _) | Constraint::EffSuperset(_, _, _) => {}
            Constraint::ExprEq(_, l, r) => {
                UnifExpr::subst_expr(l, var, with.clone());
                UnifExpr::subst_expr(r, var, with);
            }
        }
    }

    /// Applies a substitution of effects to this constraint.
    pub fn subst_effs(&mut self, var: usize, with: Rc<UnifEffs>) {
        match self {
            Constraint::EffEq(_, l, r) | Constraint::EffSuperset(_, l, r) => {
                l.subst_effs(var, with.clone());
                r.subst_effs(var, with);
            }
            Constraint::ExprEq(_, l, r) => {
                UnifExpr::subst_effs(l, var, with.clone());
                UnifExpr::subst_effs(r, var, with);
            }
        }
    }
}

impl Display for Constraint {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Constraint::EffEq(_, l, r) => write!(fmt, "{} ~ {}", l, r),
            // TODO: Is Unicode reasonable here?
            Constraint::EffSuperset(_, l, r) => write!(fmt, "{} ⊇ {}", l, r),
            Constraint::ExprEq(_, l, r) => write!(fmt, "{} ~ {}", l, r),
        }
    }
}

/// A set of effects which can undergo unification.
#[derive(Clone, Debug)]
pub struct UnifEffs(HashSet<FQName>, Option<usize>);

impl UnifEffs {
    /// Returns a set of effects that unifies with anything.
    pub fn any() -> UnifEffs {
        UnifEffs(HashSet::new(), Some(genint()))
    }

    /// Returns the empty set of effects.
    pub fn none() -> UnifEffs {
        UnifEffs(HashSet::new(), None)
    }

    /// Returns whether this is `UnifEffs::none()`.
    pub fn is_none(&self) -> bool {
        self.0.is_empty() && self.1.is_none()
    }

    fn merge_effs(&mut self, var: usize, with: UnifEffs) {
        if self.1 == Some(var) {
            self.0.extend(with.0);
            self.1 = with.1;
        }
    }

    fn subst_effs(&mut self, var: usize, with: Rc<UnifEffs>) {
        unimplemented!("{}.subst_effs({}, {})", self, var, with)
    }
}

impl Display for UnifEffs {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        if self.0.is_empty() && self.1.is_some() {
            write!(fmt, "#VAR:{}#", self.1.unwrap())
        } else {
            write!(fmt, "(")?;
            let mut first = true;
            for eff in &self.0 {
                if first {
                    first = false;
                } else {
                    write!(fmt, " ")?;
                }

                write!(fmt, "{}", eff)?;
            }
            if let Some(tail) = self.1 {
                write!(fmt, " | #VAR:{}#", tail)?;
            }
            write!(fmt, ")")
        }
    }
}

impl From<Effects> for UnifEffs {
    fn from(effs: Effects) -> UnifEffs {
        UnifEffs(effs.0, None)
    }
}

/// An expression during unification. This is similar to the AST expression, but has an additional
/// constructor for unification variables.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum UnifExpr {
    /// A function call.
    Call(
        #[derivative(Debug = "ignore")] Location,
        Rc<UnifExpr>,
        Vec<Rc<UnifExpr>>,
    ),

    /// A constant literal. Note that nil and conses are _not_ literals -- they're instead defined
    /// in library code via lang items, and correspondingly CST literals containing either are
    /// lowered appropriately.
    Const(#[derivative(Debug = "ignore")] Location, Literal),

    /// A global variable.
    GlobalVar(#[derivative(Debug = "ignore")] Location, FQName),

    /// A lambda.
    Lam(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Rc<UnifExpr>, Rc<UnifExpr>, UnifEffs)>,
    ),

    /// A local variable.
    LocalVar(#[derivative(Debug = "ignore")] Location, SharedString),

    /// A pi type.
    Pi(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Rc<UnifExpr>)>,
        Rc<UnifExpr>,
        UnifEffs,
    ),

    /// The type of types.
    Type(#[derivative(Debug = "ignore")] Location),

    /// The type of the type of types. If this is still present at the end of elaboration, an error
    /// will result.
    TypeOfTypeOfTypes(#[derivative(Debug = "ignore")] Location),

    /// A unification variable.
    UnifVar(#[derivative(Debug = "ignore")] Location, usize),
}

impl UnifExpr {
    /// Collects constraints over the AST, assigning types to expressions as it goes. env is a
    /// vector with the condition that on an `Ok(_)` return, it must be the same as when it was
    /// passed in; this is for efficiency reasons.
    fn collect_constraints(
        &self,
        constraints: &mut Vec<Constraint>,
        chk_ty: Rc<UnifExpr>,
        env: &mut Vec<(SharedString, Rc<UnifExpr>)>,
    ) -> Result<()> {
        let inf_ty = match self {
            UnifExpr::Call(loc, _func, args) => {
                let _ty_args = args
                    .iter()
                    .map(|_arg| hole(loc.clone()))
                    .collect::<Vec<_>>();
                let _ret_ty = hole(loc.clone());
                unimplemented!()
            }
            UnifExpr::Const(loc, val) => {
                todo!(@loc.clone(), "the type of {} is... I need lang items", val)
            }
            // UnifExpr::GlobalVar(loc, name) => unimplemented!(),
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
                        expr.collect_constraints(constraints, ty.clone(), env)?;
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
            UnifExpr::Type(loc) => Rc::new(UnifExpr::TypeOfTypeOfTypes(loc.clone())),
            UnifExpr::TypeOfTypeOfTypes(_) => {
                raise!("The type of type of types shouldn't be typechecked")
            }
            UnifExpr::UnifVar(loc, _) => hole(loc.clone()),
            _ => todo!(@self.loc(), "collect over {}", self),
        };

        constraints.push(Constraint::ExprEq(self.loc(), inf_ty, chk_ty));
        Ok(())
    }

    fn loc(&self) -> Location {
        match self {
            UnifExpr::Call(loc, _, _)
            | UnifExpr::Const(loc, _)
            | UnifExpr::GlobalVar(loc, _)
            | UnifExpr::Lam(loc, _, _)
            | UnifExpr::LocalVar(loc, _)
            | UnifExpr::Pi(loc, _, _, _)
            | UnifExpr::Type(loc)
            | UnifExpr::TypeOfTypeOfTypes(loc)
            | UnifExpr::UnifVar(loc, _) => loc.clone(),
        }
    }

    fn merge_effs(target: &mut Rc<Self>, var: usize, with: UnifEffs) {
        match *Rc::make_mut(target) {
            UnifExpr::Call(_, ref mut func, ref mut args) => {
                args.iter_mut()
                    .for_each(|arg| UnifExpr::merge_effs(arg, var, with.clone()));
                UnifExpr::merge_effs(func, var, with);
            }
            UnifExpr::Lam(_, _, ref mut body) => {
                for &mut (_, ref mut ty, ref mut expr, ref mut effs) in body.iter_mut() {
                    UnifExpr::merge_effs(ty, var, with.clone());
                    UnifExpr::merge_effs(expr, var, with.clone());
                    effs.merge_effs(var, with.clone());
                }
            }
            UnifExpr::Pi(_, ref mut args, ref mut body, ref mut effs) => {
                args.iter_mut()
                    .for_each(|(_, arg)| UnifExpr::merge_effs(arg, var, with.clone()));
                UnifExpr::merge_effs(body, var, with.clone());
                effs.merge_effs(var, with);
            }
            _ => {} // Other constructors cannot contain effects.
        }
    }

    fn subst_expr(target: &mut Rc<Self>, var: usize, with: Rc<UnifExpr>) {
        match *Rc::make_mut(target) {
            UnifExpr::Call(_, ref mut func, ref mut args) => {
                args.iter_mut()
                    .for_each(|arg| UnifExpr::subst_expr(arg, var, with.clone()));
                UnifExpr::subst_expr(func, var, with)
            }
            UnifExpr::Lam(_, _, ref mut body) => {
                for (_, ty, expr, _effs) in body {
                    UnifExpr::subst_expr(ty, var, with.clone());
                    UnifExpr::subst_expr(expr, var, with.clone());
                }
            }
            UnifExpr::Pi(_, ref mut args, ref mut body, _) => {
                args.iter_mut()
                    .for_each(|(_, arg)| UnifExpr::subst_expr(arg, var, with.clone()));
                UnifExpr::subst_expr(body, var, with);
            }
            UnifExpr::UnifVar(_, n) if var == n => *target = with.clone(),
            _ => {} // Other constructors cannot contain (and are not themselves) UnifVar.
        }
    }

    fn subst_effs(target: &mut Rc<Self>, var: usize, with: Rc<UnifEffs>) {
        unimplemented!("{}.subst_effs({}, {})", target, var, with)
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
            UnifExpr::GlobalVar(_, name) => write!(fmt, "{}", name,),
            UnifExpr::Lam(_, args, body) => {
                write!(fmt, "(fn (")?;
                fmt_iter(fmt, args)?;
                write!(fmt, ")")?;
                for (name, ty, expr, _effs) in body {
                    let name = match name {
                        Some(name) => &*name,
                        None => "_",
                    };
                    write!(fmt, " (def {} {} {})", name, ty, expr)?;
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
                if !effs.is_none() {
                    write!(fmt, " {}", effs)?;
                }
                write!(fmt, ")")
            }
            UnifExpr::Type(_) => write!(fmt, "#TYPE#"),
            UnifExpr::TypeOfTypeOfTypes(_) => write!(fmt, "#TYPE-OF-TYPE-OF-TYPES#"),
            UnifExpr::UnifVar(_, n) => write!(fmt, "#VAR:{}#", n),
        }
    }
}

impl From<&Expr> for UnifExpr {
    fn from(expr: &Expr) -> UnifExpr {
        match expr {
            Expr::Call(loc, func, args) => UnifExpr::Call(
                loc.clone(),
                Rc::new((&**func).into()),
                args.iter().map(|_| unimplemented!()).collect(),
            ),
            Expr::Const(loc, lit) => UnifExpr::Const(loc.clone(), lit.clone()),
            Expr::GlobalVar(loc, name) => UnifExpr::GlobalVar(loc.clone(), name.clone()),
            Expr::Lam(loc, args, body) => UnifExpr::Lam(
                loc.clone(),
                args.clone(),
                body.iter().map(|_| unimplemented!()).collect(),
            ),
            Expr::LocalVar(loc, name) => UnifExpr::LocalVar(loc.clone(), name.clone()),
            Expr::Pi(loc, args, body, effs) => UnifExpr::Pi(
                loc.clone(),
                args.iter()
                    .map(|(name, ty)| (name.clone(), Rc::new((&**ty).into())))
                    .collect(),
                Rc::new((&**body).into()),
                effs.clone().into(),
            ),
            Expr::Type(loc) => UnifExpr::Type(loc.clone()),
            Expr::TypeOfTypeOfTypes(loc) => UnifExpr::TypeOfTypeOfTypes(loc.clone()),
        }
    }
}

pub fn unify_ty_expr(ty: &mut Rc<UnifExpr>, expr: &mut Rc<UnifExpr>) -> Result<()> {
    let mut constraints = Vec::new();
    if let Err(err) = ty.collect_constraints(&mut constraints, hole(ty.loc()), &mut Vec::new()) {
        if let (UnifExpr::Type(_), UnifExpr::TypeOfTypeOfTypes(_)) = (&**expr, &**ty) {
            // Ignore the error; this is special-cased as legal.
        } else {
            return Err(err);
        }
    }
    expr.collect_constraints(&mut constraints, ty.clone(), &mut Vec::new())?;

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

fn unify(target: &mut Rc<UnifExpr>, mut constraints: Vec<Constraint>) -> Result<()> {
    while let Some(c) = constraints.pop() {
        trace!("Solving {}", c);
        c.solve(target, &mut constraints)?;
    }
    Ok(())
}

pub fn reify(expr: &UnifExpr) -> Result<Expr> {
    debug!("About to reify {}", expr);
    match expr {
        UnifExpr::Call(loc, func, args) => {
            let func = reify(func).chain(|| err!(@expr.loc(), "in {}", expr))?;
            let args = args
                .iter()
                .map(|arg| reify(arg))
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            Ok(Expr::Call(loc.clone(), Box::new(func), args))
        }
        UnifExpr::GlobalVar(loc, name) => Ok(Expr::GlobalVar(loc.clone(), name.clone())),
        UnifExpr::Lam(loc, args, body) => {
            let body = body
                .iter()
                .map(|(def_name, ty, expr, effs)| {
                    let effs = Effects(effs.0.iter().cloned().collect());
                    let ty =
                        reify(ty).chain(|| err!(@ty.loc(), "in {} (the type of {})", ty, expr))?;
                    let expr = reify(expr).chain(|| err!(@expr.loc(), "in {}", expr))?;
                    Ok((def_name.clone(), Box::new(ty), Box::new(expr), effs))
                })
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            Ok(Expr::Lam(loc.clone(), args.clone(), body))
        }
        UnifExpr::LocalVar(loc, name) => Ok(Expr::LocalVar(loc.clone(), name.clone())),
        UnifExpr::Pi(loc, args, body, effs) => {
            let args = args
                .iter()
                .map(|(name, ty)| {
                    let ty = reify(ty)?;
                    Ok((name.clone(), Box::new(ty)))
                })
                .collect::<Result<_>>()
                .chain(|| err!(@expr.loc(), "in {}", expr))?;
            let body = reify(body).chain(|| err!(@expr.loc(), "in {}", expr))?;
            let effs = Effects(effs.0.iter().cloned().collect());
            Ok(Expr::Pi(loc.clone(), args, Box::new(body), effs))
        }
        UnifExpr::Type(loc) => Ok(Expr::Type(loc.clone())),
        UnifExpr::TypeOfTypeOfTypes(_loc) => {
            raise!(@expr.loc(), "{} cannot be reified; it's only available from Module::intrinsics", expr)
        }
        UnifExpr::UnifVar(loc, _) => raise!(@loc.clone(), "Cannot reify a hole!"),
        expr => todo!(@expr.loc(), "reify({:?})", expr),
    }
}

fn hole(loc: Location) -> Rc<UnifExpr> {
    Rc::new(UnifExpr::UnifVar(loc, genint()))
}
