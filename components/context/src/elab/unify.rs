use crate::types::{UnifEffs, UnifExpr};
use log::trace;
use stahl_errors::{Location, Result};
use std::{
    collections::HashSet,
    fmt::{Display, Formatter, Result as FmtResult},
    rc::Rc,
};

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum Constraint {
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
            Constraint::EffEq(loc, l, r) => match (l.1, r.1) {
                (Some(l_tail), Some(r_tail)) => {
                    if l.0 == r.0 {
                        UnifExpr::merge_effs(
                            target,
                            l_tail,
                            UnifEffs(HashSet::new(), Some(r_tail)),
                        );
                        Ok(())
                    } else {
                        todo!(@loc.clone(), "EffEq({}, {})", l, r)
                    }
                }
                (Some(l_tail), None) => {
                    let l_not_r = l.0.difference(&r.0).collect::<HashSet<_>>();
                    let r_not_l = r.0.difference(&l.0).collect::<HashSet<_>>();
                    if l_not_r.len() == 0 && r_not_l.len() == 0 {
                        let new_effs = Rc::new(UnifEffs(HashSet::new(), None));
                        for c in constraints {
                            c.subst_effs(l_tail, new_effs.clone());
                        }
                        UnifExpr::subst_effs(target, l_tail, new_effs);
                        Ok(())
                    } else {
                        println!("{:?}", l_not_r);
                        println!("{:?}", r_not_l);
                        todo!(@loc.clone(), "EffEq({}, {})", l, r)
                    }
                }
                (None, Some(r_tail)) => {
                    let l_not_r = l.0.difference(&r.0).collect::<HashSet<_>>();
                    let r_not_l = r.0.difference(&l.0).collect::<HashSet<_>>();
                    if l_not_r.len() == 0 && r_not_l.len() == 0 {
                        let new_effs = Rc::new(UnifEffs(HashSet::new(), None));
                        for c in constraints {
                            c.subst_effs(r_tail, new_effs.clone());
                        }
                        UnifExpr::subst_effs(target, r_tail, new_effs);
                        Ok(())
                    } else {
                        println!("{:?}", l_not_r);
                        println!("{:?}", r_not_l);
                        todo!(@loc.clone(), "EffEq({}, {})", l, r)
                    }
                }
                (None, None) => todo!(@loc.clone(), "EffEq({}, {})", l, r),
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
                (&UnifExpr::Intrinsic(_, li), &UnifExpr::Intrinsic(_, ri)) => {
                    if li == ri {
                        Ok(())
                    } else {
                        raise!(@loc.clone(), "Cannot unify #{}# with #{}#", li, ri);
                    }
                }
                (
                    &UnifExpr::Pi(_, ref largs, ref lbody, ref leffs),
                    &UnifExpr::Pi(_, ref rargs, ref rbody, ref reffs),
                ) => {
                    if largs.len() != rargs.len() {
                        todo!(@loc.clone(), "Invalid argument count: {} vs {}", l, r);
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

impl UnifEffs {
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

impl UnifExpr {
    fn alpha(target: &mut Rc<UnifExpr>, from: &str, to: UnifExpr) {
        let target = Rc::make_mut(target);
        match target {
            UnifExpr::Call(_, ref mut func, ref mut args) => {
                for arg in args {
                    UnifExpr::alpha(arg, from, to.clone());
                }
                UnifExpr::alpha(func, from, to);
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
                    UnifExpr::alpha(ty, from, to.clone());
                    UnifExpr::alpha(expr, from, to.clone());
                }
            }
            UnifExpr::LocalVar(_, name) => {
                if name == from {
                    *target = to;
                }
            }
            UnifExpr::Pi(_, ref mut args, ref mut body, _) => {
                for (name, ref mut ty) in args {
                    if name == from {
                        return;
                    }
                    UnifExpr::alpha(ty, from, to.clone());
                }
                UnifExpr::alpha(body, from, to.clone());
            }
            UnifExpr::Const(_, _)
            | UnifExpr::GlobalVar(_, _)
            | UnifExpr::Intrinsic(_, _)
            | UnifExpr::Type(_)
            | UnifExpr::UnifVar(_, _) => {}
        }
    }

    fn merge_effs(target: &mut Rc<UnifExpr>, var: usize, with: UnifEffs) {
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

    fn subst_expr(target: &mut Rc<UnifExpr>, var: usize, with: Rc<UnifExpr>) {
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

    fn subst_effs(target: &mut Rc<UnifExpr>, var: usize, with: Rc<UnifEffs>) {
        match *Rc::make_mut(target) {
            UnifExpr::Call(_, ref mut func, ref mut args) => {
                args.iter_mut()
                    .for_each(|arg| UnifExpr::subst_effs(arg, var, with.clone()));
                UnifExpr::subst_effs(func, var, with)
            }
            UnifExpr::Lam(_, _, ref mut body) => {
                for (_, ty, expr, effs) in body {
                    UnifExpr::subst_effs(ty, var, with.clone());
                    UnifExpr::subst_effs(expr, var, with.clone());
                    effs.subst_effs(var, with.clone());
                }
            }
            UnifExpr::Pi(_, ref mut args, ref mut body, ref mut effs) => {
                args.iter_mut()
                    .for_each(|(_, arg)| UnifExpr::subst_effs(arg, var, with.clone()));
                UnifExpr::subst_effs(body, var, with.clone());
                effs.subst_effs(var, with);
            }
            _ => {} // Other constructors cannot contain effects.
        }
    }
}

/// Performs unification on the given expression.
pub fn unify(target: &mut Rc<UnifExpr>, mut constraints: Vec<Constraint>) -> Result<()> {
    while let Some(c) = constraints.pop() {
        trace!("Solving {}", c);
        c.solve(target, &mut constraints)?;
        trace!("{}", target);
    }
    Ok(())
}
