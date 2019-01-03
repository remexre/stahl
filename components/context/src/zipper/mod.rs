#![allow(clippy::type_complexity)]

mod complex_movements;
mod tactical;

use crate::types::{UnifEffs, UnifExpr};
use stahl_errors::Location;
use stahl_util::{unwrap_rc, SharedString, SplitVec};
use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    rc::Rc,
};

/// A zipper for an expression.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Zipper {
    /// The currently focused expression, or `None` if there was a panic while modifying the
    /// position of the zipper.
    expr: Option<Rc<UnifExpr>>,

    /// The path upwards.
    pub(crate) path: Vec<ZipperPathNode>,
}

impl Zipper {
    /// Creates a zipper around the given expression.
    pub(crate) fn new(expr: Rc<UnifExpr>) -> Zipper {
        Zipper {
            expr: Some(expr),
            path: Vec::new(),
        }
    }

    /// Returns the expression under focus.
    pub fn expr(&self) -> &UnifExpr {
        self.expr.as_ref().unwrap()
    }

    /// Modifies the expression under focus.
    pub(crate) fn modify_expr(&mut self, f: impl FnOnce(Rc<UnifExpr>) -> Rc<UnifExpr>) {
        self.expr = Some(f(self.expr.take().unwrap()));
    }

    /// Returns the expression currently under focus, taking it out.
    pub(crate) fn take_expr(&mut self) -> UnifExpr {
        unwrap_rc(self.expr.take().unwrap())
    }

    /// Returns whether the zipper is at a hole.
    pub fn at_hole(&self) -> bool {
        match **self.expr.as_ref().unwrap() {
            UnifExpr::UnifVar(_, _) => true,
            _ => false,
        }
    }

    /// Returns whether the zipper is at a leaf.
    pub fn at_leaf(&self) -> bool {
        match **self.expr.as_ref().unwrap() {
            UnifExpr::Const(_, _)
            | UnifExpr::GlobalVar(_, _)
            | UnifExpr::Intrinsic(_, _)
            | UnifExpr::LocalVar(_, _)
            | UnifExpr::UnifVar(_, _) => true,
            _ => false,
        }
    }

    /// Returns whether the zipper is at the top of the expression tree.
    pub fn at_top(&self) -> bool {
        self.path.is_empty()
    }

    /// Fully zips up the zipper, returning the expression.
    pub(crate) fn into_expr(mut self) -> UnifExpr {
        self.go_to_top();
        self.take_expr()
    }

    /// Goes up one level, panicking if we're already at the top.
    pub fn go_up(&mut self) {
        let path_head = self
            .path
            .pop()
            .expect("Cannot go up from top of expression");
        self.modify_expr(|expr| Rc::new(path_head.rebuild(expr)));
    }

    /// Descends to the argument of the currently selected call expression. Panics if there aren't
    /// enough arguments, or if the expression isn't a call.
    pub fn go_to_call_arg(&mut self, argn: usize) {
        let expr = self.take_expr();
        if let UnifExpr::Call(loc, func, args) = expr {
            let (expr, args) = SplitVec::new(args, argn);
            self.path.push(ZipperPathNode::CallArgs(loc, func, args));
            self.expr = Some(expr);
        } else {
            panic!("go_to_call_arg on non-call expression {}", expr)
        }
    }

    /// Descends to the function of the currently selected call expression. Panics if the
    /// expression isn't a call.
    pub fn go_to_call_func(&mut self) {
        let expr = self.take_expr();
        if let UnifExpr::Call(loc, func, args) = expr {
            self.path.push(ZipperPathNode::CallFunc(loc, args));
            self.expr = Some(func);
        } else {
            panic!("go_to_call_func on non-call expression {}", expr)
        }
    }

    /// Descends to the expression of the currently selected lambda expression's body. Panics if the
    /// expression isn't a lambda.
    pub fn go_to_lam_expr(&mut self, exprn: usize) {
        let expr = self.take_expr();
        if let UnifExpr::Lam(loc, args, body) = expr {
            let ((name, ty, expr, effs), body) = SplitVec::new(body, exprn);
            self.path
                .push(ZipperPathNode::LamExpr(loc, args, name, ty, effs, body));
            self.expr = Some(expr);
        } else {
            panic!("go_to_lam_expr on non-lambda expression {}", expr)
        }
    }

    /// Descends to the type of the currently selected lambda expression's body. Panics if the
    /// expression isn't a lambda.
    pub fn go_to_lam_ty(&mut self, exprn: usize) {
        let expr = self.take_expr();
        if let UnifExpr::Lam(loc, args, body) = expr {
            let ((name, ty, expr, effs), body) = SplitVec::new(body, exprn);
            self.path
                .push(ZipperPathNode::LamTy(loc, args, name, expr, effs, body));
            self.expr = Some(ty);
        } else {
            panic!("go_to_lam_ty on non-lambda expression {}", expr)
        }
    }

    /// Descends to the argument of the currently selected pi expression. Panics if the expression
    /// isn't a pi.
    pub fn go_to_pi_arg(&mut self, argn: usize) {
        let expr = self.take_expr();
        if let UnifExpr::Pi(loc, args, ret, effs) = expr {
            let ((name, arg), args) = SplitVec::new(args, argn);
            self.path
                .push(ZipperPathNode::PiArg(loc, name, args, ret, effs));
            self.expr = Some(arg);
        } else {
            panic!("go_to_pi_arg on non-pi expression {}", expr)
        }
    }

    /// Descends to the return of the currently selected pi expression. Panics if the expression
    /// isn't a pi.
    pub fn go_to_pi_return(&mut self) {
        let expr = self.take_expr();
        if let UnifExpr::Pi(loc, args, ret, effs) = expr {
            self.path.push(ZipperPathNode::PiRet(loc, args, effs));
            self.expr = Some(ret);
        } else {
            panic!("go_to_pi_return on non-pi expression {}", expr)
        }
    }
}

impl Display for Zipper {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        for part in self.path.iter() {
            part.display_left(fmt)?;
        }
        write!(fmt, "{{{}}}", self.expr.as_ref().unwrap())?;
        for part in self.path.iter().rev() {
            part.display_right(fmt)?;
        }
        Ok(())
    }
}

/// A node in the path to the root of an AST expression tree, for use in a zipper.
#[derive(Derivative)]
#[derivative(Debug)]
pub(crate) enum ZipperPathNode {
    /// The argument position of a call expression.
    CallArgs(
        #[derivative(Debug = "ignore")] Location,
        Rc<UnifExpr>,
        SplitVec<Rc<UnifExpr>>,
    ),

    /// The function position of a call expression.
    CallFunc(#[derivative(Debug = "ignore")] Location, Vec<Rc<UnifExpr>>),

    /// An expression in the body of a lambda.
    LamExpr(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Option<SharedString>,
        Rc<UnifExpr>,
        UnifEffs,
        SplitVec<(Option<SharedString>, Rc<UnifExpr>, Rc<UnifExpr>, UnifEffs)>,
    ),

    /// The type of an expression in the body of a lambda.
    LamTy(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Option<SharedString>,
        Rc<UnifExpr>,
        UnifEffs,
        SplitVec<(Option<SharedString>, Rc<UnifExpr>, Rc<UnifExpr>, UnifEffs)>,
    ),

    /// An argument to a pi type.
    PiArg(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        SplitVec<(SharedString, Rc<UnifExpr>)>,
        Rc<UnifExpr>,
        UnifEffs,
    ),

    /// The return type of a pi type.
    PiRet(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Rc<UnifExpr>)>,
        UnifEffs,
    ),
}

impl ZipperPathNode {
    /// Writes the part of the node to the left of the hole.
    fn display_left(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            ZipperPathNode::CallArgs(_, _func, _args) => unimplemented!(),
            ZipperPathNode::CallFunc(_, _args) => unimplemented!(),
            ZipperPathNode::LamExpr(_, _args, _name, _ty, _effs, _body) => unimplemented!(),
            ZipperPathNode::LamTy(_, _args, _name, _body_expr, _effs, _body) => unimplemented!(),
            ZipperPathNode::PiArg(_, name, args, _, _) => {
                write!(fmt, "(pi (")?;
                for arg in args.left() {
                    write!(fmt, "({} {}) ", arg.0, arg.1)?;
                }
                write!(fmt, "({} ", name)
            }
            ZipperPathNode::PiRet(_, _args, _effs) => unimplemented!(),
        }
    }

    /// Writes the part of the node to the right of the hole.
    fn display_right(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            ZipperPathNode::CallArgs(_, _func, _args) => unimplemented!(),
            ZipperPathNode::CallFunc(_, _args) => unimplemented!(),
            ZipperPathNode::LamExpr(_, _args, _name, _ty, _effs, _body) => unimplemented!(),
            ZipperPathNode::LamTy(_, _args, _name, _body_expr, _effs, _body) => unimplemented!(),
            ZipperPathNode::PiArg(_, _, args, body, effs) => {
                write!(fmt, ")")?;
                for arg in args.right() {
                    write!(fmt, " ({} {})", arg.0, arg.1)?;
                }
                write!(fmt, ") {}", body)?;
                if !effs.is_none() {
                    write!(fmt, " {}", effs)?;
                }
                write!(fmt, ")")
            }
            ZipperPathNode::PiRet(_, _args, _effs) => unimplemented!(),
        }
    }

    /// Inserts the given expression into the correct position.
    fn rebuild(self, expr: Rc<UnifExpr>) -> UnifExpr {
        match self {
            ZipperPathNode::CallArgs(loc, func, args) => {
                UnifExpr::Call(loc, func, args.reunify(expr))
            }
            ZipperPathNode::CallFunc(loc, args) => UnifExpr::Call(loc, expr, args),
            ZipperPathNode::LamExpr(loc, args, name, ty, effs, body) => {
                UnifExpr::Lam(loc, args, body.reunify((name, ty, expr, effs)))
            }
            ZipperPathNode::LamTy(loc, args, name, body_expr, effs, body) => {
                UnifExpr::Lam(loc, args, body.reunify((name, expr, body_expr, effs)))
            }
            ZipperPathNode::PiArg(loc, name, args, body, effs) => {
                UnifExpr::Pi(loc, args.reunify((name, expr)), body, effs)
            }
            ZipperPathNode::PiRet(loc, args, effs) => UnifExpr::Pi(loc, args, expr, effs),
        }
    }
}
