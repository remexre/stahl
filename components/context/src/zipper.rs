use crate::split_vec::SplitVec;
use stahl_ast::{Effects, Expr, Literal};
use stahl_errors::Location;
use stahl_util::SharedString;
use std::sync::Arc;

/// A path to the root of an AST expression tree, for use in a zipper.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Zipper {
    /// The currently focused expression.
    pub expr: Arc<Expr>,

    /// The path upwards.
    pub path: Vec<ZipperPathNode>,
}

impl Zipper {
    /// Creates a zipper around the given expression.
    pub fn new(expr: Arc<Expr>) -> Zipper {
        Zipper {
            expr,
            path: Vec::new(),
        }
    }

    /// Goes up one level, panicking if we're already at the top.
    pub fn go_up(&mut self) {
        self.expr = self
            .path
            .pop()
            .expect("Cannot go up from top of expression")
            .rebuild(self.expr.clone());
    }
}

/// A node in the path to the root of an AST expression tree, for use in a zipper.
#[derive(Derivative)]
#[derivative(Debug)]
pub enum ZipperPathNode {
    /// The argument position of a call expression.
    CallArgs(
        #[derivative(Debug = "ignore")] Location,
        Arc<Expr>,
        SplitVec<Arc<Expr>>,
    ),

    /// The function position of a call expression.
    CallFunc(#[derivative(Debug = "ignore")] Location, Vec<Arc<Expr>>),

    /// An expression in the body of a lambda.
    LamExpr(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Option<SharedString>,
        Arc<Expr>,
        Effects,
        SplitVec<(Option<SharedString>, Arc<Expr>, Arc<Expr>, Effects)>,
    ),

    /// The type of an expression in the body of a lambda.
    LamTy(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Option<SharedString>,
        Arc<Expr>,
        Effects,
        SplitVec<(Option<SharedString>, Arc<Expr>, Arc<Expr>, Effects)>,
    ),

    /// An argument to a pi type.
    PiArg(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        SplitVec<(SharedString, Arc<Expr>)>,
        Arc<Expr>,
        Effects,
    ),

    /// The return type of a pi type.
    PiRet(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Arc<Expr>)>,
        Effects,
    ),
}

impl ZipperPathNode {
    /// Inserts the given expression into the correct position.
    fn rebuild(self, expr: Arc<Expr>) -> Arc<Expr> {
        let expr = match self {
            ZipperPathNode::CallArgs(loc, func, args) => Expr::Call(loc, func, args.reunify(expr)),
            ZipperPathNode::CallFunc(loc, args) => Expr::Call(loc, expr, args),
            ZipperPathNode::LamExpr(loc, args, name, ty, effs, body) => {
                Expr::Lam(loc, args, body.reunify((name, ty, expr, effs)))
            }
            ZipperPathNode::LamTy(loc, args, name, body_expr, effs, body) => {
                Expr::Lam(loc, args, body.reunify((name, expr, body_expr, effs)))
            }
            ZipperPathNode::PiArg(loc, name, args, body, effs) => {
                Expr::Pi(loc, args.reunify((name, expr)), body, effs)
            }
            ZipperPathNode::PiRet(loc, args, effs) => Expr::Pi(loc, args, expr, effs),
        };
        Arc::new(expr)
    }
}
