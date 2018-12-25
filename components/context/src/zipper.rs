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

    /// A constant.
    Const(#[derivative(Debug = "ignore")] Location, Literal),

    /// A global variable.
    GlobalVar(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        SharedString,
        SharedString,
    ),

    /// A lambda.
    Lam(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Arc<Expr>, Arc<Expr>, Effects)>,
    ),

    /// A local variable.
    LocalVar(#[derivative(Debug = "ignore")] Location, SharedString),

    /// A pi type with effects.
    Pi(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Arc<Expr>)>,
        Arc<Expr>,
        Effects,
    ),

    /// The type of types.
    Type(#[derivative(Debug = "ignore")] Location),
}
