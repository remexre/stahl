#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod from_value;

use stahl_errors::Location;
use stahl_parser::Value;
use stahl_util::SharedString;
use std::sync::Arc;

/// A top-level declaration.
#[derive(Derivative)]
#[derivative(Debug)]
pub enum Decl {
    /// A constant.
    Def(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Arc<Expr>,
    ),

    /// A effect declaration.
    DefEff(#[derivative(Debug = "ignore")] Location, Effect),
}

impl Decl {
    /// Returns the location at which the expression is.
    pub fn loc(&self) -> Location {
        match self {
            Decl::Def(loc, _, _) | Decl::DefEff(loc, _) => loc.clone(),
        }
    }
}

/// An effect.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Effect(SharedString, Arc<Expr>, Option<Arc<Expr>>);

/// A set of effects, possibly an extensible one.
#[derive(Debug, Default)]
pub struct Effects(Vec<Effect>);

/// An expression.
#[derive(Derivative)]
#[derivative(Debug)]
pub enum Expr {
    /// A call to a function with a given number of arguments.
    Call(
        #[derivative(Debug = "ignore")] Location,
        Arc<Expr>,
        Vec<Arc<Expr>>,
    ),

    /// A constant.
    Const(#[derivative(Debug = "ignore")] Location, Value),

    /// A lambda.
    Lam(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Arc<Expr>)>,
    ),

    /// A pi type with effects.
    Pi(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Arc<Expr>)>,
        Arc<Expr>,
        Effects,
    ),

    /// The type of types. Note that there is no syntax for this; it is exposed as a constant in
    /// the standard library via a tactic.
    Ty(#[derivative(Debug = "ignore")] Location),

    /// A variable.
    Var(#[derivative(Debug = "ignore")] Location, SharedString),
}

impl Expr {
    /// Returns the location at which the expression is.
    pub fn loc(&self) -> Location {
        match self {
            Expr::Call(loc, _, _)
            | Expr::Const(loc, _)
            | Expr::Lam(loc, _, _)
            | Expr::Pi(loc, _, _, _)
            | Expr::Ty(loc)
            | Expr::Var(loc, _) => loc.clone(),
        }
    }
}
