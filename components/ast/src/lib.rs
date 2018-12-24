#[macro_use]
extern crate stahl_errors;

mod from_value;

use stahl_errors::Location;
use stahl_parser::Value;
use stahl_util::SharedString;
use std::sync::Arc;

/// A top-level declaration.
#[derive(Debug)]
pub enum Decl {
    /// A constant.
    Def {},

    /// A effect declaration.
    DefEffects {},

    /// A type declaration.
    DefType {},
}

/// An effect.
#[derive(Debug)]
pub struct Effect(SharedString, Arc<Expr>, Option<Arc<Expr>>);

/// A set of effects, possibly an extensible one.
#[derive(Debug, Default)]
pub struct Effects(Vec<Effect>);

/// An expression.
#[derive(Debug)]
pub enum Expr {
    /// A call to a function with a given number of arguments.
    Call(Location, Arc<Expr>, Vec<Arc<Expr>>),

    /// A constant.
    Const(Location, Value),

    /// A lambda.
    Lam(
        Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Arc<Expr>)>,
    ),

    /// A pi type with effects.
    Pi(Location, Vec<(SharedString, Arc<Expr>)>, Arc<Expr>, Effects),

    /// The type of types. Note that there is no syntax for this; it is exposed as a constant in
    /// the standard library via a tactic.
    Ty(Location),

    /// A variable.
    Var(Location, SharedString),
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
