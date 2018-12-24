#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod from_value;

use stahl_errors::Location;
use stahl_parser::Value;
use stahl_util::{fmt_iter, SharedString};
use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    sync::Arc,
};

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
    /// Returns the location at which the declaration is.
    pub fn loc(&self) -> Location {
        match self {
            Decl::Def(loc, _, _) | Decl::DefEff(loc, _) => loc.clone(),
        }
    }

    /// Returns the name of the declaration.
    pub fn name(&self) -> SharedString {
        match self {
            Decl::Def(_, name, _) | Decl::DefEff(_, Effect(name, _, _)) => name.clone(),
        }
    }
}

impl Display for Decl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Decl::Def(_, name, expr) => write!(fmt, "(def {} {})", name.as_ref(), expr),
            Decl::DefEff(_, Effect(name, expr, None)) => {
                write!(fmt, "(defeff {} {})", name.as_ref(), expr)
            }
            Decl::DefEff(_, Effect(name, expr, Some(ret))) => {
                write!(fmt, "(defeff {} {} {})", name.as_ref(), expr, ret)
            }
        }
    }
}

/// An effect.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Effect(SharedString, Arc<Expr>, Option<Arc<Expr>>);

impl Display for Effect {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        if let Some(ret) = self.2.as_ref() {
            write!(fmt, "({} {} {})", self.0.as_ref(), self.1, ret)
        } else {
            write!(fmt, "({} {})", self.0.as_ref(), self.1)
        }
    }
}

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

impl Display for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Expr::Call(_, func, args) => {
                write!(fmt, "({} ", func)?;
                fmt_iter(fmt, args)?;
                write!(fmt, ")")
            }
            Expr::Const(_, val) => match val {
                Value::Int(_, _) | Value::String(_, _) => write!(fmt, "{}", val),
                Value::Cons(_, _, _) | Value::Symbol(_, _) | Value::Nil(_) => {
                    write!(fmt, "'{}", val)
                }
            },
            Expr::Lam(_, args, body) => {
                write!(fmt, "(fn (")?;
                fmt_iter(fmt, args.iter().map(|s| s.as_ref()))?;
                write!(fmt, ")")?;
                for (name, expr) in body {
                    if let Some(name) = name {
                        write!(fmt, " (def {} {})", name.as_ref(), expr)?;
                    } else {
                        write!(fmt, " {}", expr)?;
                    }
                }
                write!(fmt, ")")
            }
            Expr::Pi(_, args, body, effs) => {
                write!(fmt, "(pi (")?;
                let mut first = true;
                for (name, expr) in args {
                    if first {
                        first = false;
                    } else {
                        fmt.write_str(" ")?;
                    }
                    write!(fmt, "({} {})", name.as_ref(), expr)?;
                }
                write!(fmt, ") {}", body)?;
                if !effs.0.is_empty() {
                    unimplemented!();
                }
                write!(fmt, ")")
            }
            Expr::Ty(_) => write!(fmt, "#TYPE#"),
            Expr::Var(_, name) => write!(fmt, "{}", name.as_ref()),
        }
    }
}
