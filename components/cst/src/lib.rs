//! The concrete syntax of Stahl.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod from_value;

use stahl_errors::Location;
use stahl_util::{fmt_iter, SharedString};
use stahl_value::Value;
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
        Arc<Expr>,
    ),

    /// A effect declaration.
    DefEff(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Arc<Expr>,
        Option<Arc<Expr>>,
    ),

    /// A effect set declaration.
    DefEffSet(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Vec<SharedString>,
    ),

    /// A type definition.
    ///
    /// The second argument is the name of the type being defined. The third argument is the kind
    /// of the type. The fourth argument is the list of constructors, which are each pairs of the
    /// constructor name and constructor type.
    DefTy(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Arc<Expr>,
        Vec<(Location, SharedString, Option<Arc<Expr>>)>,
    ),
}

impl Decl {
    /// Returns the location at which the declaration is.
    pub fn loc(&self) -> Location {
        match self {
            Decl::Def(loc, _, _, _)
            | Decl::DefEff(loc, _, _, _)
            | Decl::DefEffSet(loc, _, _)
            | Decl::DefTy(loc, _, _, _) => loc.clone(),
        }
    }

    /// Returns the name of the declaration.
    pub fn name(&self) -> SharedString {
        match self {
            Decl::Def(_, name, _, _)
            | Decl::DefEff(_, name, _, _)
            | Decl::DefEffSet(_, name, _)
            | Decl::DefTy(_, name, _, _) => name.clone(),
        }
    }
}

impl Display for Decl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Decl::Def(_, name, ty, expr) => write!(fmt, "(def {} {} {})", name, ty, expr),
            Decl::DefEff(_, name, expr, None) => write!(fmt, "(defeff {} {})", name, expr),
            Decl::DefEff(_, name, expr, Some(ret)) => {
                write!(fmt, "(defeff {} {} {})", name, expr, ret)
            }
            Decl::DefEffSet(_, name, effs) => {
                write!(fmt, "(defeffset {} ", name)?;
                for eff in effs {
                    write!(fmt, " {}", eff)?;
                }
                write!(fmt, ")")
            }
            Decl::DefTy(_, name, kind, ctors) => {
                write!(fmt, "(defty {} {}", name, kind)?;
                for (_, name, ctor) in ctors {
                    if let Some(ctor) = ctor {
                        write!(fmt, " ({} {})", name, ctor)?;
                    } else {
                        write!(fmt, " {}", name)?;
                    }
                }
                write!(fmt, ")")
            }
        }
    }
}

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

    /// A hole, to be filled in via unification.
    Hole(#[derivative(Debug = "ignore")] Location),

    /// A lambda.
    Lam(
        #[derivative(Debug = "ignore")] Location,
        Vec<Option<SharedString>>,
        Vec<(Option<(SharedString, Option<Arc<Expr>>)>, Arc<Expr>)>,
    ),

    /// A pi type with effects.
    Pi(
        #[derivative(Debug = "ignore")] Location,
        Vec<(Option<SharedString>, Arc<Expr>)>,
        Arc<Expr>,
        Vec<SharedString>,
    ),

    /// A variable.
    Var(#[derivative(Debug = "ignore")] Location, SharedString),
}

impl Expr {
    /// Returns the location at which the expression is.
    pub fn loc(&self) -> Location {
        match self {
            Expr::Call(loc, _, _)
            | Expr::Const(loc, _)
            | Expr::Hole(loc)
            | Expr::Lam(loc, _, _)
            | Expr::Pi(loc, _, _, _)
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
            Expr::Hole(_) => write!(fmt, "_"),
            Expr::Lam(_, args, body) => {
                write!(fmt, "(fn (")?;
                fmt_iter(
                    fmt,
                    args.iter().map(|s| match s {
                        Some(s) => s.as_str(),
                        None => "_",
                    }),
                )?;
                write!(fmt, ")")?;
                for (def_info, expr) in body {
                    match def_info {
                        Some((name, None)) => write!(fmt, " (def {} {})", name, expr)?,
                        Some((name, Some(ty))) => write!(fmt, " (def {} {} {})", name, ty, expr)?,
                        None => write!(fmt, " {}", expr)?,
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
                    write!(fmt, "(")?;
                    if let Some(name) = name {
                        write!(fmt, "{}", name)?;
                    } else {
                        write!(fmt, "_")?;
                    }
                    write!(fmt, " {})", expr)?;
                }
                write!(fmt, ") {}", body)?;
                if !effs.is_empty() {
                    unimplemented!();
                }
                write!(fmt, ")")
            }
            Expr::Var(_, name) => write!(fmt, "{}", name),
        }
    }
}
