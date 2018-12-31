#[macro_use]
extern crate derivative;

use stahl_errors::Location;
use stahl_util::{fmt_iter, fmt_string, SharedString};
use std::fmt::{Display, Formatter, Result as FmtResult};

/// A fully-qualified name.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FQName(pub SharedString, pub SharedString, pub SharedString);

impl Display for FQName {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}/{}/{}", self.0, self.1, self.2)
    }
}

/// A top-level declaration.
#[derive(Derivative)]
#[derivative(Debug)]
pub enum Decl {
    /// A constant.
    Def(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Box<Expr>,
        Box<Expr>,
    ),

    /// A effect declaration.
    DefEff(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Box<Expr>,
        Option<Box<Expr>>,
    ),
}

impl Decl {
    /// Returns the location at which the declaration is.
    pub fn loc(&self) -> Location {
        match self {
            Decl::Def(loc, _, _, _) | Decl::DefEff(loc, _, _, _) => loc.clone(),
        }
    }

    /// Returns the name of the declaration.
    pub fn name(&self) -> SharedString {
        match self {
            Decl::Def(_, name, _, _) | Decl::DefEff(_, name, _, _) => name.clone(),
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
        }
    }
}

/// A set of effects, possibly an extensible one.
#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Effects(pub Vec<FQName>);

impl Display for Effects {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "(")?;
        fmt_iter(fmt, &self.0)?;
        write!(fmt, ")")
    }
}

/// An expression.
#[derive(Clone, Derivative, Eq, Ord, PartialEq, PartialOrd)]
#[derivative(Debug)]
pub enum Expr {
    /// A call to a function with a given number of arguments.
    Call(
        #[derivative(Debug = "ignore")] Location,
        Box<Expr>,
        Vec<Box<Expr>>,
    ),

    /// A constant.
    Const(#[derivative(Debug = "ignore")] Location, Literal),

    /// A global variable.
    GlobalVar(#[derivative(Debug = "ignore")] Location, FQName),

    /// A lambda.
    Lam(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Box<Expr>, Box<Expr>, Effects)>,
    ),

    /// A local variable.
    LocalVar(#[derivative(Debug = "ignore")] Location, SharedString),

    /// A pi type with effects.
    Pi(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Box<Expr>)>,
        Box<Expr>,
        Effects,
    ),

    /// The type of types.
    Type(#[derivative(Debug = "ignore")] Location),
}

impl Expr {
    /// Returns the location at which the expression is.
    pub fn loc(&self) -> Location {
        match self {
            Expr::Call(loc, _, _)
            | Expr::Const(loc, _)
            | Expr::GlobalVar(loc, _)
            | Expr::Lam(loc, _, _)
            | Expr::LocalVar(loc, _)
            | Expr::Pi(loc, _, _, _)
            | Expr::Type(loc) => loc.clone(),
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
                Literal::Int(_, _) | Literal::String(_, _) => write!(fmt, "{}", val),
                Literal::Symbol(_, _) => write!(fmt, "'{}", val),
            },
            Expr::GlobalVar(_, name) => write!(fmt, "{}", name),
            Expr::Lam(_, args, body) => {
                write!(fmt, "(fn (")?;
                fmt_iter(fmt, args)?;
                write!(fmt, ")")?;
                for (name, ty, expr, _) in body {
                    if let Some(name) = name {
                        write!(fmt, " (def {} {} {})", name, ty, expr)?;
                    } else {
                        write!(fmt, " {}", expr)?;
                    }
                }
                write!(fmt, ")")
            }
            Expr::LocalVar(_, name) => write!(fmt, "{}", name),
            Expr::Pi(_, args, body, effs) => {
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
                    write!(fmt, " {}", effs)?;
                }
                write!(fmt, ")")
            }
            Expr::Type(_) => write!(fmt, "#TYPE#"),
        }
    }
}

/// A literal value. Cons and nil are excluded, as they are converted to function calls.
#[derive(Clone, Derivative, Eq, Ord, PartialEq, PartialOrd)]
#[derivative(Debug)]
pub enum Literal {
    Int(#[derivative(Debug = "ignore")] Location, isize),
    String(#[derivative(Debug = "ignore")] Location, SharedString),
    Symbol(#[derivative(Debug = "ignore")] Location, SharedString),
}

impl Display for Literal {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Literal::Int(_, n) => write!(fmt, "{}", n),
            Literal::String(_, s) => fmt_string(s, fmt),
            Literal::Symbol(_, s) => write!(fmt, "{}", s),
        }
    }
}
