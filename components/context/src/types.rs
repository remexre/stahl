use stahl_ast::{Effects, Expr, FQName, Intrinsic, Literal};
use stahl_errors::Location;
use stahl_util::{fmt_iter, genint, SharedString};
use std::{
    collections::HashSet,
    fmt::{Display, Formatter, Result as FmtResult},
    rc::Rc,
};

/// A set of effects which can undergo unification.
#[derive(Clone, Debug, PartialEq)]
pub struct UnifEffs(pub HashSet<FQName>, pub Option<usize>);

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
#[derivative(Debug, PartialEq = "feature_allow_slow_enum")]
pub enum UnifExpr {
    /// A function call.
    Call(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        Rc<UnifExpr>,
        Vec<Rc<UnifExpr>>,
    ),

    /// A constant literal. Note that nil and conses are _not_ literals -- they're instead defined
    /// in library code via lang items, and correspondingly CST literals containing either are
    /// lowered appropriately.
    Const(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        Literal,
    ),

    /// A global variable.
    GlobalVar(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        FQName,
    ),

    /// A compiler intrinsic.
    Intrinsic(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        Intrinsic,
    ),

    /// A lambda.
    Lam(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        Vec<SharedString>,
        Vec<(Option<SharedString>, Rc<UnifExpr>, Rc<UnifExpr>, UnifEffs)>,
    ),

    /// A local variable.
    LocalVar(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        SharedString,
    ),

    /// A pi type.
    Pi(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        Vec<(SharedString, Rc<UnifExpr>)>,
        Rc<UnifExpr>,
        UnifEffs,
    ),

    /// A unification variable.
    UnifVar(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        usize,
    ),
}

impl UnifExpr {
    /// Returns the location of the `UnifExpr`.
    pub fn loc(&self) -> Location {
        match self {
            UnifExpr::Call(loc, _, _)
            | UnifExpr::Const(loc, _)
            | UnifExpr::GlobalVar(loc, _)
            | UnifExpr::Intrinsic(loc, _)
            | UnifExpr::Lam(loc, _, _)
            | UnifExpr::LocalVar(loc, _)
            | UnifExpr::Pi(loc, _, _, _)
            | UnifExpr::UnifVar(loc, _) => loc.clone(),
        }
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
            UnifExpr::Intrinsic(_, i) => write!(fmt, "#{}#", i),
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
            Expr::Intrinsic(loc, i) => UnifExpr::Intrinsic(loc.clone(), *i),
            Expr::Lam(loc, args, body) => UnifExpr::Lam(
                loc.clone(),
                args.clone(),
                body.iter()
                    .map(|(name, ty, expr, effs)| {
                        (
                            name.clone(),
                            Rc::new((&**ty).into()),
                            Rc::new((&**expr).into()),
                            effs.clone().into(),
                        )
                    })
                    .collect(),
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
        }
    }
}
