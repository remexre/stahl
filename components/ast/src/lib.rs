//! The main abstract syntax tree of Stahl.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

use stahl_errors::{raise, Error, Location, Result};
use stahl_util::{fmt_iter, fmt_string, SharedString};
use std::{
    collections::HashSet,
    fmt::{Display, Formatter, Result as FmtResult},
    str::FromStr,
    sync::Arc,
};

/// A fully-qualified name.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FQName(pub LibName, pub SharedString, pub SharedString);

impl Display for FQName {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        if self.1 == "" {
            write!(fmt, "{}:{}", self.0, self.2)
        } else {
            write!(fmt, "{}:{}:{}", self.0, self.1, self.2)
        }
    }
}

impl FromStr for FQName {
    type Err = Error;
    fn from_str(s: &str) -> Result<FQName> {
        if !s.contains(':') {
            raise!("{} is not a fully qualified name", s);
        }

        let first = s.find(':').unwrap();
        let last = s.rfind(':').unwrap();

        let lib_name = s[..first].parse()?;
        let mod_name = if first == last {
            ""
        } else {
            &s[first + 1..last]
        };
        let decl_name = &s[last + 1..];

        if decl_name == "" {
            raise!("{} is not a fully qualified name", s);
        }

        Ok(FQName(lib_name, mod_name.into(), decl_name.into()))
    }
}

/// The name of a library, including the version number.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LibName(pub SharedString, pub u16, pub u16, pub u32);

impl Display for LibName {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}-{}-{}-{}", self.0, self.1, self.2, self.3,)
    }
}

impl FromStr for LibName {
    type Err = Error;
    fn from_str(s: &str) -> Result<LibName> {
        let mut chunks = s.rsplitn(4, '-').collect::<Vec<_>>();
        if chunks.len() != 4 {
            raise!(
                "Invalid library name {:?}; the correct format is NAME-MAJOR-MINOR-PATCH",
                s
            );
        }

        let name = chunks.pop().unwrap();
        let major = chunks.pop().unwrap().parse()?;
        let minor = chunks.pop().unwrap().parse()?;
        let patch = chunks.pop().unwrap().parse()?;
        Ok(LibName(name.into(), major, minor, patch))
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

    /// The declaration of a set of effects.
    DefEffSet(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Vec<FQName>,
    ),

    /// A type definition.
    ///
    /// The second argument is the name of the type being defined. The third argument is a list of
    /// the types of arguments to the type, which are pairs of argument name (if the argument is
    /// not an index) and argument type. The fourth argument is the list of constructors, which are
    /// triples of the constructor name, a list of the types of arguments to the constructor, and a
    /// list of arguments to the type of the value produced by the constructor.
    ///
    /// For example, the traditional `Vect` type would be represented as:
    ///
    /// ```ignore
    /// // In the module foo:bar
    /// DefTy(_, "Vect",
    ///     [(None,      Intrinsic(_, Type)         ),
    ///      (Some("n"), GlobalVar(_, FQName("std", "", "nat")))],
    ///     [("nil",  [("T", Intrinsic(_, Type))],
    ///               [LocalVar(_, "T"), GlobalVar(_, FQName("std", "", "zero"))]),
    ///      ("cons", [("T", Intrinsic(_, Type)),
    ///                ("n", GlobalVar(_, FQName("std", "", "Nat"))),
    ///                ("h", LocalVar(_, "T")),
    ///                ("t", Call(_, GlobalVar(_, FQName("foo", "bar", "Vect")),
    ///                           [LocalVar(_, "T"),
    ///                            LocalVar(_, "n")]))],
    ///               [LocalVar(_, "T"),
    ///                Call(_, GlobalVar(_, FQName("std", "", "succ")),
    ///                     [LocalVar(_, "n")])])])
    /// ```
    DefTy(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        Vec<(Option<SharedString>, Arc<Expr>)>,
        Vec<(SharedString, Vec<(SharedString, Arc<Expr>)>, Vec<Arc<Expr>>)>,
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

    /// Returns the names introduced by the declaration.
    pub fn names(&self) -> Vec<SharedString> {
        match self {
            Decl::Def(_, name, _, _)
            | Decl::DefEff(_, name, _, _)
            | Decl::DefEffSet(_, name, _) => vec![name.clone()],
            Decl::DefTy(_, name, _, ctors) => {
                let mut names = ctors
                    .iter()
                    .map(|ctor| unimplemented!())
                    .collect::<Vec<_>>();
                names.push(name.clone());
                names
            }
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
                write!(fmt, "(defeffset {}", name)?;
                for eff in effs {
                    write!(fmt, " {}", eff)?;
                }
                write!(fmt, ")")
            }
            Decl::DefTy(_, name, ty_args, ctors) => {
                unimplemented!();
            }
        }
    }
}

/// A set of effects.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Effects(pub HashSet<FQName>);

impl Display for Effects {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "(")?;
        fmt_iter(fmt, &self.0)?;
        write!(fmt, ")")
    }
}

/// A compiler intrinsic.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Intrinsic {
    /// The propositional equality type. This is defined as an intrinsic largely so it can be used
    /// for optimization.
    Eq,

    /// The type of machine-sized integers.
    Fixnum,

    /// Addition on fixnums.
    FixnumAdd,

    /// The reflexive principle; the sole constructor of the Eq type.
    Refl,

    /// The type of strings.
    String,

    /// The type of symbols.
    Symbol,

    /// The tag for a constructor, recursion principle, or type.
    Tag(FQName),

    /// The type of types.
    Type,

    /// The type of the type of types.
    TypeOfType,
}

impl Display for Intrinsic {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Intrinsic::Eq => fmt.write_str("EQ"),
            Intrinsic::Fixnum => fmt.write_str("FIXNUM"),
            Intrinsic::FixnumAdd => fmt.write_str("FIXNUM-ADD"),
            Intrinsic::Refl => fmt.write_str("REFL"),
            Intrinsic::String => fmt.write_str("STRING"),
            Intrinsic::Symbol => fmt.write_str("SYMBOL"),
            Intrinsic::Tag(name) => return write!(fmt, "{}", name),
            Intrinsic::Type => fmt.write_str("TYPE"),
            Intrinsic::TypeOfType => fmt.write_str("TYPE-OF-TYPE"),
        }
    }
}

/// An expression.
#[derive(Clone, Derivative, Eq, PartialEq)]
#[derivative(Debug)]
pub enum Expr {
    /// A call to a function with a given number of arguments.
    Call(
        #[derivative(Debug = "ignore")] Location,
        Arc<Expr>,
        Vec<Arc<Expr>>,
    ),

    /// A constant.
    Const(#[derivative(Debug = "ignore")] Location, Literal),

    /// A global variable.
    GlobalVar(#[derivative(Debug = "ignore")] Location, FQName),

    /// A compiler intrinsic.
    Intrinsic(#[derivative(Debug = "ignore")] Location, Intrinsic),

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
}

impl Expr {
    /// Returns the location at which the expression is.
    pub fn loc(&self) -> Location {
        match self {
            Expr::Call(loc, _, _)
            | Expr::Const(loc, _)
            | Expr::GlobalVar(loc, _)
            | Expr::Intrinsic(loc, _)
            | Expr::Lam(loc, _, _)
            | Expr::LocalVar(loc, _)
            | Expr::Pi(loc, _, _, _) => loc.clone(),
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
            Expr::Intrinsic(_, i) => write!(fmt, "#{}#", i),
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
        }
    }
}

/// A literal value. Cons and nil are excluded, as they are converted to function calls.
#[allow(missing_docs)]
#[derive(Clone, Derivative, Eq, Hash)]
#[derivative(Debug, PartialEq = "feature_allow_slow_enum")]
pub enum Literal {
    Int(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        isize,
    ),
    String(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        SharedString,
    ),
    Symbol(
        #[derivative(Debug = "ignore", PartialEq = "ignore")] Location,
        SharedString,
    ),
}

impl Literal {
    /// Returns the location at which the declaration is.
    pub fn loc(&self) -> Location {
        match self {
            Literal::Int(loc, _) | Literal::String(loc, _) | Literal::Symbol(loc, _) => loc.clone(),
        }
    }
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
