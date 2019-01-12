//! The Stahl S-expression parser. Performs much better than the I-expression parser.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate failure;

mod print;

use stahl_errors::{Error, Location, Result};
use stahl_util::{SharedPath, SharedString};

/// An acyclic value.
#[allow(missing_docs)]
#[derive(Clone, Derivative, Eq)]
#[derivative(PartialEq = "feature_allow_slow_enum")]
pub enum Value {
    Cons(
        #[derivative(PartialEq = "ignore")] Location,
        // TODO: See if there's a performance gain from converting these to Arcs/Rcs.
        Box<Value>,
        Box<Value>,
    ),
    Int(#[derivative(PartialEq = "ignore")] Location, isize),
    String(#[derivative(PartialEq = "ignore")] Location, SharedString),
    Symbol(#[derivative(PartialEq = "ignore")] Location, SharedString),
    Nil(#[derivative(PartialEq = "ignore")] Location),
}

impl Value {
    /// Disassembles a (possibly improper) cons-list.
    pub fn as_list(self) -> (Vec<Value>, Value) {
        match self {
            Value::Cons(_, h, t) => {
                let (mut l, t) = t.as_list();
                l.insert(0, *h);
                (l, t)
            }
            _ => (Vec::new(), self),
        }
    }

    /// Tries to use the value as a list of symbols.
    pub fn as_sym_list(&self) -> Option<Vec<SharedString>> {
        match self {
            Value::Cons(_, h, t) => {
                let mut t = t.as_sym_list()?;
                if let Value::Symbol(_, ref s) = **h {
                    t.insert(0, s.clone());
                    Some(t)
                } else {
                    None
                }
            }
            Value::Nil(_) => Some(Vec::new()),
            _ => None,
        }
    }

    /// Gets the location at which the value appeared.
    pub fn loc(&self) -> Location {
        match self {
            Value::Cons(loc, _, _)
            | Value::Int(loc, _)
            | Value::String(loc, _)
            | Value::Symbol(loc, _)
            | Value::Nil(loc) => loc.clone(),
        }
    }
}
