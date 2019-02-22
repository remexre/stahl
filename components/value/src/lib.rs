//! The Stahl S-expression parser. Performs much better than the I-expression parser.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[cfg(feature = "proptest")]
extern crate proptest;

#[cfg(feature = "proptest")]
mod cfg_proptest;
mod print;

use stahl_errors::Location;
use stahl_util::SharedString;

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

    /// Creates a cons-list from an iterator of values.
    pub fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Value {
        let mut with_locs = Vec::new();
        for val in iter {
            with_locs.push((val.loc(), val));
        }

        let mut val = Value::Nil(with_locs.last().map(|(l, _)| l.clone()).unwrap_or_default());
        while let Some((loc, h)) = with_locs.pop() {
            val = Value::Cons(loc, Box::new(h), Box::new(val));
        }
        val
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

    /// Gets a mutable reference to the location at which the value appeared.
    pub fn loc_mut(&mut self) -> &mut Location {
        match self {
            Value::Cons(loc, _, _)
            | Value::Int(loc, _)
            | Value::String(loc, _)
            | Value::Symbol(loc, _)
            | Value::Nil(loc) => loc,
        }
    }
}
