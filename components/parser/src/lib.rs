//! The Stahl parser.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lalrpop_util;
#[cfg(test)]
#[macro_use]
extern crate proptest;

lalrpop_mod!(grammar);
mod lexer;
mod print;
#[cfg(test)]
pub mod tests;

use crate::lexer::{Lexer, LexerError, Token};
use lalrpop_util::ParseError;
use stahl_errors::{Error, Location, Result};
use stahl_util::{SharedPath, SharedString};
use std::{fs::File, io::Read, str::FromStr};

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

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Value> {
        let loc = Location::new();
        grammar::ValueParser::new()
            .parse(&loc.clone(), Lexer::new(s))
            .map_err(|err| convert_err(err, loc, s.len()))
    }
}

/// Parses several `Value`s from a file.
pub fn parse_file(path: SharedPath) -> Result<Vec<Value>> {
    let mut buf = String::new();
    File::open(&path)?.read_to_string(&mut buf)?;
    let loc = Location::new().path(path);
    grammar::ValuesParser::new()
        .parse(&loc.clone(), Lexer::new(&buf))
        .map_err(|err| convert_err(err, loc, buf.len()))
}

/// Parses several `Value`s from a string.
pub fn parse_str(s: &str) -> Result<Vec<Value>> {
    let loc = Location::new();
    grammar::ValuesParser::new()
        .parse(&loc.clone(), Lexer::new(s))
        .map_err(|err| convert_err(err, loc, s.len()))
}

/// Parses several `Value`s from a string with the given location.
pub fn parse_str_from(s: &str, loc: Location) -> Result<Vec<Value>> {
    grammar::ValuesParser::new()
        .parse(&loc.clone(), Lexer::new(s))
        .map_err(|err| convert_err(err, loc, s.len()))
}

fn convert_err(err: ParseError<usize, Token, LexerError>, loc: Location, l: usize) -> Error {
    match err {
        ParseError::ExtraToken { token: (l, _, _) } | ParseError::InvalidToken { location: l } => {
            Error::new(err, loc.point(l))
        }
        ParseError::UnrecognizedToken {
            token: Some((start, _, end)),
            ..
        } => Error::new(err, loc.span(start, end)),
        ParseError::UnrecognizedToken { token: None, .. } => Error::new(err, loc.point(l)),
        ParseError::User { error } => Error::new(error, loc),
    }
}
