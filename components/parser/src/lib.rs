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

/// An acyclic value. Note that these are inefficient to do just about anything other than parse
/// to, and so should not be used for e.g. term representation during evaluation.
#[derive(Clone, Derivative, Eq)]
#[derivative(PartialEq = "feature_allow_slow_enum")]
pub enum Value {
    Cons(
        #[derivative(PartialEq = "ignore")] Location,
        Box<Value>,
        Box<Value>,
    ),
    Int(#[derivative(PartialEq = "ignore")] Location, isize),
    String(#[derivative(PartialEq = "ignore")] Location, SharedString),
    Symbol(#[derivative(PartialEq = "ignore")] Location, SharedString),
    Nil(#[derivative(PartialEq = "ignore")] Location),
}

impl Value {
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
        grammar::ValueParser::new()
            .parse(&None, Lexer::new(s))
            .map_err(|err| convert_err(err, None, s.len()))
    }
}

/// Parses several `Value`s from a file.
pub fn parse_file(path: SharedPath) -> Result<Vec<Value>> {
    let mut buf = String::new();
    File::open(path.as_ref())?.read_to_string(&mut buf)?;
    grammar::ValuesParser::new()
        .parse(&Some(path.clone()), Lexer::new(&buf))
        .map_err(|err| convert_err(err, Some(path), buf.len()))
}

/// Parses several `Value`s from a string.
pub fn parse_str(s: &str) -> Result<Vec<Value>> {
    grammar::ValuesParser::new()
        .parse(&None, Lexer::new(s))
        .map_err(|err| convert_err(err, None, s.len()))
}

fn convert_err(
    err: ParseError<usize, Token, LexerError>,
    path: Option<SharedPath>,
    l: usize,
) -> Error {
    match err {
        ParseError::ExtraToken { token: (l, _, _) } | ParseError::InvalidToken { location: l } => {
            Error::new_point(err, path, l)
        }
        ParseError::UnrecognizedToken {
            token: Some((start, _, end)),
            ..
        } => Error::new_span(err, path, start, end),
        ParseError::UnrecognizedToken { token: None, .. } => Error::new_point(err, path, l),
        ParseError::User { error } => Error::new_file(error, path),
    }
}
