#[macro_use]
extern crate failure;
#[macro_use]
extern crate lalrpop_util;
#[cfg(test)]
#[macro_use]
extern crate proptest;
extern crate regex;
extern crate stahl_errors;

lalrpop_mod!(grammar);
mod lexer;
mod print;
#[cfg(test)]
mod tests;

use crate::lexer::{Lexer, LexerError, Token};
use lalrpop_util::ParseError;
use stahl_errors::{Error, Result};
use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

/// An acyclic value. Note that these are inefficient to do just about anything other than parse
/// to, and so should not be used for e.g. term representation.
#[derive(Clone, Eq, PartialEq)]
pub enum Value {
    Cons(Box<Value>, Box<Value>),
    Int(isize),
    String(String),
    Symbol(String),
    Nil,
}

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Value> {
        grammar::ValueParser::new()
            .parse(Lexer::new(s))
            .map_err(|err| convert_err(err, None, s.len()))
    }
}

/// Parses several `Value`s from a file.
pub fn parse_file(s: &str, path: impl AsRef<Path>) -> Result<Vec<Value>> {
    let mut buf = String::new();
    File::open(path.as_ref())?.read_to_string(&mut buf)?;
    grammar::ValuesParser::new()
        .parse(Lexer::new(&buf))
        .map_err(|err| convert_err(err, Some(path.as_ref().to_owned()), s.len()))
}

/// Parses several `Value`s from a string.
pub fn parse_str(s: &str) -> Result<Vec<Value>> {
    grammar::ValuesParser::new()
        .parse(Lexer::new(s))
        .map_err(|err| convert_err(err, None, s.len()))
}

fn convert_err(
    err: ParseError<usize, Token, LexerError>,
    path: Option<PathBuf>,
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
