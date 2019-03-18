//! The Stahl S-expression parser.
#![deny(missing_docs)]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate lalrpop_util;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
#[cfg(test)]
#[macro_use]
extern crate proptest;

lalrpop_mod!(grammar);
#[doc(hidden)]
pub mod lexer;
#[cfg(test)]
pub mod tests;

pub use crate::lexer::{Lexer, LexerError, Token};
use lalrpop_util::ParseError;
use stahl_errors::{Error, Location, PointLC, Result};
use stahl_util::SharedPath;
use stahl_value::Value;
use std::{fs::File, io::Read};

/// Parses several `Value`s from a file.
pub fn parse_file(path: SharedPath) -> Result<Vec<Value>> {
    let mut buf = String::new();
    File::open(&path)?.read_to_string(&mut buf)?;
    parse_str(&buf, Location::new().path(path))
}

/// Parses several `Value`s from a string with the given location.
pub fn parse_str(s: &str, loc: Location) -> Result<Vec<Value>> {
    grammar::IExprsParser::new()
        .parse(&loc.clone(), Lexer::new(s))
        .map_err(|err| convert_err(err, loc, s))
}

/// Parses one `Value` from a string with the given location.
pub fn parse_str_one(s: &str, loc: Location) -> Result<Value> {
    grammar::IExprParser::new()
        .parse(&loc.clone(), Lexer::new(s))
        .map_err(|err| convert_err(err, loc, s))
}

fn convert_err(err: ParseError<PointLC, Token, LexerError>, loc: Location, buf: &str) -> Error {
    let mut c = 1;
    let mut l = 1;
    for ch in buf.chars() {
        if ch == '\n' {
            l += 1;
            c = 1;
        } else {
            c += 1;
        }
    }
    let l = PointLC(buf.len(), l, c);

    match err {
        ParseError::ExtraToken { token: (l, _, _) } | ParseError::InvalidToken { location: l } => {
            Error::new(err, loc.point_lc(l))
        }
        ParseError::UnrecognizedToken {
            token: Some((start, _, end)),
            ..
        } => Error::new(err, loc.span_lc(start, end)),
        ParseError::UnrecognizedToken { token: None, .. } => Error::new(err, loc.point_lc(l)),
        ParseError::User { error } => Error::new(error, loc),
    }
}
