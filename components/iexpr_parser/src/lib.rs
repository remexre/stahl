//! The Stahl S-expression parser.
#![deny(missing_docs)]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate lalrpop_util;
#[cfg(test)]
#[macro_use]
extern crate proptest;

lalrpop_mod!(grammar);
mod lexer;
#[cfg(test)]
pub mod tests;

use crate::lexer::{Lexer, LexerError, Token};
use lalrpop_util::ParseError;
use stahl_errors::{Error, Location, PointLC, Result};
use stahl_util::SharedPath;
use stahl_value::Value;
use std::{fs::File, io::Read};

/*
/// Parses a single `Value` from a string.
pub fn parse_str_one(s: &str) -> Result<Value> {
    let loc = Location::new();
    grammar::ValueParser::new()
        .parse(&loc.clone(), Lexer::new(s))
        .map_err(|err| convert_err(err, loc, s.len()))
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
*/

fn convert_err(err: ParseError<PointLC, Token, LexerError>, loc: Location, l: PointLC) -> Error {
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
