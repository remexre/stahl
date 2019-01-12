use stahl_util::{fmt_string, SharedString};
use std::{
    char::from_u32,
    fmt::{Display, Formatter, Result as FmtResult},
    iter::Peekable,
};

#[derive(Debug, Fail)]
pub enum LexerError {
    #[fail(display = "Invalid whitespace")]
    BadWhitespace,

    #[fail(display = "{:?} is too large to be stored as a number", _0)]
    IntTooBig(String),

    #[fail(display = "\"\\{}\" is not a valid escape sequence", _0)]
    InvalidEscape(char),

    #[fail(display = "{} is not a valid hex digit", _0)]
    InvalidHexEscape(char),

    #[fail(display = "{} should be written 0", _0)]
    NegativeZero(String),

    #[fail(display = "U+{:04X} is not a valid Unicode scalar value", _0)]
    NotUnicodeChar(u32),

    #[fail(display = "A string literal wasn't closed")]
    UnclosedString,

    #[fail(display = "Unexpected character {:?}", _0)]
    Unexpected(char),
}

#[derive(Debug)]
pub enum Token {
    Dedent,
    Group,
    Indent,
    Hole,
    Int(isize),
    ParenClose,
    ParenOpen,
    Pipe,
    Quote,
    String(SharedString),
    Symbol(SharedString),
}

impl Display for Token {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Token::Dedent => write!(fmt, "DEDENT"),
            Token::Group => write!(fmt, "group"),
            Token::Hole => write!(fmt, "_"),
            Token::Indent => write!(fmt, "INDENT"),
            Token::Int(n) => write!(fmt, "{}", n),
            Token::ParenClose => write!(fmt, ")"),
            Token::ParenOpen => write!(fmt, "("),
            Token::Pipe => write!(fmt, "|"),
            Token::Quote => write!(fmt, "'"),
            Token::String(s) => fmt_string(s, fmt),
            Token::Symbol(s) => write!(fmt, "{}", s),
        }
    }
}

pub struct Lexer<'src> {
    init: bool,
    iter: Peekable<PosIter<'src>>,
    last_ws: Vec<bool>,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &'src str) -> Lexer<'src> {
        Lexer {
            init: true,
            iter: PosIter { s, last: (0, 1, 1) }.peekable(),
            last_ws: Vec::new(),
        }
    }

    fn lex_hex_digit(&mut self) -> Result<u32, LexerError> {
        match self.iter.next() {
            Some((_, '0', _)) => Ok(0),
            Some((_, '1', _)) => Ok(1),
            Some((_, '2', _)) => Ok(2),
            Some((_, '3', _)) => Ok(3),
            Some((_, '4', _)) => Ok(4),
            Some((_, '5', _)) => Ok(5),
            Some((_, '6', _)) => Ok(6),
            Some((_, '7', _)) => Ok(7),
            Some((_, '8', _)) => Ok(8),
            Some((_, '9', _)) => Ok(9),
            Some((_, 'A', _)) | Some((_, 'a', _)) => Ok(10),
            Some((_, 'B', _)) | Some((_, 'b', _)) => Ok(11),
            Some((_, 'C', _)) | Some((_, 'c', _)) => Ok(12),
            Some((_, 'D', _)) | Some((_, 'd', _)) => Ok(13),
            Some((_, 'E', _)) | Some((_, 'e', _)) => Ok(14),
            Some((_, 'F', _)) | Some((_, 'f', _)) => Ok(15),
            Some((_, c, _)) => Err(LexerError::InvalidHexEscape(c)),
            None => Err(LexerError::UnclosedString),
        }
    }

    fn lex_string_escape(&mut self) -> Result<char, LexerError> {
        match self.iter.next() {
            Some((_, '"', _)) => Ok('"'),
            Some((_, '\\', _)) => Ok('\\'),
            Some((_, 'n', _)) => Ok('\n'),
            Some((_, 'r', _)) => Ok('\r'),
            Some((_, 't', _)) => Ok('\t'),
            Some((_, 'x', _)) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let n = (d0 << 4) | d1;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some((_, 'u', _)) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let d2 = self.lex_hex_digit()?;
                let d3 = self.lex_hex_digit()?;
                let n = (d0 << 12) | (d1 << 8) | (d2 << 4) | d3;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some((_, 'U', _)) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let d2 = self.lex_hex_digit()?;
                let d3 = self.lex_hex_digit()?;
                let d4 = self.lex_hex_digit()?;
                let d5 = self.lex_hex_digit()?;
                let d6 = self.lex_hex_digit()?;
                let d7 = self.lex_hex_digit()?;
                let n = (d0 << 28)
                    | (d1 << 24)
                    | (d2 << 20)
                    | (d3 << 16)
                    | (d4 << 12)
                    | (d5 << 8)
                    | (d6 << 4)
                    | d7;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some((_, c, _)) => Err(LexerError::InvalidEscape(c)),
            None => Err(LexerError::UnclosedString),
        }
    }

    fn lex_string(
        &mut self,
        start: (usize, usize, usize),
    ) -> Result<((usize, usize, usize), Token, (usize, usize, usize)), LexerError> {
        let mut s = String::new();
        let end = loop {
            match self.iter.next() {
                Some((_, '"', a)) => break a,
                Some((_, '\\', _)) => s.push(self.lex_string_escape()?),
                Some((_, c, _)) => s.push(c),
                None => return Err(LexerError::UnclosedString),
            }
        };
        Ok((start, Token::String(SharedString::from(&s)), end))
    }

    fn lex_symbolish(
        &mut self,
        start: (usize, usize, usize),
        c: char,
    ) -> Result<((usize, usize, usize), Token, (usize, usize, usize)), LexerError> {
        let mut symbolish = String::new();
        symbolish.push(c);
        let mut is_number = ('0' <= c && c <= '9') || c == '+' || c == '-';
        let mut end = start;
        loop {
            match self.iter.peek() {
                Some(&(_, c, a)) if is_symbolish(c) => {
                    is_number &= '0' <= c && c <= '9';
                    self.iter.next();
                    symbolish.push(c);
                    end = a;
                }
                Some(&(_, _, _)) => {
                    break;
                }
                None => {
                    self.iter.next();
                    break;
                }
            }
        }

        let tok = if is_number && symbolish != "+" && symbolish != "-" {
            match symbolish.parse() {
                Ok(0) if c == '-' => return Err(LexerError::NegativeZero(symbolish)),
                Ok(n) => Token::Int(n),
                Err(_) => return Err(LexerError::IntTooBig(symbolish)),
            }
        } else if symbolish == "group" {
            Token::Group
        } else {
            Token::Symbol(SharedString::from(&symbolish))
        };
        Ok((start, tok, end))
    }

    fn nl(
        &mut self,
        start_pos: (usize, usize, usize),
    ) -> Option<Result<((usize, usize, usize), Token, (usize, usize, usize)), LexerError>> {
        let mut ws = Vec::new();
        let mut end_pos = start_pos;
        loop {
            match *self.iter.peek()? {
                (_, ' ', a) => {
                    end_pos = a;
                    ws.push(false);
                    self.iter.next();
                }
                (_, '\t', a) => {
                    end_pos = a;
                    ws.push(true);
                    self.iter.next();
                }
                _ => break,
            }
        }

        if ws == self.last_ws {
            self.next()
        } else if ws.starts_with(&self.last_ws) {
            self.last_ws = ws;
            Some(Ok((start_pos, Token::Indent, end_pos)))
        } else if self.last_ws.starts_with(&ws) {
            self.last_ws = ws;
            Some(Ok((start_pos, Token::Dedent, end_pos)))
        } else {
            Some(Err(LexerError::BadWhitespace))
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<((usize, usize, usize), Token, (usize, usize, usize)), LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.init {
            self.init = false;
            self.nl((0, 1, 1))
        } else {
            loop {
                match self.iter.next()? {
                    (_, '\n', a) | (_, '\r', a) => return self.nl(a),
                    (_, ' ', _) | (_, '\t', _) => continue,
                    (_, ';', _) => loop {
                        if let (_, '\n', a) = self.iter.next()? {
                            return self.nl(a);
                        }
                    },
                    (b, '"', _) => return Some(self.lex_string(b)),
                    (b, '\'', a) => return Some(Ok((b, Token::Quote, a))),
                    (b, '(', a) => return Some(Ok((b, Token::ParenOpen, a))),
                    (b, ')', a) => return Some(Ok((b, Token::ParenClose, a))),
                    (b, '_', a) => return Some(Ok((b, Token::Hole, a))),
                    (b, '|', a) => return Some(Ok((b, Token::Pipe, a))),
                    (b, c, _) if is_symbolish(c) => return Some(self.lex_symbolish(b, c)),
                    (_, c, _) => return Some(Err(LexerError::Unexpected(c))),
                }
            }
        }
    }
}

fn is_symbolish(c: char) -> bool {
    ('0' <= c && c <= '9')
        || ('A' <= c && c <= 'Z')
        || ('a' <= c && c <= 'z')
        || c == '*'
        || c == '+'
        || c == '-'
        || c == '/'
        || c == ':'
        || c == '<'
        || c == '='
        || c == '>'
        || c == '?'
}

pub struct PosIter<'a> {
    s: &'a str,
    last: (usize, usize, usize),
}

impl Iterator for PosIter<'_> {
    type Item = ((usize, usize, usize), char, (usize, usize, usize));
    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}
