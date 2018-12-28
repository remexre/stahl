use stahl_util::{fmt_string, SharedString};
use std::{
    char::from_u32,
    fmt::{Display, Formatter, Result as FmtResult},
    iter::Peekable,
    str::CharIndices,
};

#[derive(Debug, Fail)]
pub enum LexerError {
    #[fail(display = "{:?} is too large to be stored as a number", _0)]
    IntTooBig(String),

    #[fail(display = "\"\\{}\" is not a valid escape sequence", _0)]
    InvalidEscape(char),

    #[fail(display = "{} is not a valid hex digit", _0)]
    InvalidHexEscape(char),

    #[fail(display = "{} should be written 0", _0)]
    NegativeZero(String),

    #[fail(display = "U+{:02X} is not a valid Unicode scalar value", _0)]
    NotUnicodeChar(u32),

    #[fail(display = "A string literal wasn't closed")]
    UnclosedString,

    #[fail(display = "Unexpected character {:?}", _0)]
    Unexpected(char),
}

#[derive(Debug)]
pub enum Token {
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
            Token::Hole => write!(fmt, "_"),
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
    iter: Peekable<CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &'src str) -> Lexer<'src> {
        Lexer {
            iter: s.char_indices().peekable(),
        }
    }

    fn lex_hex_digit(&mut self) -> Result<u32, LexerError> {
        match self.iter.next() {
            Some((_, '0')) => Ok(0),
            Some((_, '1')) => Ok(1),
            Some((_, '2')) => Ok(2),
            Some((_, '3')) => Ok(3),
            Some((_, '4')) => Ok(4),
            Some((_, '5')) => Ok(5),
            Some((_, '6')) => Ok(6),
            Some((_, '7')) => Ok(7),
            Some((_, '8')) => Ok(8),
            Some((_, '9')) => Ok(9),
            Some((_, 'A')) | Some((_, 'a')) => Ok(10),
            Some((_, 'B')) | Some((_, 'b')) => Ok(11),
            Some((_, 'C')) | Some((_, 'c')) => Ok(12),
            Some((_, 'D')) | Some((_, 'd')) => Ok(13),
            Some((_, 'E')) | Some((_, 'e')) => Ok(14),
            Some((_, 'F')) | Some((_, 'f')) => Ok(15),
            Some((_, c)) => Err(LexerError::InvalidHexEscape(c)),
            None => Err(LexerError::UnclosedString),
        }
    }

    fn lex_string_escape(&mut self) -> Result<char, LexerError> {
        match self.iter.next() {
            Some((_, '"')) => Ok('"'),
            Some((_, '\\')) => Ok('\\'),
            Some((_, 'n')) => Ok('\n'),
            Some((_, 'r')) => Ok('\r'),
            Some((_, 't')) => Ok('\t'),
            Some((_, 'x')) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let n = (d0 << 4) | d1;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some((_, 'u')) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let d2 = self.lex_hex_digit()?;
                let d3 = self.lex_hex_digit()?;
                let n = (d0 << 12) | (d1 << 8) | (d2 << 4) | d3;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some((_, 'U')) => {
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
            Some((_, c)) => Err(LexerError::InvalidEscape(c)),
            None => Err(LexerError::UnclosedString),
        }
    }

    fn lex_string(&mut self, start: usize) -> Result<(usize, Token, usize), LexerError> {
        let mut s = String::new();
        let end = loop {
            match self.iter.next() {
                Some((i, '"')) => break i,
                Some((_, '\\')) => s.push(self.lex_string_escape()?),
                Some((_, c)) => s.push(c),
                None => return Err(LexerError::UnclosedString),
            }
        };
        Ok((start, Token::String(SharedString::from(&s)), end))
    }

    fn lex_symbolish(
        &mut self,
        start: usize,
        c: char,
    ) -> Result<(usize, Token, usize), LexerError> {
        let mut symbolish = String::new();
        symbolish.push(c);
        let mut is_number = ('0' <= c && c <= '9') || c == '+' || c == '-';
        let mut end = start;
        loop {
            match self.iter.peek() {
                Some(&(_, c)) if is_symbolish(c) => {
                    is_number &= '0' <= c && c <= '9';
                    self.iter.next();
                    symbolish.push(c);
                }
                Some(&(i, _)) => {
                    end = i;
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
        } else {
            Token::Symbol(SharedString::from(&symbolish))
        };
        Ok((start, tok, end))
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<(usize, Token, usize), LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        Some('outer: loop {
            match self.iter.next()? {
                (_, c) if is_whitespace(c) => continue,
                (_, ';') => loop {
                    if self.iter.next()?.1 == '\n' {
                        continue 'outer;
                    }
                },
                (i, '"') => break self.lex_string(i),
                (i, '\'') => break Ok((i, Token::Quote, i + 1)),
                (i, '(') => break Ok((i, Token::ParenOpen, i + 1)),
                (i, ')') => break Ok((i, Token::ParenClose, i + 1)),
                (i, '_') => break Ok((i, Token::Hole, i + 1)),
                (i, '|') => break Ok((i, Token::Pipe, i + 1)),
                (i, c) if is_symbolish(c) => break self.lex_symbolish(i, c),
                (_, c) => break Err(LexerError::Unexpected(c)),
            }
        })
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
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\n' || c == '\r' || c == '\t'
}
