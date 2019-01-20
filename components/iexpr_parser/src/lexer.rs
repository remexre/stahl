use stahl_errors::PointLC;
use stahl_util::{fmt_string, SharedString};
use std::{
    char::from_u32,
    fmt::{Display, Formatter, Result as FmtResult},
    str::Chars,
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

#[derive(Debug, PartialEq)]
pub enum Token {
    Dedent,
    Group,
    Indent,
    Hole,
    Int(isize),
    Newline,
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
            Token::Newline => write!(fmt, "NEWLINE"),
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
    end: bool,
    indented: usize,
    iter: Peekable<PosIter<Chars<'src>>>,
    last: PointLC,
    last_ws: &'src str,
    queued_nl: Option<PointLC>,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &'src str) -> Lexer<'src> {
        let mut l = Lexer {
            end: false,
            indented: 0,
            iter: Peekable::new(PosIter {
                iter: s.chars(),
                pos: PointLC(0, 1, 1),
            }),
            last: PointLC(0, 1, 1),
            last_ws: "",
            queued_nl: None,
        };
        l.last_ws = l.eat_whitespace();
        l
    }

    fn eat_whitespace(&mut self) -> &'src str {
        let s = self.iter.iter().iter.as_str();
        let pos = s.find(|c| c != ' ' && c != '\t').unwrap_or_else(|| s.len());
        for _ in 0..pos {
            assert!(self.iter.next().is_some());
        }
        dbg!(&s[..pos])
    }

    fn lex_hex_digit(&mut self) -> Result<u32, LexerError> {
        match self.iter.next() {
            Some(('0', _)) => Ok(0),
            Some(('1', _)) => Ok(1),
            Some(('2', _)) => Ok(2),
            Some(('3', _)) => Ok(3),
            Some(('4', _)) => Ok(4),
            Some(('5', _)) => Ok(5),
            Some(('6', _)) => Ok(6),
            Some(('7', _)) => Ok(7),
            Some(('8', _)) => Ok(8),
            Some(('9', _)) => Ok(9),
            Some(('A', _)) | Some(('a', _)) => Ok(10),
            Some(('B', _)) | Some(('b', _)) => Ok(11),
            Some(('C', _)) | Some(('c', _)) => Ok(12),
            Some(('D', _)) | Some(('d', _)) => Ok(13),
            Some(('E', _)) | Some(('e', _)) => Ok(14),
            Some(('F', _)) | Some(('f', _)) => Ok(15),
            Some((c, _)) => Err(LexerError::InvalidHexEscape(c)),
            None => Err(LexerError::UnclosedString),
        }
    }

    fn lex_string_escape(&mut self) -> Result<char, LexerError> {
        match self.iter.next() {
            Some(('"', _)) => Ok('"'),
            Some(('\\', _)) => Ok('\\'),
            Some(('n', _)) => Ok('\n'),
            Some(('r', _)) => Ok('\r'),
            Some(('t', _)) => Ok('\t'),
            Some(('x', _)) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let n = (d0 << 4) | d1;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some(('u', _)) => {
                let d0 = self.lex_hex_digit()?;
                let d1 = self.lex_hex_digit()?;
                let d2 = self.lex_hex_digit()?;
                let d3 = self.lex_hex_digit()?;
                let n = (d0 << 12) | (d1 << 8) | (d2 << 4) | d3;
                from_u32(n).ok_or_else(|| LexerError::NotUnicodeChar(n))
            }
            Some(('U', _)) => {
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
            Some((c, _)) => Err(LexerError::InvalidEscape(c)),
            None => Err(LexerError::UnclosedString),
        }
    }

    fn lex_string(&mut self, start: PointLC) -> Result<(PointLC, Token, PointLC), LexerError> {
        let mut s = String::new();
        let end = loop {
            match self.iter.next() {
                Some(('"', a)) => break a,
                Some(('\\', _)) => s.push(self.lex_string_escape()?),
                Some((c, _)) => s.push(c),
                None => return Err(LexerError::UnclosedString),
            }
        };
        Ok((start, Token::String(SharedString::from(&s)), end))
    }

    fn lex_symbolish(
        &mut self,
        start: PointLC,
        c: char,
        mut end: PointLC,
    ) -> Result<(PointLC, Token, PointLC), LexerError> {
        let mut symbolish = String::new();
        symbolish.push(c);
        let mut is_number = ('0' <= c && c <= '9') || c == '+' || c == '-';
        loop {
            match self.iter.peek() {
                Some(&(c, a)) if is_symbolish(c) => {
                    is_number &= '0' <= c && c <= '9';
                    self.iter.next();
                    symbolish.push(c);
                    end = a;
                }
                Some(&(_, _)) => {
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

    fn on_nl(
        &mut self,
        after_nl: PointLC,
    ) -> Option<Result<(PointLC, Token, PointLC), LexerError>> {
        let ws = self.eat_whitespace();
        if ws == self.last_ws {
            unimplemented!("ws = {:?}", ws)
        } else if ws.starts_with(self.last_ws) {
            unimplemented!("ws = {:?}", ws)
        } else if self.last_ws.starts_with(ws) {
            unimplemented!("ws = {:?}", ws)
        } else {
            Some(Err(LexerError::BadWhitespace))
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<(PointLC, Token, PointLC), LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = if let Some(a) = self.queued_nl {
            self.on_nl(a)
        } else {
            'outer: loop {
                match self.iter.next() {
                    Some(('\n', a)) => {
                        self.queued_nl = Some(a);
                        break Some(Ok((self.last, Token::Newline, a)));
                    }
                    Some((' ', a)) | Some(('\t', a)) => {
                        self.last = a;
                    }
                    Some((';', _)) => loop {
                        match self.iter.next() {
                            Some(('\n', a)) => break 'outer self.on_nl(a),
                            Some(_) => {}
                            None => break 'outer None,
                        }
                    },
                    Some(('"', _)) => break Some(self.lex_string(self.last)),
                    Some(('\'', a)) => break Some(Ok((self.last, Token::Quote, a))),
                    Some(('(', a)) => break Some(Ok((self.last, Token::ParenOpen, a))),
                    Some((')', a)) => break Some(Ok((self.last, Token::ParenClose, a))),
                    Some(('_', a)) => break Some(Ok((self.last, Token::Hole, a))),
                    Some(('|', a)) => break Some(Ok((self.last, Token::Pipe, a))),
                    Some((c, a)) if is_symbolish(c) => {
                        break Some(self.lex_symbolish(self.last, c, a));
                    }
                    Some((c, _)) => break Some(Err(LexerError::Unexpected(c))),
                    None => break None,
                }
            }
        };
        match res {
            Some(Ok((b, v, a))) => {
                if v == Token::Newline {
                    self.end = true;
                }

                self.last = a;
                Some(Ok((b, v, a)))
            }
            None if self.indented > 0 => {
                self.indented -= 1;
                self.end = false;
                Some(Ok((self.last, Token::Dedent, self.last)))
            }
            None if !self.end => {
                self.end = true;
                Some(Ok((self.last, Token::Newline, self.last)))
            }
            r => r,
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

pub struct Peekable<I: Iterator> {
    iter: I,
    peeked: Option<I::Item>,
}

impl<I: Iterator> Peekable<I> {
    fn new(iter: I) -> Peekable<I> {
        Peekable { iter, peeked: None }
    }

    fn iter(&mut self) -> &mut I {
        assert!(self.peeked.is_none());
        &mut self.iter
    }

    fn peek(&mut self) -> Option<&I::Item> {
        if self.peeked.is_none() {
            self.peeked = self.iter.next();
        }
        self.peeked.as_ref()
    }
}

impl<I: Iterator> Iterator for Peekable<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<I::Item> {
        self.peeked.take().or_else(|| self.iter.next())
    }
}

pub struct PosIter<I> {
    iter: I,
    pos: PointLC,
}

impl<I: Iterator<Item = char>> Iterator for PosIter<I> {
    type Item = (char, PointLC);
    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.iter.next()?;
        self.pos.advance(ch);
        Some((ch, self.pos))
    }
}
