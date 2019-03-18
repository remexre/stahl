use stahl_errors::{PointLC, Position};
use stahl_util::{fmt_string, SharedString};
use std::{
    char::from_u32,
    collections::VecDeque,
    fmt::{Display, Formatter, Result as FmtResult},
    str::Chars,
};

/// An error from the lexer.
#[derive(Debug, Fail)]
pub enum LexerError {
    /// Whitespace was found that left the indentation level ambiguous.
    #[fail(display = "Invalid whitespace {}", _0)]
    BadWhitespace(Position),

    /// A number was found that couldn't fit in a numeric type.
    #[fail(display = "{:?} is too large to be stored as a number", _0)]
    IntTooBig(String),

    /// An invalid escape sequence was found.
    #[fail(display = "\"\\{}\" is not a valid escape sequence", _0)]
    InvalidEscape(char),

    /// An invalid hex digit in an escape sequence was found.
    #[fail(display = "{} is not a valid hex digit", _0)]
    InvalidHexEscape(char),

    /// `-0` was found.
    #[fail(display = "{} should be written 0", _0)]
    NegativeZero(String),

    /// A unicode escape was found that doesn't map to any known Unicode scalar value.
    #[fail(display = "U+{:04X} is not a valid Unicode scalar value", _0)]
    NotUnicodeChar(u32),

    /// A string literal that had no ending quote mark.
    #[fail(display = "A string literal wasn't closed")]
    UnclosedString,

    /// An unexpected character was found.
    #[fail(display = "Unexpected character {:?}", _0)]
    Unexpected(char),
}

/// A lexical token.
#[derive(Clone, Debug, PartialEq)]
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

/// The Stahl lexer.
///
/// The lexer is a relatively straightforward iterator over
/// `Result<(PointLC, Token, PointLC), LexerError>` values, where the two points correspond to the
/// beginning and end of the token in the input string. Note that once an error is returned, it is
/// illegal to continue lexing.
///
/// ## Example
///
/// ```
/// # use stahl_errors::PointLC;
/// # use stahl_parser::{Lexer, Token};
/// # fn main() {
/// let src = "foo bar | 'baz";
/// let mut lexer = Lexer::new(src);
/// let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
/// assert_eq!(tokens, vec![
///     (PointLC(0, 1, 1), Token::Symbol("foo".into()), PointLC(3, 1, 4)),
///     (PointLC(4, 1, 5), Token::Symbol("bar".into()), PointLC(7, 1, 8)),
///     (PointLC(8, 1, 9), Token::Pipe, PointLC(9, 1, 10)),
///     (PointLC(10, 1, 11), Token::Quote, PointLC(11, 1, 12)),
///     (PointLC(11, 1, 12), Token::Symbol("baz".into()), PointLC(14, 1, 15)),
///     (PointLC(14, 1, 15), Token::Newline, PointLC(14, 1, 15)),
/// ]);
/// # }
/// ```
pub struct Lexer<'src> {
    end: bool,
    iter: Peekable<PosIter<Chars<'src>>>,
    last: PointLC,
    paren_depth: isize,
    queued: VecDeque<(PointLC, Token, PointLC)>,
    queued_nl: Option<PointLC>,
    ws: &'src str,
    ws_levels: Vec<(usize, bool)>,
}

impl<'src> Lexer<'src> {
    /// Creates a new instance of the lexer for the given string.
    ///
    /// The lexer is not yet capable of lexing streaming input, although such a change would be
    /// relatively straightforward.
    pub fn new(s: &'src str) -> Lexer<'src> {
        Lexer {
            end: false,
            iter: Peekable::new(PosIter {
                iter: s.chars(),
                pos: PointLC(0, 1, 1),
            }),
            last: PointLC(0, 1, 1),
            paren_depth: 0,
            queued: VecDeque::new(),
            queued_nl: None,
            ws: "",
            ws_levels: Vec::new(),
        }
    }

    fn eat_whitespace(&mut self) -> &'src str {
        let s = self.iter.iter().iter.as_str();
        let pos = s.find(|c| c != ' ' && c != '\t').unwrap_or_else(|| s.len());
        if pos > 0 {
            for _ in 0..(pos - 1) {
                assert!(self.iter.next().is_some());
            }
            self.last = self.iter.next().unwrap().1;
        }
        &s[..pos]
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
        if ws == "" {
            while let Some((_, emit)) = self.ws_levels.pop() {
                if emit {
                    self.queued.push_back((after_nl, Token::Dedent, self.last));
                }
            }
            self.ws = ws;
            self.next()
        } else if ws.starts_with(self.ws) {
            if ws.len() == self.ws.len() {
                self.next()
            } else {
                let emit = self.paren_depth <= 0;
                self.ws_levels.push((self.ws.len(), emit));
                self.ws = ws;
                if emit {
                    self.queued.push_back((after_nl, Token::Indent, self.last));
                }
                self.next()
            }
        } else if self.ws.starts_with(ws) {
            self.ws = ws;
            let mut trunc = None;
            for (i, (stop, emit)) in self.ws_levels.iter().cloned().enumerate() {
                if ws.len() == stop {
                    trunc = Some(i);
                }
                if emit && trunc.is_some() {
                    self.queued.push_back((after_nl, Token::Dedent, self.last));
                }
            }
            if let Some(n) = trunc {
                self.ws_levels.drain(n..);
                self.next()
            } else {
                Some(Err(LexerError::BadWhitespace(Position::SpanLC(
                    after_nl, self.last,
                ))))
            }
        } else {
            Some(Err(LexerError::BadWhitespace(Position::SpanLC(
                after_nl, self.last,
            ))))
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<(PointLC, Token, PointLC), LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = if let Some(tok) = self.queued.pop_front() {
            Some(Ok(tok))
        } else if let Some(a) = self.queued_nl.take() {
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
                            Some(('\n', a)) => {
                                self.queued_nl = Some(a);
                                break 'outer Some(Ok((self.last, Token::Newline, a)));
                            }
                            Some(_) => {}
                            None => break 'outer None,
                        }
                    },
                    Some(('"', _)) => break Some(self.lex_string(self.last)),
                    Some(('\'', a)) => break Some(Ok((self.last, Token::Quote, a))),
                    Some(('(', a)) => {
                        self.paren_depth += 1;
                        break Some(Ok((self.last, Token::ParenOpen, a)));
                    }
                    Some((')', a)) => {
                        self.paren_depth -= 1;
                        break Some(Ok((self.last, Token::ParenClose, a)));
                    }
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
                self.end = v == Token::Newline;
                self.last = a;
                Some(Ok((b, v, a)))
            }
            Some(Err(e)) => Some(Err(e)),
            None => {
                if !self.end {
                    self.end = true;
                    Some(Ok((self.last, Token::Newline, self.last)))
                } else {
                    while let Some((_, emit)) = self.ws_levels.pop() {
                        if emit {
                            return Some(Ok((self.last, Token::Dedent, self.last)));
                        }
                    }
                    None
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
