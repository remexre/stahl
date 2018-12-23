use crate::Value;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

pub fn fmt_string(s: &str, fmt: &mut Formatter) -> FmtResult {
    write!(fmt, "\"")?;
    for c in s.chars() {
        fmt_char(c, fmt)?;
    }
    write!(fmt, "\"")
}

pub fn fmt_char(c: char, fmt: &mut Formatter) -> FmtResult {
    let n = c as u32;
    if c == '"' || c == '\\' {
        write!(fmt, "\\{}", c)
    } else if ' ' <= c && c <= '~' {
        write!(fmt, "{}", c)
    } else if c == '\n' {
        write!(fmt, "\\n")
    } else if c == '\r' {
        write!(fmt, "\\r")
    } else if c == '\t' {
        write!(fmt, "\\t")
    } else if n <= 0xff {
        write!(fmt, "\\x{:02x}", n)
    } else if n <= 0xffff {
        write!(fmt, "\\u{:04x}", n)
    } else {
        write!(fmt, "\\U{:08x}", n)
    }
}

impl Value {
    fn fmt_tail(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Value::Cons(_, h, t) => {
                write!(fmt, " {}", h)?;
                t.fmt_tail(fmt)
            }
            Value::Nil(_) => write!(fmt, ")"),
            v => write!(fmt, " | {})", v),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Value::Cons(_, h, t) => {
                write!(fmt, "({}", h)?;
                t.fmt_tail(fmt)
            }
            Value::Int(_, n) => write!(fmt, "{}", n),
            Value::String(_, s) => fmt_string(s, fmt),
            Value::Symbol(_, s) => write!(fmt, "{}", s),
            Value::Nil(_) => write!(fmt, "()"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        Debug::fmt(self, fmt)
    }
}
