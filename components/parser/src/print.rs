use crate::Value;
use stahl_util::fmt_string;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

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
            Value::Symbol(_, s) => write!(fmt, "{}", s.as_ref()),
            Value::Nil(_) => write!(fmt, "()"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        Debug::fmt(self, fmt)
    }
}
