use owning_ref::ArcRef;
use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    path::Path,
};

/// A `SharedString` literal.
#[macro_export]
macro_rules! s {
    ($e:expr) => {
        $crate::SharedString::new(std::sync::Arc::from($e))
    };
}

/// A path whose backing storage is shared (via a reference count).
pub type SharedPath = ArcRef<Path>;

/// A string whose backing storage is shared (via a reference count).
pub type SharedString = ArcRef<str>;

/// Formats a list of space-separated items.
pub fn fmt_iter<I, T>(fmt: &mut Formatter, iter: I) -> FmtResult
where
    I: IntoIterator<Item = T>,
    T: Display,
{
    let mut first = true;
    for val in iter {
        if first {
            first = false;
        } else {
            fmt.write_str(" ")?;
        }
        Display::fmt(&val, fmt)?;
    }
    Ok(())
}

/// Formats a string as per Stahl's escaping rules.
pub fn fmt_string(s: &str, fmt: &mut Formatter) -> FmtResult {
    write!(fmt, "\"")?;
    for c in s.chars() {
        fmt_char(c, fmt)?;
    }
    write!(fmt, "\"")
}

/// Formats a string character as per Stahl's escaping rules.
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
