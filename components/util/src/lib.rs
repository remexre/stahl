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
