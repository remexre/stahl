extern crate failure;

use failure::{err_msg, Error as FailureError};
use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    path::PathBuf,
};

/// Returns an Error constructed from a formatting expression.
#[macro_export]
macro_rules! raise {
    ($($e:expr),*) => {
        return std::result::Result::Err($crate::Error::new_basic(format!($($e),*)))
    };
}

/// A useful alias for Result with this crate's Error type.
pub type Result<T> = std::result::Result<T, Error>;

/// The common error wrapper, which allows attaching a location or span.
#[derive(Debug)]
pub struct Error {
    /// The error.
    pub err: FailureError,

    /// The location, if any is known.
    pub loc: Option<Location>,

    /// The path of the file where the error occurred, if any is known.
    pub path: Option<PathBuf>,
}

impl Error {
    /// Creates a new error with no location information from a string. Intended for the `raise!()`
    /// macro, mainly to avoid having to export `err_msg`.
    #[doc(hidden)]
    pub fn new_basic(s: String) -> Error {
        Error {
            err: err_msg(s),
            loc: None,
            path: None,
        }
    }

    /// Creates an error in the given file.
    pub fn new_file<T: Into<FailureError>>(t: T, path: Option<PathBuf>) -> Error {
        Error {
            err: t.into(),
            loc: None,
            path,
        }
    }

    /// Creates an error at a certain point in the given file.
    pub fn new_point<T: Into<FailureError>>(t: T, path: Option<PathBuf>, point: usize) -> Error {
        Error {
            err: t.into(),
            loc: Some(Location::Point(point)),
            path,
        }
    }

    /// Creates an error at a span in the given file.
    pub fn new_span<T: Into<FailureError>>(
        t: T,
        path: Option<PathBuf>,
        start: usize,
        end: usize,
    ) -> Error {
        Error {
            err: t.into(),
            loc: Some(Location::Span(start, end)),
            path,
        }
    }
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self.loc {
            Some(ref loc) => write!(fmt, "{}: {}", loc, self.err),
            None => self.err.fmt(fmt),
        }
    }
}

impl<T> From<T> for Error
where
    T: Into<FailureError>,
{
    fn from(t: T) -> Error {
        Error {
            err: t.into(),
            loc: None,
            path: None,
        }
    }
}

/// The location or span at which an error occurred.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Location {
    /// A single point in a file.
    Point(usize),

    /// A span in a file.
    Span(usize, usize),
}

impl Display for Location {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Location::Point(point) => write!(fmt, "Byte {}", point),
            Location::Span(start, end) => write!(fmt, "Bytes {}-{}", start, end),
        }
    }
}
