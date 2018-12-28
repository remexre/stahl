extern crate failure;

use failure::{err_msg, Error as FailureError};
use stahl_util::SharedPath;
use std::fmt::{Display, Formatter, Result as FmtResult};

/// The equivalent of `assert!`, but using `raise!` instead of `panic!`.
#[macro_export]
macro_rules! ensure {
    ($c:expr) => {
        if !$c {
            raise!(concat!("Ensurance failed: ", stringify!($c)))
        }
    };
    ($c:expr, @$l:expr) => {
        if !$c {
            raise!(@$l, concat!("Ensurance failed: ", stringify!($c)))
        }
    };
    ($c:expr, $($e:expr),+) => {
        if !$c {
            raise!($($e),+)
        }
    };
    ($c:expr, @$l:expr, $($e:expr),+) => {
        if !$c {
            raise!(@$l, $($e),+)
        }
    };
}

/// The equivalent of `assert_eq!`, but using `raise!` instead of `panic!`.
#[macro_export]
macro_rules! ensure_eq {
    ($a:expr, $b:expr) => { ensure!($a == $b) };
    ($a:expr, $b:expr, $($tt:tt)*) => { ensure!($a == $b, $($tt)*) };
}

/// The equivalent of `assert_ne!`, but using `raise!` instead of `panic!`.
#[macro_export]
macro_rules! ensure_ne {
    ($a:expr, $b:expr) => { ensure!($a != $b) };
    ($a:expr, $b:expr, $($tt:tt)*) => { ensure!($a != $b, $($tt)*) };
}

/// Evaluates to an `Error` constructed from a formatting expression.
#[macro_export]
macro_rules! err {
    ($($e:expr),+) => {
        $crate::Error::new_basic(format!($($e),+), $crate::Location::default())
    };
    (@$l:expr, $($e:expr),+) => {
        $crate::Error::new_basic(format!($($e),+), $l)
    };
}

/// Returns an `Error` constructed from a formatting expression.
#[macro_export]
macro_rules! raise {
    ($($e:expr),+) => {
        return std::result::Result::Err(err!($($e),+));
    };
    (@$l:expr, $($e:expr),+) => {
        return std::result::Result::Err(err!(@$l, $($e),+));
    };
}

/// An equivalent of `unimplemented!()` that returns an `Error` instead.
#[macro_export]
macro_rules! todo {
    () => {
        raise!("TODO at {}:{}", file!(), line!())
    };
    (@$l:expr) => {
        raise!(@$l, "TODO at {}:{}", file!(), line!())
    };
    ($f:expr, $($a:expr),*) => {
        raise!(concat!("TODO at {}:{}, ", $f), file!(), line!(), $($a),*)
    };
    (@$l:expr, $f:expr, $($a:expr),*) => {
        raise!(@$l, concat!("TODO at {}:{}, ", $f), file!(), line!(), $($a),*)
    };
}

/// A useful alias for Result with this crate's Error type.
pub type Result<T> = std::result::Result<T, Error>;

/// The common error wrapper, which allows attaching a location or span.
#[derive(Debug)]
pub struct Error {
    /// The cause of the error, if any.
    pub cause: Option<Box<Error>>,

    /// The error.
    pub err: FailureError,

    /// The location, if any is known.
    pub loc: Location,
}

impl Error {
    /// Creates a new error with no location information from a string. Intended for the `raise!()`
    /// macro, mainly to avoid having to export `err_msg`.
    #[doc(hidden)]
    pub fn new_basic(s: String, loc: Location) -> Error {
        Error {
            cause: None,
            err: err_msg(s),
            loc,
        }
    }

    /// Creates an error in the given file.
    pub fn new_file<T: Into<FailureError>>(t: T, path: Option<SharedPath>) -> Error {
        Error {
            cause: None,
            err: t.into(),
            loc: Location::new_file(path),
        }
    }

    /// Creates an error at a certain point in the given file.
    pub fn new_point<T: Into<FailureError>>(t: T, path: Option<SharedPath>, point: usize) -> Error {
        Error {
            cause: None,
            err: t.into(),
            loc: Location::new_point(path, point),
        }
    }

    /// Creates an error at a span in the given file.
    pub fn new_span<T: Into<FailureError>>(
        t: T,
        path: Option<SharedPath>,
        start: usize,
        end: usize,
    ) -> Error {
        Error {
            cause: None,
            err: t.into(),
            loc: Location::new_span(path, start, end),
        }
    }

    /// Chains an error onto this one. The resulting error will have this error as a cause.
    pub fn chain(self, other: Error) -> Error {
        Error {
            cause: Some(Box::new(self)),
            err: other.err,
            loc: other.loc,
        }
    }
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let mut err = Some(self);
        let mut first = true;
        while let Some(e) = err {
            if first {
                first = false
            } else {
                write!(fmt, "\n        ")?;
            }
            write!(fmt, "{}: {}", e.loc, e.err)?;
            err = e.cause.as_ref().map(|e| &**e);
        }
        Ok(())
    }
}

impl<T> From<T> for Error
where
    T: Into<FailureError>,
{
    fn from(t: T) -> Error {
        Error {
            cause: None,
            err: t.into(),
            loc: Location::default(),
        }
    }
}

/// The location at which an error occurred.
#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Location {
    /// The file where the error occurred.
    pub path: Option<SharedPath>,

    /// The position in the file where the error occurred.
    pub pos: Option<Position>,
}

impl Location {
    /// Creates an error in the given file.
    pub fn new_file(path: Option<SharedPath>) -> Location {
        Location { path, pos: None }
    }

    /// Creates an error at a certain point in the given file.
    pub fn new_point(path: Option<SharedPath>, point: usize) -> Location {
        Location {
            path,
            pos: Some(Position::Point(point)),
        }
    }

    /// Creates an error at a span in the given file.
    pub fn new_span(path: Option<SharedPath>, start: usize, end: usize) -> Location {
        Location {
            path,
            pos: Some(Position::Span(start, end)),
        }
    }
}

impl Display for Location {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match (self.path.as_ref(), self.pos) {
            (Some(path), Some(pos)) => write!(fmt, "In {}, at {}", path.display(), pos),
            (Some(path), None) => write!(fmt, "In {}", path.display()),
            (None, Some(pos)) => write!(fmt, "At {}", pos),
            (None, None) => write!(fmt, "At an unknown location"),
        }
    }
}

impl From<Position> for Location {
    fn from(pos: Position) -> Location {
        Location {
            path: None,
            pos: Some(pos),
        }
    }
}

/// The point or span at which an error occurred.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Position {
    /// A single point in a file.
    Point(usize),

    /// A span in a file.
    Span(usize, usize),
}

impl Display for Position {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Position::Point(point) => write!(fmt, "byte {}", point),
            Position::Span(start, end) => write!(fmt, "bytes {}-{}", start, end),
        }
    }
}

/// An extension trait for `Result`s.
pub trait ResultExt {
    /// Chains an error onto the error present, if any. The resulting error will have this error as
    /// a cause.
    fn chain(self, other: impl FnOnce() -> Error) -> Self;
}

impl<T> ResultExt for Result<T> {
    fn chain(self, other: impl FnOnce() -> Error) -> Self {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(e.chain(other())),
        }
    }
}
