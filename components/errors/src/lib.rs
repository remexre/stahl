//! A common error type for Stahl.
//!
//! This may be generic enough to split out for more errors; just about whenever Location is
//! relevant, this is probably the error type to use.
#![deny(missing_docs)]

use failure::{err_msg, Error as FailureError};
use log::warn;
use stahl_util::{SharedPath, SharedString};
use std::fmt::{Display, Formatter, Result as FmtResult};

/// The equivalent of `assert!`, but using `raise!` instead of `panic!`.
#[macro_export]
macro_rules! ensure {
    ($c:expr) => {
        if !$c {
            $crate::raise!(concat!("Ensurance failed: ", stringify!($c)))
        }
    };
    ($c:expr, @$l:expr) => {
        if !$c {
            $crate::raise!(@$l, concat!("Ensurance failed: ", stringify!($c)))
        }
    };
    ($c:expr, $($e:expr),+) => {
        if !$c {
            $crate::raise!($($e),+)
        }
    };
    ($c:expr, @$l:expr, $($e:expr),+) => {
        if !$c {
            $crate::raise!(@$l, $($e),+)
        }
    };
}

/// The equivalent of `assert_eq!`, but using `raise!` instead of `panic!`.
#[macro_export]
macro_rules! ensure_eq {
    ($a:expr, $b:expr) => { $crate::ensure!($a == $b) };
    ($a:expr, $b:expr, $($tt:tt)*) => { $crate::ensure!($a == $b, $($tt)*) };
}

/// The equivalent of `assert_ne!`, but using `raise!` instead of `panic!`.
#[macro_export]
macro_rules! ensure_ne {
    ($a:expr, $b:expr) => { $crate::ensure!($a != $b) };
    ($a:expr, $b:expr, $($tt:tt)*) => { $crate::ensure!($a != $b, $($tt)*) };
}

/// Evaluates to an `Error` constructed from a formatting expression.
#[macro_export]
macro_rules! err {
    ($($e:expr),+) => {
        // TODO: Warn about this.
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
        return std::result::Result::Err($crate::err!($($e),+));
    };
    (@$l:expr, $($e:expr),+) => {
        return std::result::Result::Err($crate::err!(@$l, $($e),+));
    };
}

/// An equivalent of `unimplemented!()` that returns an `Error` instead.
#[macro_export]
macro_rules! todo {
    () => {
        $crate::raise!("TODO at {}:{}", file!(), line!())
    };
    (@$l:expr) => {
        $crate::raise!(@$l, "TODO at {}:{}", file!(), line!())
    };
    ($f:expr $(, $a:expr)* $(,)*) => {
        $crate::raise!(concat!("TODO at {}:{}, ", $f), file!(), line!(), $($a),*)
    };
    (@$l:expr, $f:expr $(, $a:expr)* $(,)*) => {
        $crate::raise!(@$l, concat!("TODO at {}:{}, ", $f), file!(), line!() $(, $a)*)
    };
}

/// A useful alias for Result with this crate's Error type.
pub type Result<T> = std::result::Result<T, Error>;

/// The common error wrapper, which allows attaching a location and/or a cause.
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
        Error::new(err_msg(s), loc)
    }

    /// Creates a new error with no cause.
    pub fn new(err: impl Into<FailureError>, loc: impl Into<Location>) -> Error {
        Error {
            cause: None,
            err: err.into(),
            loc: loc.into(),
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
                write!(fmt, "\nCAUSE - ")?;
            }
            if e.loc.is_empty() {
                // TODO: Suitably chastise the programmer, ideally with line numbers.
            } else {
                write!(fmt, "{}: ", e.loc)?;
            }
            write!(fmt, "{}", e.err)?;
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
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Location {
    /// The name of the location.
    pub name: Option<SharedString>,

    /// The file where the error occurred.
    pub path: Option<SharedPath>,

    /// The position in the file where the error occurred.
    pub pos: Option<Position>,
}

impl Location {
    /// Creates an "empty" location.
    pub fn new() -> Location {
        Location::default()
    }

    /// Returns whether the location is entirely devoid of information.
    pub fn is_empty(&self) -> bool {
        self.name == None && self.path == None && self.pos == None
    }

    /// Adds a name to the location.
    pub fn name(mut self, name: SharedString) -> Location {
        self.name = Some(name);
        self
    }

    /// Adds a file path to the location.
    pub fn path(mut self, path: SharedPath) -> Location {
        if self.path.is_some() {
            warn!(
                "Adding path {} to location that already has one: {}",
                path.display(),
                self
            );
        }
        self.path = Some(path);
        self
    }

    /// Adds a file path to the location, if it is present.
    pub fn path_opt(self, path: Option<SharedPath>) -> Location {
        if let Some(path) = path {
            self.path(path)
        } else {
            self
        }
    }

    /// Adds a position to the location.
    pub fn position(mut self, pos: Position) -> Location {
        if self.pos.is_some() {
            warn!(
                "Adding position {} to location that already has one: {}",
                pos, self
            );
        }
        self.pos = Some(pos);
        self
    }

    /// Adds the given point to the location as a position.
    pub fn point(self, point: usize) -> Location {
        self.position(Position::Point(point))
    }

    /// Adds the given line-column point to the location as a position.
    pub fn point_lc(self, point: PointLC) -> Location {
        self.position(Position::PointLC(point))
    }

    /// Adds the given span to the location as a position.
    pub fn span(self, start: usize, end: usize) -> Location {
        self.position(Position::Span(start, end))
    }

    /// Adds the given line-column span to the location as a position.
    pub fn span_lc(self, start: PointLC, end: PointLC) -> Location {
        self.position(Position::SpanLC(start, end))
    }
}

impl Display for Location {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match (self.name.as_ref(), self.path.as_ref(), self.pos) {
            (None, Some(path), Some(pos)) => write!(fmt, "in {}, {}", path.display(), pos),
            (None, Some(path), None) => write!(fmt, "in {}", path.display()),
            (None, None, Some(pos)) => write!(fmt, "{}", pos),
            (None, None, None) => {
                // TODO: Suitably chastise the programmer, ideally with line numbers.
                write!(fmt, "at an unknown location")
            }
            (Some(name), Some(path), Some(pos)) => {
                write!(fmt, "in {} (in {}, {})", name, path.display(), pos)
            }
            (Some(name), Some(path), None) => write!(fmt, "in {} (in {})", name, path.display()),
            (Some(name), None, Some(pos)) => write!(fmt, "in {} ({})", name, pos),
            (Some(name), None, None) => write!(fmt, "in {}", name),
        }
    }
}

impl From<Position> for Location {
    fn from(pos: Position) -> Location {
        Location::new().position(pos)
    }
}

/// The point or span at which an error occurred.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Position {
    /// A single point in a file.
    Point(usize),

    /// A point in the file, along with line and column information.
    PointLC(PointLC),

    /// A span in a file.
    Span(usize, usize),

    /// A span in a file, along with line and column information.
    SpanLC(PointLC, PointLC),
}

impl Display for Position {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            Position::Point(point) => write!(fmt, "at byte {}", point),
            Position::PointLC(point) => write!(fmt, "at {}", point),
            Position::Span(start, end) => write!(fmt, "at bytes {}-{}", start, end),
            Position::SpanLC(s, e) => write!(fmt, "from {} to {}", s, e),
        }
    }
}

/// A point in source code. A `(byte, line, column)` triple.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PointLC(pub usize, pub usize, pub usize);

impl PointLC {
    /// Advances the point by the given character.
    pub fn advance(&mut self, ch: char) {
        self.0 += ch.len_utf8();
        match ch {
            '\n' => {
                self.1 += 1;
                self.2 = 1;
            }
            _ => {
                self.2 += 1;
            }
        }
    }
}

impl Default for PointLC {
    fn default() -> PointLC {
        PointLC(0, 1, 1)
    }
}

impl Display for PointLC {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "line {}, column {}", self.1, self.2)
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
