//! Some utilities used throughout the Stahl compiler.
#![deny(missing_docs)]

mod split_vec;

pub use crate::split_vec::SplitVec;
use std::{
    borrow::Borrow,
    ffi::OsStr,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    rc::Rc,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

/// Returns a Display for printing a `Vec`.
pub fn display_vec<T, U>(vec: T) -> impl Display
where
    T: AsRef<[U]>,
    U: Display,
{
    struct DisplayVec<T: AsRef<[U]>, U: Display>(T, PhantomData<U>);

    impl<T: AsRef<[U]>, U: Display> Display for DisplayVec<T, U> {
        fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
            write!(fmt, "[")?;
            let mut first = true;
            let slice = self.0.as_ref();
            for x in slice {
                if first {
                    first = false;
                } else {
                    write!(fmt, ", ")?;
                }
                write!(fmt, "{}", x)?;
            }
            write!(fmt, "]")
        }
    }

    DisplayVec(vec, PhantomData)
}

/// A path whose backing storage is shared (via a reference count).
#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SharedPath(Arc<Path>);

impl AsRef<OsStr> for SharedPath {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref().as_ref()
    }
}

impl AsRef<Path> for SharedPath {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl Deref for SharedPath {
    type Target = Path;
    fn deref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl Debug for SharedPath {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{:?}", self.0.as_ref())
    }
}

impl Display for SharedPath {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        Display::fmt(&self.0.as_ref().display(), fmt)
    }
}

impl From<PathBuf> for SharedPath {
    fn from(path: PathBuf) -> SharedPath {
        SharedPath(Arc::from(path.as_ref()))
    }
}

/// A string whose backing storage is shared (via a reference count).
#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SharedString(Arc<str>, bool);

impl SharedString {
    /// Returns the string being shared.
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }

    /// Creates a string based on `genint`.
    pub fn gensym() -> SharedString {
        format!("#G:{}#", genint()).into()
    }

    /// Creates a string based on `genint` that returns true for `is_anon`.
    pub fn gensym_anon() -> SharedString {
        let mut s = SharedString::gensym();
        s.1 = true;
        s
    }

    /// Returns whether the string is the result of instantiating an anonymous argument.
    pub fn is_anon(&self) -> bool {
        self.1
    }
}

impl AsRef<OsStr> for SharedString {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref().as_ref()
    }
}

impl AsRef<Path> for SharedString {
    fn as_ref(&self) -> &Path {
        self.0.as_ref().as_ref()
    }
}

impl AsRef<str> for SharedString {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Borrow<str> for SharedString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Deref for SharedString {
    type Target = str;
    fn deref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Debug for SharedString {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{:?}", &**self)
    }
}

impl Display for SharedString {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        fmt.write_str(self.as_str())
    }
}

impl From<&str> for SharedString {
    fn from(s: &str) -> SharedString {
        SharedString(Arc::from(s), false)
    }
}

impl From<&String> for SharedString {
    fn from(s: &String) -> SharedString {
        SharedString::from(s as &str)
    }
}

impl From<String> for SharedString {
    fn from(s: String) -> SharedString {
        SharedString(Arc::from(s), false)
    }
}

impl PartialEq<SharedString> for str {
    fn eq(&self, other: &SharedString) -> bool {
        *self == **other
    }
}

impl PartialEq<SharedString> for &str {
    fn eq(&self, other: &SharedString) -> bool {
        **self == **other
    }
}

impl PartialEq<str> for SharedString {
    fn eq(&self, other: &str) -> bool {
        **self == *other
    }
}

impl PartialEq<&str> for SharedString {
    fn eq(&self, other: &&str) -> bool {
        **self == **other
    }
}

impl PartialEq<&SharedString> for SharedString {
    fn eq(&self, other: &&SharedString) -> bool {
        **self == ***other
    }
}

/// A wrapper for values which can be taken out.
#[derive(Debug)]
pub struct Taker<T>(Option<T>);

impl<T> Taker<T> {
    /// Takes the value out of the Taker.
    pub fn take(&mut self) -> T {
        self.0.take().unwrap()
    }

    /// Returns whether the value has been taken yet.
    pub fn taken(&self) -> bool {
        self.0.is_none()
    }
}

impl<T> From<T> for Taker<T> {
    fn from(t: T) -> Taker<T> {
        Taker(Some(t))
    }
}

impl<T> Deref for Taker<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.as_ref().unwrap()
    }
}

impl<T> DerefMut for Taker<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.as_mut().unwrap()
    }
}

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

/// Generates a unique integer.
pub fn genint() -> usize {
    static N: AtomicUsize = AtomicUsize::new(0);
    N.fetch_add(1, Ordering::SeqCst)
}

/// Takes a value from an Arc if the reference is unique, otherwise clones it out.
pub fn unwrap_arc<T: Clone>(arc: Arc<T>) -> T {
    Arc::try_unwrap(arc).unwrap_or_else(|arc| (*arc).clone())
}

/// Takes a value from an Rc if the reference is unique, otherwise clones it out.
pub fn unwrap_rc<T: Clone>(rc: Rc<T>) -> T {
    // TODO: Have some sort of functionality for determinining if this is ever actually able to
    // unwrap the Rc, or if it's always cloning. Serialize backtraces to a file?
    Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
}

/// Unzips an iterator that returns its pairs inside a `Result`.
pub fn unzip_result_iter<CT, CU, E, II, T, U>(iter: II) -> Result<(CT, CU), E>
where
    CT: Default + Extend<T>,
    CU: Default + Extend<U>,
    II: IntoIterator<Item = Result<(T, U), E>>,
{
    let mut ct = CT::default();
    let mut cu = CU::default();
    iter.into_iter().try_for_each(|r| {
        let (t, u) = r?;
        ct.extend(Some(t));
        cu.extend(Some(u));
        Ok(())
    })?;
    Ok((ct, cu))
}
