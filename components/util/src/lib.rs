use owning_ref::ArcRef;
use std::path::Path;

/// A path whose backing storage is shared (via a reference count).
pub type SharedPath = ArcRef<Path>;

/// A string whose backing storage is shared (via a reference count).
pub type SharedString = ArcRef<str>;

/// A `SharedString` literal.
#[macro_export]
macro_rules! s {
    ($e:expr) => {
        $crate::SharedString::new(std::sync::Arc::from($e))
    };
}
