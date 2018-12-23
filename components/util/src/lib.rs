use owning_ref::ArcRef;
use std::path::Path;

/// A path whose backing storage is shared (via a reference count).
pub type SharedPath = ArcRef<Path>;

/// A string whose backing storage is shared (via a reference count).
pub type SharedString = ArcRef<str>;
