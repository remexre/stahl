use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    mem::{forget, uninitialized},
    ptr::{swap, write},
};

/// A vector which is split at a given index. Effectively, this is a `Vec` with a single
/// uninitialized position, which can be reunified to yield the original `Vec` without memory
/// allocation.
pub struct SplitVec<T> {
    idx: usize,
    vec: Vec<T>,
}

impl<T> SplitVec<T> {
    /// Creates a `SplitVec`, yielding the element at the given index. Panics if the index is out
    /// of bounds.
    pub fn new(vec: Vec<T>, idx: usize) -> (T, SplitVec<T>) {
        unimplemented!()
    }

    /// Places a value at the empty position and returns the original `Vec`.
    pub fn reunify(self, elem: T) -> Vec<T> {
        unimplemented!()
    }

    /// Returns the slice corresponding to the elements left of the split.
    pub fn left_ref(&self) -> &[T] {
        unimplemented!()
    }

    /// Returns the mutable slice corresponding to the elements left of the split.
    pub fn left_mut(&mut self) -> &mut [T] {
        unimplemented!()
    }

    /// Returns the slice corresponding to the elements right of the split.
    pub fn right_ref(&self) -> &[T] {
        unimplemented!()
    }

    /// Returns the mutable slice corresponding to the elements right of the split.
    pub fn right_mut(&mut self) -> &mut [T] {
        unimplemented!()
    }
}

impl<T: Debug> Debug for SplitVec<T> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        unimplemented!()
    }
}

impl<T> Drop for SplitVec<T> {
    fn drop(&mut self) {
        unimplemented!()
    }
}
