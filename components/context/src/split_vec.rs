use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    mem::{forget, swap, uninitialized},
    ptr::write,
};

/// A vector which is split at a given index. Effectively, this is a `Vec` with a single
/// uninitialized position, which can be reunified to yield the original `Vec` without memory
/// allocation.
pub struct SplitVec<T> {
    idx: usize,

    // Note: vec[idx] is uninitialized. If vec is `None`, we're partway through a drop.
    vec: Option<Vec<T>>,
}

impl<T> SplitVec<T> {
    /// Creates a `SplitVec`, yielding the element at the given index. Panics if the index is out
    /// of bounds.
    pub fn new(mut vec: Vec<T>, idx: usize) -> (T, SplitVec<T>) {
        assert!(idx < vec.len());

        let elem = unsafe {
            let mut elem = uninitialized();
            swap(&mut vec[idx], &mut elem);
            elem
        };
        (
            elem,
            SplitVec {
                idx,
                vec: Some(vec),
            },
        )
    }

    /// Places a value at the empty position and returns the original `Vec`.
    pub fn reunify(mut self, mut elem: T) -> Vec<T> {
        let mut vec = None;
        swap(&mut self.vec, &mut vec);
        let mut vec = vec.expect("Reunifying dropped SplitVec");
        swap(&mut vec[self.idx], &mut elem);
        forget(elem);
        vec
    }

    /// Returns the slice corresponding to the elements left of the split.
    pub fn left(&self) -> &[T] {
        &self.vec.as_ref().expect("Taking left of dropped SplitVec")[..self.idx]
    }

    /// Returns the mutable slice corresponding to the elements left of the split.
    pub fn left_mut(&mut self) -> &mut [T] {
        &mut self
            .vec
            .as_mut()
            .expect("Taking left_mut of dropped SplitVec")[..self.idx]
    }

    /// Returns the slice corresponding to the elements right of the split.
    pub fn right(&self) -> &[T] {
        &self.vec.as_ref().expect("Taking right of dropped SplitVec")[self.idx + 1..]
    }

    /// Returns the mutable slice corresponding to the elements right of the split.
    pub fn right_mut(&mut self) -> &mut [T] {
        &mut self
            .vec
            .as_mut()
            .expect("Taking right_mut of dropped SplitVec")[self.idx + 1..]
    }
}

impl<T: Debug> Debug for SplitVec<T> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        fmt.debug_tuple("SplitVec")
            .field(&self.left())
            .field(&self.right())
            .finish()
    }
}

impl<T> Drop for SplitVec<T> {
    fn drop(&mut self) {
        let mut vec = None;
        swap(&mut self.vec, &mut vec);
        let mut vec = vec.expect("Double-dropping SplitVec?");
        vec.drain(self.idx + 1..).for_each(drop);
        unsafe {
            vec.set_len(self.idx);
        }
    }
}
