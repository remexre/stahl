use std::{
    fmt::{Debug, Formatter, Result as FmtResult},
    mem::forget,
    ptr::{drop_in_place, read, write},
};

/// A vector which is split at a given index. Effectively, this is a `Vec` with a single
/// uninitialized position, which can be reunified to yield the original `Vec` without memory
/// allocation.
pub struct SplitVec<T> {
    idx: usize,

    // Note: vec[idx] is uninitialized.
    vec: Vec<T>,
}

impl<T> SplitVec<T> {
    /// Creates a `SplitVec`, yielding the element at the given index. Panics if the index is out
    /// of bounds.
    pub fn new(mut vec: Vec<T>, idx: usize) -> (T, SplitVec<T>) {
        assert!(idx < vec.len());

        let elem = unsafe { read(&vec[idx]) };
        (elem, SplitVec { idx, vec })
    }

    /// Places a value at the empty position and returns the original `Vec`.
    pub fn reunify(mut self, elem: T) -> Vec<T> {
        let vec = unsafe {
            write(&mut self.vec[self.idx], elem);
            read(&mut self.vec)
        };
        forget(self);
        vec
    }

    /// Returns the slices corresponding to the elements left of the split and right of the split.
    pub fn both(&self) -> (&[T], &[T]) {
        (self.left(), self.right())
    }

    /// Returns the mutable slices corresponding to the elements left of the split and right of the
    /// split.
    pub fn both_mut(&mut self) -> (&mut [T], &mut [T]) {
        let (l, ur) = self.vec.split_at_mut(self.idx);
        (l, &mut ur[1..])
    }

    /// Returns the slice corresponding to the elements left of the split.
    pub fn left(&self) -> &[T] {
        &self.vec[..self.idx]
    }

    /// Returns the mutable slice corresponding to the elements left of the split.
    pub fn left_mut(&mut self) -> &mut [T] {
        &mut self.vec[..self.idx]
    }

    /// Returns the slice corresponding to the elements right of the split.
    pub fn right(&self) -> &[T] {
        &self.vec[self.idx + 1..]
    }

    /// Returns the mutable slice corresponding to the elements right of the split.
    pub fn right_mut(&mut self) -> &mut [T] {
        &mut self.vec[self.idx + 1..]
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
        let left = self.left_mut() as *mut _;
        let right = self.right_mut() as *mut _;
        unsafe {
            self.vec.set_len(0);
            drop_in_place(left);
            drop_in_place(right);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copy() {
        let (three, sv) = SplitVec::new(vec![1, 2, 3, 4, 5], 2);
        assert_eq!(sv.left(), &[1, 2]);
        assert_eq!(three, 3);
        assert_eq!(sv.right(), &[4, 5]);
        assert_eq!(sv.both(), (&[1, 2] as &[_], &[4, 5] as &[_]));
    }

    #[test]
    fn non_copy() {
        let vec = vec![
            "foo".to_string(),
            "bar".to_string(),
            "baz".to_string(),
            "quux".to_string(),
        ];

        let (bar, mut sv) = SplitVec::new(vec, 1);
        assert_eq!(sv.left_mut(), &["foo"]);
        assert_eq!(bar, "bar");
        assert_eq!(sv.right_mut(), &["baz", "quux"]);
        assert_eq!(
            sv.both_mut(),
            (
                &mut ["foo".to_string()] as &mut [String],
                &mut ["baz".to_string(), "quux".to_string()] as &mut [String]
            )
        );
    }

    #[test]
    fn reunify() {
        let vec = vec![
            "foo".to_string(),
            "bar".to_string(),
            "baz".to_string(),
            "quux".to_string(),
        ];

        let (baz, sv) = SplitVec::new(vec.clone(), 2);
        assert_eq!(sv.left(), &["foo", "bar"]);
        assert_eq!(baz, "baz");
        assert_eq!(sv.right(), &["quux"]);

        let vec2 = sv.reunify(baz);
        assert_eq!(vec, vec2);
    }
}
