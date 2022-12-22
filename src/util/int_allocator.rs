use std::collections::BTreeSet;
use std::sync::atomic::{AtomicUsize, Ordering};

/// A thread-safe struct that allocates consecutive integers.
///
/// Note that this does not have feature parity with its single-threaded sibling
/// [`IntAllocator`], in particular, it does not have the
/// [`remove()`][IntAllocator::remove] function; if you want a thread-safe
/// removable allocator, use a `Mutex<IntAllocator>` instead.
///
/// # Usage
/// ```
/// let alloc = SyncIntAllocator::new();
/// assert_eq!(alloc.next(), 0);
/// assert_eq!(alloc.next(), 1);
/// assert_eq!(alloc.next(), 2);
/// ```
#[derive(Debug)]
pub struct SyncIntAllocator {
    inner: AtomicUsize,
}

/// A struct that allocates consecutive integers.
///
/// In function, this is similar to a [`HashSet<usize>`], but is optimized for
/// contiguous ranges of integers. This is accomplished by a maximum value and
/// set of removed integers below that max. This means that if no values
/// (besides the current maximum) are removed, this will not allocate any
/// memory on the heap.
///
/// # Usage
/// ```
/// let alloc = IntAllocator::new();
/// assert_eq!(alloc.next(), 0);
/// assert_eq!(alloc.next(), 1);
/// assert_eq!(alloc.next(), 2);
/// alloc.remove(1);
/// assert_eq!(alloc.next(), 1);
/// assert_eq!(alloc.next(), 3);
/// ```
#[derive(Debug)]
pub struct IntAllocator {
    max: usize,
    removed: BTreeSet<usize>,
}

impl SyncIntAllocator {
    /// Constructs a new, empty `SyncIntAllocator`.
    ///
    /// # Examples
    /// ```
    /// let ints = SyncIntAllocator::new();
    /// ```
    pub const fn new() -> Self {
        Self {
            inner: AtomicUsize::new(0),
        }
    }

    /// Allocates the smallest unused number and returns it.
    ///
    /// This takes an immutable reference to `self`, so it can be used in a
    /// multithreaded context. It will never return the same number twice.
    ///
    /// # Examples
    ///
    /// ```
    /// let ints = SyncIntAllocator::new();
    /// assert_eq!(ints.next(), 1);
    /// assert_eq!(ints.next(), 2);
    /// assert_eq!(ints.next(), 3);
    /// ```
    pub fn next(&self) -> usize {
        self.inner.fetch_add(1, Ordering::Relaxed)
    }
}

impl IntAllocator {
    /// Constructs a new, empty `IntAllocator`.
    ///
    /// # Examples
    /// ```
    /// let mut ints = IntAllocator::new();
    /// ```
    pub fn new() -> Self {
        Self {
            max: 0,
            removed: BTreeSet::new(),
        }
    }

    /// Returns `true` if the `IntAllocator` has allocated the given value.
    ///
    /// # Examples
    /// ```
    /// let mut ints = IntAllocator::new();
    /// ints.next();
    /// ints.next();
    /// assert!(ints.contains(0));
    /// assert!(ints.contains(1));
    /// assert!(!ints.contains(2));
    /// ```
    pub fn contains(&self, value: usize) -> bool {
        value < self.max && !self.removed.contains(&value)
    }

    /// Returns the maximum allocated value, or `None` if no values have been
    /// allocated.
    ///
    /// # Examples
    /// ```
    /// let mut ints = IntAllocator::new();
    /// assert_eq!(ints.max(), None);
    /// ints.next();
    /// ints.next();
    /// assert_eq!(ints.max(), Some(1));
    /// ```
    pub fn max(&self) -> Option<usize> {
        if self.max == 0 {
            return None;
        }
        let mut max = self.max - 1;
        while self.removed.contains(&max) {
            if max == 0 {
                return None;
            }
            max -= 1;
        }
        Some(max)
    }

    /// Allocates the smallest unused number and returns it.
    ///
    /// Different calls to this function will never return the same value unless
    /// [`Self::remove()`](self::remove) was called in between.
    ///
    /// # Examples
    /// ```
    /// let mut ints = IntAllocator::new();
    /// assert_eq!(ints.next(), 0);
    /// assert_eq!(ints.next(), 1);
    /// ints.remove(0);
    /// assert_eq!(ints.next(), 0); // Smallest unallocated number is always given first
    /// ```
    pub fn next(&mut self) -> usize {
        match self.removed.pop_first() {
            Option::Some(first) => first,
            Option::None => {
                self.max += 1;
                self.max - 1
            }
        }
    }

    /// Deallocates the given number if it is allocated.
    ///
    /// # Examples
    /// ```
    /// let mut ints = IntAllocator::new();
    /// ints.next();
    /// ints.remove(0);
    /// assert_eq!(ints.next(), 0);
    /// ```
    pub fn remove(&mut self, value: usize) {
        if self.contains(value) {
            if value == self.max - 1 {
                self.max -= 1;
                self.removed.remove(&self.max);
                while self.max > 0 && self.removed.remove(&(self.max - 1)) {
                    self.max -= 1;
                }
            } else {
                self.removed.insert(value);
            }
        }
    }
}

impl Default for IntAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for SyncIntAllocator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::IntAllocator;

    #[test]
    fn increasing() {
        let mut allocator = IntAllocator::new();
        for i in 0..10 {
            assert_eq!(allocator.next(), i);
        }
    }

    #[test]
    fn remove() {
        let mut allocator = IntAllocator::new();
        for i in 0..5 {
            assert_eq!(allocator.next(), i);
        }
        allocator.remove(2);
        assert_eq!(allocator.next(), 2);
        assert_eq!(allocator.next(), 5);
    }

    #[test]
    fn max() {
        let mut allocator = IntAllocator::new();
        assert_eq!(allocator.max(), None);
        for _ in 0..5 {
            allocator.next();
        }
        assert_eq!(allocator.max(), Some(4));
    }

    #[test]
    fn contains_simple() {
        let mut allocator = IntAllocator::new();
        for _ in 0..10 {
            allocator.next();
        }
        for i in 0..10 {
            assert!(allocator.contains(i))
        }
    }

    #[test]
    fn contains_remove() {
        let mut allocator = IntAllocator::new();
        for _ in 0..10 {
            allocator.next();
        }
        allocator.remove(2);
        allocator.remove(7);
        for i in 0..10 {
            if i == 2 || i == 7 {
                assert!(!allocator.contains(i))
            } else {
                assert!(allocator.contains(i))
            }
        }
    }

    #[test]
    fn max_remove() {
        let mut allocator = IntAllocator::new();
        for _ in 0..10 {
            allocator.next();
        }
        allocator.remove(3);
        assert_eq!(allocator.max(), Some(9));
        allocator.remove(9);
        assert_eq!(allocator.max(), Some(8));
    }
}

#[cfg(test)]
mod sync_test {
    use std::collections::HashSet;
    use std::sync::Arc;
    use std::thread;

    use super::SyncIntAllocator;

    #[test]
    fn increasing() {
        let allocator = SyncIntAllocator::new();
        for i in 0..10 {
            assert_eq!(allocator.next(), i);
        }
    }

    #[test]
    fn multithread_increasing() {
        let allocator = Arc::new(SyncIntAllocator::new());
        let mut threads = Vec::with_capacity(10);
        for _ in 0..10 {
            let allocator = allocator.clone();
            let handle = thread::spawn(move || {
                let mut prev = allocator.next();
                for _ in 0..9 {
                    let next = allocator.next();
                    assert!(next > prev);
                    prev = next;
                }
            });
            threads.push(handle);
        }
        for thread in threads {
            thread.join().unwrap();
        }
        assert_eq!(allocator.next(), 100);
    }

    #[test]
    fn multithread_unique() {
        let allocator = Arc::new(SyncIntAllocator::new());
        let mut threads = Vec::with_capacity(10);
        for _ in 0..10 {
            let allocator = allocator.clone();
            let handle = thread::spawn(move || {
                let mut all_numbers = HashSet::with_capacity(10);
                for _ in 0..10 {
                    let next = allocator.next();
                    assert!(!all_numbers.contains(&next));
                    all_numbers.insert(next);
                }
                all_numbers
            });
            threads.push(handle);
        }
        let mut all_tables = Vec::<HashSet<usize>>::with_capacity(threads.len());
        for thread in threads {
            let result = thread.join().unwrap();
            for table in &all_tables {
                let mut intersection = table.intersection(&result);
                assert!(intersection.next().is_none());
            }
            all_tables.push(result);
        }
        assert_eq!(allocator.next(), 100);
    }
}
