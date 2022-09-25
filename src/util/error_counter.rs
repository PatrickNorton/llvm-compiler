use std::sync::atomic::{AtomicUsize, Ordering};

/// A counter for the number of warnings and errors emitted.
///
/// This is a thread-safe counter, so it can be incremented through a shared
/// reference.
#[derive(Debug)]
pub struct ErrorCounter {
    warnings: AtomicUsize,
    errors: AtomicUsize,
}

impl ErrorCounter {
    /// Creates a new [`ErrorCounter`].
    ///
    /// Both the warning and error counts are set to 0 by default.
    ///
    /// # Examples
    /// ```
    /// let x = ErrorCounter::new();
    /// ```
    pub fn new() -> Self {
        Self {
            warnings: AtomicUsize::new(0),
            errors: AtomicUsize::new(0),
        }
    }

    /// Returns the current warning count.
    pub fn get_warnings(&self) -> usize {
        self.warnings.load(Ordering::Relaxed)
    }

    /// Returns the current error count.
    pub fn get_errors(&self) -> usize {
        self.errors.load(Ordering::Relaxed)
    }

    /// Increments the current warning count.
    pub fn add_warning(&self) {
        self.warnings.fetch_add(1, Ordering::Relaxed);
    }

    /// Increments the current error count.
    pub fn add_error(&self) {
        self.errors.fetch_add(1, Ordering::Relaxed);
    }
}

impl Default for ErrorCounter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::thread;

    use super::ErrorCounter;

    #[test]
    fn empty() {
        let counter = ErrorCounter::new();
        assert_eq!(counter.get_warnings(), 0);
        assert_eq!(counter.get_errors(), 0);
    }

    #[test]
    fn default() {
        let counter = ErrorCounter::default();
        assert_eq!(counter.get_warnings(), 0);
        assert_eq!(counter.get_errors(), 0);
    }

    #[test]
    fn single_thread_warnings() {
        let counter = ErrorCounter::new();
        for i in 0..10 {
            assert_eq!(counter.get_warnings(), i);
            counter.add_warning();
        }
    }

    #[test]
    fn single_thread_errors() {
        let counter = ErrorCounter::new();
        for i in 0..10 {
            assert_eq!(counter.get_errors(), i);
            counter.add_error();
        }
    }

    #[test]
    fn single_thread_interleaved() {
        let counter = ErrorCounter::new();
        for i in 0..10 {
            assert_eq!(counter.get_warnings(), i);
            assert_eq!(counter.get_errors(), i);
            counter.add_warning();
            counter.add_error();
        }
    }

    #[test]
    fn multi_thread_warnings() {
        let counter = Arc::new(ErrorCounter::new());
        let mut threads = Vec::with_capacity(10);
        for _ in 0..10 {
            let count = counter.clone();
            let handle = thread::spawn(move || {
                for _ in 0..10 {
                    count.add_warning();
                }
            });
            threads.push(handle);
        }
        for thread in threads {
            thread.join().unwrap();
        }
        assert_eq!(counter.get_warnings(), 100);
    }

    #[test]
    fn multi_thread_errors() {
        let counter = Arc::new(ErrorCounter::new());
        let mut threads = Vec::with_capacity(10);
        for _ in 0..10 {
            let count = counter.clone();
            let handle = thread::spawn(move || {
                for _ in 0..10 {
                    count.add_error();
                }
            });
            threads.push(handle);
        }
        for thread in threads {
            thread.join().unwrap();
        }
        assert_eq!(counter.get_errors(), 100);
    }

    #[test]
    fn multi_thread_interleaved() {
        let counter = Arc::new(ErrorCounter::new());
        let mut threads = Vec::with_capacity(10);
        for _ in 0..10 {
            let count = counter.clone();
            let handle = thread::spawn(move || {
                for _ in 0..10 {
                    count.add_warning();
                    count.add_error();
                }
            });
            threads.push(handle);
        }
        for thread in threads {
            thread.join().unwrap();
        }
        assert_eq!(counter.get_warnings(), 100);
        assert_eq!(counter.get_errors(), 100);
    }
}
