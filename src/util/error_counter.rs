use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug)]
pub struct ErrorCounter {
    warnings: AtomicUsize,
    errors: AtomicUsize,
}

impl ErrorCounter {
    pub fn new() -> Self {
        Self {
            warnings: AtomicUsize::new(0),
            errors: AtomicUsize::new(0),
        }
    }

    pub fn get_warnings(&self) -> usize {
        self.warnings.load(Ordering::Relaxed)
    }

    pub fn get_errors(&self) -> usize {
        self.errors.load(Ordering::Relaxed)
    }

    pub fn add_warning(&self) {
        self.warnings.fetch_add(1, Ordering::Relaxed);
    }

    pub fn add_error(&self) {
        self.errors.fetch_add(1, Ordering::Relaxed);
    }
}

impl Default for ErrorCounter {
    fn default() -> Self {
        Self::new()
    }
}
