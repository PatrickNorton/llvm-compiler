use std::path::{Path, PathBuf};
use std::sync::Arc;

use once_cell::sync::Lazy;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct LineInfo {
    inner: Arc<InfoInner>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct InfoInner {
    path: PathBuf,
    line_no: usize,
    line: String,
    start: usize,
}

pub trait Lined {
    fn line_info(&self) -> &LineInfo;
}

static EMPTY: Lazy<LineInfo> = Lazy::new(|| LineInfo {
    inner: Arc::new(InfoInner {
        path: PathBuf::new(),
        line_no: usize::MAX,
        line: String::new(),
        start: 0,
    }),
});

impl LineInfo {
    pub fn new(path: PathBuf, line_no: usize, line: String, start: usize) -> LineInfo {
        LineInfo {
            inner: Arc::new(InfoInner {
                path,
                line_no,
                line,
                start,
            }),
        }
    }

    pub fn empty() -> LineInfo {
        EMPTY.clone()
    }

    pub fn empty_ref<'a>() -> &'a LineInfo {
        &*EMPTY
    }

    pub fn get_path(&self) -> &Path {
        &self.inner.path
    }

    pub fn get_line_number(&self) -> usize {
        self.inner.line_no
    }

    pub fn info_string(&self) -> String {
        let num_spaces = self.inner.start + self.inner.line_no.to_string().len() + 2;
        format!(
            "{}: {}\n{:spaces$}^",
            self.inner.line_no,
            self.inner.line,
            "",
            spaces = num_spaces
        )
    }

    pub fn substring(&self, start: usize) -> LineInfo {
        LineInfo {
            inner: Arc::new(InfoInner {
                path: self.inner.path.clone(),
                line_no: self.inner.line_no,
                line: self.inner.line.clone(),
                start: self.inner.start + start,
            }),
        }
    }
}

impl Lined for LineInfo {
    fn line_info(&self) -> &LineInfo {
        self
    }
}

impl<T> Lined for &T
where
    T: Lined,
{
    fn line_info(&self) -> &LineInfo {
        (*self).line_info()
    }
}
