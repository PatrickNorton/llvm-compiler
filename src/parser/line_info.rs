use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct LineInfo {
    inner: Option<Arc<InfoInner>>,
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

impl LineInfo {
    pub fn new(path: PathBuf, line_no: usize, line: String, start: usize) -> LineInfo {
        LineInfo {
            inner: Some(Arc::new(InfoInner {
                path,
                line_no,
                line,
                start,
            })),
        }
    }

    pub const fn empty() -> LineInfo {
        Self { inner: None }
    }

    pub fn empty_ref<'a>() -> &'a LineInfo {
        static EMPTY: LineInfo = LineInfo::empty();
        &EMPTY
    }

    pub fn get_path(&self) -> &Path {
        self.inner
            .as_ref()
            .map_or_else(|| Path::new(""), |x| &x.path)
    }

    pub fn get_line_number(&self) -> usize {
        self.inner
            .as_ref()
            .map_or_else(|| usize::MAX, |x| x.line_no)
    }

    pub fn info_string(&self) -> String {
        match &self.inner {
            Option::Some(inner) => {
                let num_spaces = inner.start + inner.line_no.to_string().len() + 2;
                format!(
                    "{}: {}\n{:spaces$}^",
                    inner.line_no,
                    inner.line,
                    "",
                    spaces = num_spaces
                )
            }
            Option::None => "Line info not found".to_string(),
        }
    }

    pub fn substring(&self, start: usize) -> LineInfo {
        match &self.inner {
            Option::Some(inner) => LineInfo {
                inner: Some(Arc::new(InfoInner {
                    path: inner.path.clone(),
                    line_no: inner.line_no,
                    line: inner.line.clone(),
                    start: inner.start + start,
                })),
            },
            Option::None => LineInfo::empty(),
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
    T: Lined + ?Sized,
{
    fn line_info(&self) -> &LineInfo {
        (*self).line_info()
    }
}
