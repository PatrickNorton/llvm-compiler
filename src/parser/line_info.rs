use std::path::{Path, PathBuf};

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct LineInfo {
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
            path,
            line_no,
            line,
            start,
        }
    }

    pub fn get_path(&self) -> &Path {
        &self.path
    }

    pub fn get_line_number(&self) -> usize {
        self.line_no
    }

    pub fn info_string(&self) -> String {
        let num_spaces = self.start + self.line_no.to_string().len() + 2;
        format!(
            "{}: {}\n{:spaces$}^",
            self.line_no,
            self.line,
            "",
            spaces = num_spaces
        )
    }
}

impl Lined for LineInfo {
    fn line_info(&self) -> &LineInfo {
        self
    }
}
