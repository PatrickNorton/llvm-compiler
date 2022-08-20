use std::error::Error;
use std::fmt::{Display, Write};

use backtrace::Backtrace;

use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;

// TODO? Make an ErrorBuilder API

#[derive(Debug)]
pub enum CompilerError {
    Normal(CompilerException),
    Internal(CompilerInternalError),
    Todo(CompilerTodoError),
}

#[derive(Debug)]
pub struct CompilerException {
    message: String,
    backtrace: Backtrace,
}

#[derive(Debug)]
pub struct CompilerInternalError {
    message: String,
    backtrace: Backtrace,
}

#[derive(Debug)]
pub struct CompilerTodoError {
    message: String,
    backtrace: Backtrace,
}

impl CompilerException {
    pub fn of<T: ToString, L: Lined>(message: T, line_info: L) -> Self {
        let mut message = message.to_string();
        let line_info = line_info.line_info();
        write!(
            &mut message,
            "\nError: File {} Line {}\n{}",
            line_info.get_path().display(),
            line_info.get_line_number(),
            line_info.info_string(),
        )
        .unwrap();
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }

    pub fn with_note<T: ToString, D: Display, L: Lined>(message: T, note: D, line_info: L) -> Self {
        let mut message = message.to_string();
        let line_info = line_info.line_info();
        write!(
            &mut message,
            "\nNote: {}\nError: File {} Line {}\n{}",
            note,
            line_info.get_path().display(),
            line_info.get_line_number(),
            line_info.info_string(),
        )
        .unwrap();
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }

    pub fn double_def(name: &str, info1: impl Lined, info2: impl Lined) -> Self {
        let info1 = info1.line_info();
        let info2 = info2.line_info();
        let message = format!(
            "Error: name {} defined twice\n\
             Definition 1: File {} Line {}\n{}\n\
             Definition 2: File {} Line {}\n{}\n",
            name,
            info1.get_path().display(),
            info1.get_line_number(),
            info1.info_string(),
            info2.get_path().display(),
            info2.get_line_number(),
            info2.info_string()
        );
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }

    pub fn double_def_op(op: OpSpTypeNode, info1: impl Lined, info2: impl Lined) -> Self {
        let info1 = info1.line_info();
        let info2 = info2.line_info();
        let message = format!(
            "Error: {} defined twice\n\
             Definition 1: File {} Line {}\n{}\n\
             Definition 2: File {} Line {}\n{}\n",
            op,
            info1.get_path().display(),
            info1.get_line_number(),
            info1.info_string(),
            info2.get_path().display(),
            info2.get_line_number(),
            info2.info_string()
        );
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }
}

impl CompilerInternalError {
    pub fn of<T: ToString, L: Lined>(message: T, line_info: L) -> Self {
        let mut message = message.to_string();
        let line_info = line_info.line_info();
        write!(
            &mut message,
            "\nInternal error: File {} Line {}\n{}",
            line_info.get_path().display(),
            line_info.get_line_number(),
            line_info.info_string(),
        )
        .unwrap();
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }

    pub fn with_note<T: ToString, D: Display, L: Lined>(message: T, note: D, line_info: L) -> Self {
        let mut message = message.to_string();
        let line_info = line_info.line_info();
        write!(
            &mut message,
            "\nNote: {}\nInternal error: File {} Line {}\n{}",
            note,
            line_info.get_path().display(),
            line_info.get_line_number(),
            line_info.info_string(),
        )
        .unwrap();
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }
}

impl CompilerTodoError {
    pub fn of<T: ToString, L: Lined>(message: T, line_info: L) -> Self {
        let mut message = message.to_string();
        let line_info = line_info.line_info();
        write!(
            &mut message,
            "\nError: Operation not yet implemented\nFile {} Line {}\n{}",
            line_info.get_path().display(),
            line_info.get_line_number(),
            line_info.info_string(),
        )
        .unwrap();
        Self {
            message,
            backtrace: Backtrace::new(),
        }
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::Normal(n) => Display::fmt(n, f),
            CompilerError::Internal(i) => Display::fmt(i, f),
            CompilerError::Todo(t) => Display::fmt(t, f),
        }
    }
}

impl Display for CompilerException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:?}", self.message, self.backtrace)
    }
}

impl Display for CompilerInternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:?}", self.message, self.backtrace)
    }
}

impl Display for CompilerTodoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:?}", self.message, self.backtrace)
    }
}

impl Error for CompilerError {}

impl Error for CompilerException {}

impl Error for CompilerInternalError {}

impl Error for CompilerTodoError {}

impl From<CompilerException> for CompilerError {
    fn from(x: CompilerException) -> Self {
        CompilerError::Normal(x)
    }
}

impl From<CompilerInternalError> for CompilerError {
    fn from(x: CompilerInternalError) -> Self {
        CompilerError::Internal(x)
    }
}

impl From<CompilerTodoError> for CompilerError {
    fn from(x: CompilerTodoError) -> Self {
        CompilerError::Todo(x)
    }
}
