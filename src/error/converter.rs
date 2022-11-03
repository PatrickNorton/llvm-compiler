use std::backtrace::Backtrace;
use std::error::Error;
use std::fmt::Display;

use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;

use super::builder::{ErrorBuilder, ErrorType};

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
    pub fn of<D: Display, L: Lined>(message: D, line_info: L) -> Self {
        let builder = ErrorBuilder::new(&line_info).with_message(message);
        Self::from_builder(builder)
    }

    pub fn with_note<D1: Display, D2: Display, L: Lined>(
        message: D1,
        note: D2,
        line_info: L,
    ) -> Self {
        let builder = ErrorBuilder::new(&line_info)
            .with_message(message)
            .with_note(note);
        Self::from_builder(builder)
    }

    pub fn double_def(name: &str, info1: impl Lined, info2: impl Lined) -> Self {
        Self::from_builder(ErrorBuilder::double_def(name, &info1, &info2))
    }

    pub fn double_def_op(op: OpSpTypeNode, info1: impl Lined, info2: impl Lined) -> Self {
        Self::from_builder(ErrorBuilder::double_def(op, &info1, &info2))
    }

    pub fn from_builder(builder: ErrorBuilder<'_>) -> Self {
        Self {
            message: builder.get_message(ErrorType::Standard),
            backtrace: Backtrace::capture(),
        }
    }
}

impl CompilerInternalError {
    pub fn of<D: Display, L: Lined>(message: D, line_info: L) -> Self {
        let builder = ErrorBuilder::new(&line_info).with_message(message);
        Self::from_builder(builder)
    }

    pub fn with_note<D1: Display, D2: Display, L: Lined>(
        message: D1,
        note: D2,
        line_info: L,
    ) -> Self {
        let builder = ErrorBuilder::new(&line_info)
            .with_message(message)
            .with_note(note);
        Self::from_builder(builder)
    }

    pub fn from_builder(builder: ErrorBuilder<'_>) -> Self {
        Self {
            message: builder.get_message(ErrorType::Internal),
            backtrace: Backtrace::capture(),
        }
    }
}

impl CompilerTodoError {
    pub fn of<D: Display, L: Lined>(message: D, line_info: L) -> Self {
        let builder = ErrorBuilder::new(&line_info).with_message(message);
        Self::from_builder(builder)
    }

    pub fn from_builder(builder: ErrorBuilder<'_>) -> Self {
        Self {
            message: builder.get_message(ErrorType::Todo),
            backtrace: Backtrace::capture(),
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
        write!(f, "{}\n{}", self.message, self.backtrace)
    }
}

impl Display for CompilerInternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}", self.message, self.backtrace)
    }
}

impl Display for CompilerTodoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}", self.message, self.backtrace)
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
