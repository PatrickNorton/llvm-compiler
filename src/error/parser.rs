use std::backtrace::Backtrace;
use std::error::Error;
use std::fmt::Display;

use super::builder::{ErrorBuilder, ErrorType};
use super::line_info::Lined;

pub type ParseResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    Normal(ParserException),
    Internal(ParserInternalError),
}

#[derive(Debug)]
pub struct ParserException {
    message: String,
    backtrace: Backtrace,
}

#[derive(Debug)]
pub struct ParserInternalError {
    message: String,
    backtrace: Backtrace,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum InvalidToken {
    Exclamation,
    Semicolon,
    Operator,
    Backslash,
}

impl ParserException {
    pub fn from_builder(builder: ErrorBuilder<'_>) -> Self {
        Self {
            message: builder.get_message(ErrorType::Standard),
            backtrace: Backtrace::capture(),
        }
    }

    pub fn of<D: Display, U: Lined>(message: D, line_info: U) -> Self {
        Self::from_builder(ErrorBuilder::new(&line_info).with_message(&message))
    }

    pub fn with_note<T: Display, D: Display, U: Lined>(message: T, note: D, line_info: U) -> Self {
        Self::from_builder(
            ErrorBuilder::new(&line_info)
                .with_message(message)
                .with_note(note),
        )
    }
}

impl ParserInternalError {
    pub fn from_builder(builder: ErrorBuilder<'_>) -> Self {
        Self {
            message: builder.get_message(ErrorType::Standard),
            backtrace: Backtrace::capture(),
        }
    }

    pub fn of<D: Display, U: Lined>(message: D, line_info: U) -> Self {
        Self::from_builder(ErrorBuilder::new(&line_info).with_message(&message))
    }
}

impl From<ParserException> for ParserError {
    fn from(e: ParserException) -> Self {
        ParserError::Normal(e)
    }
}

impl From<ParserInternalError> for ParserError {
    fn from(e: ParserInternalError) -> Self {
        ParserError::Internal(e)
    }
}

impl InvalidToken {
    pub fn parse(text: &str) -> Option<InvalidToken> {
        if text.starts_with('!') {
            Option::Some(InvalidToken::Exclamation)
        } else if text.starts_with(';') {
            Option::Some(InvalidToken::Semicolon)
        } else if text.starts_with("operator ") {
            Option::Some(InvalidToken::Operator)
        } else if text.starts_with('\\') {
            Option::Some(InvalidToken::Backslash)
        } else {
            Option::None
        }
    }

    pub const fn message(&self) -> &'static str {
        match self {
            InvalidToken::Exclamation => "! is invalid",
            InvalidToken::Semicolon => "; is not allowed, go use Java or something",
            InvalidToken::Operator => "Invalid operator definition",
            InvalidToken::Backslash => "Invalid backslash escape",
        }
    }
}

impl Error for ParserError {}

impl Error for ParserException {}

impl Error for ParserInternalError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Normal(n) => write!(f, "{n}"),
            ParserError::Internal(i) => write!(f, "{i}"),
        }
    }
}

impl Display for ParserException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}", self.message, self.backtrace)
    }
}

impl Display for ParserInternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}", self.message, self.backtrace)
    }
}
