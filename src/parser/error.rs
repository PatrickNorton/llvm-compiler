use crate::parser::line_info::Lined;
use std::fmt::Write;

pub type ParseResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    Normal(ParserException),
    Internal(ParserInternalError),
}

#[derive(Debug)]
pub struct ParserException {
    message: String,
}

#[derive(Debug)]
pub struct ParserInternalError {}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum InvalidToken {
    Exclamation,
    Semicolon,
    Operator,
    Backslash,
}

impl ParserException {
    pub fn of<T: ToString, U: Lined>(message: T, line_info: U) -> Self {
        let mut message = message.to_string();
        let line_info = line_info.line_info();
        write!(
            &mut message,
            "\nError: File {} Line {}\n{}",
            line_info.get_path().display(),
            line_info.get_line_number(),
            line_info.info_string()
        )
        .unwrap();
        ParserException { message }
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

    pub fn message(&self) -> &'static str {
        match self {
            InvalidToken::Exclamation => "! is invalid",
            InvalidToken::Semicolon => "; is not allowed, go use Java or something",
            InvalidToken::Operator => "Invalid operator definition",
            InvalidToken::Backslash => "Invalid backslash escape",
        }
    }
}
