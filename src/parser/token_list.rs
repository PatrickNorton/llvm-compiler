use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::{Token, TokenType};
use crate::parser::tokenizer::Tokenizer;
use std::cmp::Ordering;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenList {
    buffer: VecDeque<Token>,
    tokenizer: Tokenizer,
    next: Option<Token>,
}

impl TokenList {
    pub fn new(tokenizer: Tokenizer) -> TokenList {
        TokenList {
            buffer: VecDeque::new(),
            tokenizer,
            next: Option::None,
        }
    }

    pub fn next_token(&mut self) -> ParseResult<Token> {
        match self.buffer.pop_front() {
            Option::Some(x) => Result::Ok(x),
            Option::None => self.tokenizer.tokenize_next(),
        }
    }

    pub fn next_tok(&mut self, ignore_newlines: bool) -> ParseResult<Token> {
        let token = self.next_token();
        if ignore_newlines {
            self.pass_newlines()?;
        }
        token
    }

    pub fn first(&mut self) -> ParseResult<&Token> {
        self.ensure_length(1)?;
        ParseResult::Ok(&self.buffer[0])
    }

    pub fn get_token(&mut self, at: usize) -> ParseResult<&Token> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(&self.buffer[at])
    }

    pub fn token_type(&mut self) -> ParseResult<&TokenType> {
        ParseResult::Ok(self.first()?.token_type())
    }

    pub fn token_type_at(&mut self, at: usize) -> ParseResult<&TokenType> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(self.buffer[at].token_type())
    }

    pub fn token_equals(&mut self, text: impl AsRef<str>) -> ParseResult<bool> {
        ParseResult::Ok(self.first()?.equals(text.as_ref()))
    }

    pub fn error(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(message.as_ref(), first))
    }

    pub fn default_error(&mut self) -> ParserError {
        self.error_with_first("Unexpected")
    }

    pub fn error_expected(&mut self, expected: &str) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(
            format!("Expected {}, got {:?}", expected, first),
            first,
        ))
    }

    pub fn error_with_first(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(
            format!("{} {:?}", message.as_ref(), first),
            first,
        ))
    }

    pub fn line_info(&mut self) -> ParseResult<&LineInfo> {
        self.first().map(Token::line_info)
    }

    pub fn pass_newlines(&mut self) -> ParseResult<()> {
        while matches!(self.token_type()?, TokenType::Newline) {
            self.next_token()?;
        }
        ParseResult::Ok(())
    }

    pub fn brace_contains(&mut self, tok: &TokenType) -> ParseResult<bool> {
        for token in self.first_level() {
            if token.is_type(tok) {
                return ParseResult::Ok(true);
            }
        }
        ParseResult::Ok(false)
    }

    pub fn brace_contains_str(&mut self, text: impl AsRef<str>) -> ParseResult<bool> {
        let text = text.as_ref();
        for token in self.first_level() {
            if token.equals(text) {
                return ParseResult::Ok(true);
            }
        }
        ParseResult::Ok(false)
    }

    pub fn brace_contains_kwd(&mut self, kwd: Keyword) -> ParseResult<bool> {
        for token in self.first_level() {
            if token.is_kwd(kwd) {
                return ParseResult::Ok(true);
            }
        }
        ParseResult::Ok(false)
    }

    pub fn brace_is_empty(&mut self) -> ParseResult<bool> {
        let mut next = 1;
        while matches!(self.token_type_at(next)?, &TokenType::Newline) {
            next += 1;
        }
        ParseResult::Ok(matches!(
            self.token_type_at(next)?,
            &TokenType::CloseBrace(_)
        ))
    }

    pub fn size_of_variable(&mut self) -> ParseResult<usize> {
        self.size_of_variable_at(0)
    }

    pub fn size_of_variable_at(&mut self, offset: usize) -> ParseResult<usize> {
        assert!(matches!(
            self.token_type()?,
            TokenType::Name(_) | TokenType::OpenBrace(_)
        ));
        let mut net_braces = 0;
        let mut was_var = false;
        for size in offset.. {
            let token = self.get_token(size)?;
            match token.token_type() {
                TokenType::OpenBrace(_) => net_braces += 1,
                TokenType::CloseBrace(_) => {
                    if net_braces == 0 {
                        return Ok(size);
                    } else {
                        net_braces -= 1;
                    }
                }
                TokenType::Name(_) => {
                    if was_var && net_braces == 0 {
                        return Ok(size);
                    } else {
                        was_var = true;
                    }
                }
                TokenType::Dot => {
                    was_var = false;
                }
                TokenType::Epsilon => match net_braces.cmp(&0) {
                    Ordering::Greater => {
                        return Err(ParserError::Normal(ParserException::of(
                            "Unmatched brace",
                            token,
                        )))
                    }
                    Ordering::Equal => return Ok(size),
                    Ordering::Less => {}
                },
                _ => {
                    if net_braces == 0 {
                        return Ok(size);
                    }
                }
            }
        }
        unreachable!("Infinite loop")
    }

    pub fn expect_newline(&self) -> ParseResult<()> {
        todo!()
    }

    fn first_level(&mut self) -> impl Iterator<Item = Token> + '_ {
        LevelIterator::new(1)
    }

    fn ensure_length(&mut self, len: usize) -> ParseResult<()> {
        while self.buffer.len() < len {
            let token = self.tokenizer.tokenize_next()?;
            self.buffer.push_back(token)
        }
        ParseResult::Ok(())
    }
}

impl Iterator for TokenList {
    type Item = ParseResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Option::Some(self.next_token())
    }
}

struct LevelIterator {}

impl LevelIterator {
    pub fn new(level: u32) -> LevelIterator {
        todo!()
    }
}

impl Iterator for LevelIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
