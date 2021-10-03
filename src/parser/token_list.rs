use crate::parser::error::ParseResult;
use crate::parser::token::{Token, TokenType};
use crate::parser::tokenizer::Tokenizer;
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

    pub fn token_is(&mut self, token: &TokenType) -> ParseResult<bool> {
        ParseResult::Ok(self.first()?.is_type(token))
    }

    pub fn token_is_descr(&mut self) -> ParseResult<bool> {
        ParseResult::Ok(self.first()?.is_descriptor())
    }

    fn pass_newlines(&mut self) -> ParseResult<()> {
        while self.token_is(&TokenType::Newline)? {
            self.next_token()?;
        }
        ParseResult::Ok(())
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
