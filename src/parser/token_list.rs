use crate::parser::error::{ParseResult, ParserError, ParserException, ParserInternalError};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::token::{Token, TokenType};
use crate::parser::tokenizer::Tokenizer;
use std::cmp::Ordering;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenList {
    buffer: VecDeque<Token>,
    tokenizer: Tokenizer,
}

impl TokenList {
    pub fn new(tokenizer: Tokenizer) -> TokenList {
        TokenList {
            buffer: VecDeque::new(),
            tokenizer,
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

    pub fn next_if(
        &mut self,
        predicate: impl FnOnce(&Token) -> bool,
    ) -> ParseResult<Option<Token>> {
        self.ensure_length(1)?;
        if predicate(self.buffer.get(0).unwrap()) {
            self.next_token().map(Option::Some)
        } else {
            ParseResult::Ok(Option::None)
        }
    }

    pub fn next_if_ignoring(
        &mut self,
        ignore_newlines: bool,
        predicate: impl FnOnce(&Token) -> bool,
    ) -> ParseResult<Option<Token>> {
        self.ensure_length(1)?;
        if predicate(self.buffer.get(0).unwrap()) {
            self.next_tok(ignore_newlines).map(Option::Some)
        } else {
            ParseResult::Ok(Option::None)
        }
    }

    pub fn next_if_equals(&mut self, text: impl AsRef<str>) -> ParseResult<Option<Token>> {
        self.next_if(|x| x.equals(text.as_ref()))
    }

    pub fn next_if_eq_ignore(
        &mut self,
        text: impl AsRef<str>,
        ignore_newlines: bool,
    ) -> ParseResult<Option<Token>> {
        self.next_if_ignoring(ignore_newlines, |x| x.equals(text.as_ref()))
    }

    #[inline]
    pub fn first(&mut self) -> ParseResult<&Token> {
        self.ensure_length(1)?;
        ParseResult::Ok(&self.buffer[0])
    }

    #[inline]
    pub fn get_token(&mut self, at: usize) -> ParseResult<&Token> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(&self.buffer[at])
    }

    #[inline]
    pub fn token_type(&mut self) -> ParseResult<&TokenType> {
        ParseResult::Ok(self.first()?.token_type())
    }

    #[inline]
    pub fn token_type_at(&mut self, at: usize) -> ParseResult<&TokenType> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(self.buffer[at].token_type())
    }

    #[inline]
    pub fn token_equals(&mut self, text: impl AsRef<str>) -> ParseResult<bool> {
        ParseResult::Ok(self.first()?.equals(text.as_ref()))
    }

    #[inline]
    pub fn token_eq_at(&mut self, at: usize, text: impl AsRef<str>) -> ParseResult<bool> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(self.buffer[at].equals(text.as_ref()))
    }

    pub fn token_eq_either(
        &mut self,
        first: impl AsRef<str>,
        second: impl AsRef<str>,
    ) -> ParseResult<bool> {
        let value = self.first()?;
        ParseResult::Ok(value.equals(first.as_ref()) || value.equals(second.as_ref()))
    }

    pub fn error(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(message.as_ref(), first))
    }

    pub fn internal_error(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Internal(ParserInternalError::of(message.as_ref(), first))
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

    pub fn expect(&mut self, expected: &str, ignore_newlines: bool) -> ParseResult<()> {
        if !self.token_equals(expected)? {
            Err(self.error_expected(expected))
        } else {
            self.next_tok(ignore_newlines)?;
            ParseResult::Ok(())
        }
    }

    pub fn expect_keyword(&mut self, expected: Keyword, ignore_newlines: bool) -> ParseResult<()> {
        if !self.token_equals(expected.name())? {
            Err(self.error_expected(expected.name()))
        } else {
            self.next_tok(ignore_newlines)?;
            ParseResult::Ok(())
        }
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

    pub fn brace_contains_str(&mut self, text: impl AsRef<str>) -> ParseResult<bool> {
        let text = text.as_ref();
        self.brace_contains(|x| x.equals(text))
    }

    pub fn brace_contains_kwd(&mut self, kwd: Keyword) -> ParseResult<bool> {
        self.brace_contains(|x| x.is_kwd(kwd))
    }

    pub fn brace_contains_kwds<const N: usize>(&mut self, kwd: [Keyword; N]) -> ParseResult<bool> {
        self.brace_contains(|x| kwd.iter().any(|y| x.is_kwd(*y)))
    }

    pub fn brace_contains(&mut self, mut pred: impl FnMut(&Token) -> bool) -> ParseResult<bool> {
        let mut net_braces = 0;
        let mut index = 0;
        let first = self.first()?;
        if let TokenType::OpenBrace(_) = first.token_type() {
            index += 1;
            net_braces += 1;
        } else {
            return Ok(false);
        }
        while net_braces > 0 {
            let token = self.get_token(index)?;
            if net_braces == 1 && pred(token) {
                return Ok(true);
            } else {
                match token.token_type() {
                    TokenType::Epsilon => {
                        if net_braces > 0 {
                            return Err(self.error("Unmatched brace"));
                        } else {
                            return Ok(false);
                        }
                    }
                    TokenType::OpenBrace(o) => {
                        if net_braces == 0
                            && *o == '{'
                            && self.token_type_at(index - 1)?.precedes_literal_brace()
                        {
                            return Ok(false);
                        } else {
                            net_braces += 1;
                        }
                    }
                    TokenType::CloseBrace(_) => {
                        net_braces -= 1;
                        if net_braces == 0 {
                            return Ok(false);
                        }
                    }
                    _ => {}
                }
            }
            index += 1;
        }
        Ok(false)
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

    pub fn line_contains(&mut self, mut pred: impl FnMut(&Token) -> bool) -> ParseResult<bool> {
        let mut net_braces = 0;
        let mut index = 0;
        let first = self.first()?;
        if pred(first) {
            return Ok(true);
        }
        match first.token_type() {
            TokenType::OpenBrace(_) => {
                index += 1;
                net_braces += 1;
            }
            TokenType::Newline | TokenType::Epsilon => return Ok(false),
            _ => {}
        }
        loop {
            let token = self.get_token(index)?;
            if net_braces == 0 && pred(token) {
                return Ok(true);
            } else {
                match token.token_type() {
                    TokenType::Epsilon => {
                        if net_braces > 0 {
                            return Err(self.error("Unmatched brace"));
                        } else {
                            return Ok(false);
                        }
                    }
                    TokenType::OpenBrace(o) => {
                        if net_braces == 0
                            && *o == '{'
                            && self.token_type_at(index - 1)?.precedes_literal_brace()
                        {
                            return Ok(false);
                        } else {
                            net_braces += 1;
                        }
                    }
                    TokenType::CloseBrace(_) => {
                        net_braces -= 1;
                    }
                    TokenType::Newline => {
                        if net_braces == 0 {
                            return Ok(false);
                        }
                    }
                    _ => {}
                }
            }
            index += 1;
        }
    }

    pub fn size_of_variable(&mut self) -> ParseResult<usize> {
        self.size_of_variable_at(0)
    }

    pub fn size_of_variable_at(&mut self, offset: usize) -> ParseResult<usize> {
        assert!(matches!(
            self.token_type_at(offset)?,
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
                TokenType::Dot(_) => {
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

    pub fn expect_newline(&mut self) -> ParseResult<()> {
        match parse_if_matches!(self, TokenType::Newline)? {
            Option::Some(_) => Ok(()),
            Option::None => Err(self.error("Expected newline")),
        }
    }

    pub fn matching_brace(brace: char) -> char {
        match brace {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => panic!("Unknown brace {}", brace),
        }
    }

    pub fn number_of_newlines(&mut self, start: usize) -> ParseResult<usize> {
        let mut count = 0;
        while let TokenType::Newline = self.token_type_at(start + count)? {
            count += 1;
        }
        Ok(count)
    }

    fn ensure_length(&mut self, len: usize) -> ParseResult<()> {
        while self.buffer.len() < len {
            let token = self.tokenizer.tokenize_next()?;
            self.buffer.push_back(token)
        }
        ParseResult::Ok(())
    }
}
