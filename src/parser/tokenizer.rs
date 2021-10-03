use crate::parser::error::{InvalidToken, ParseResult, ParserException};
use crate::parser::line_info::LineInfo;
use crate::parser::token::{Token, TokenType};
use std::collections::BTreeSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use unicode_normalization::UnicodeNormalization;

#[derive(Debug)]
pub struct Tokenizer {
    reader: BufReader<File>,
    file_name: PathBuf,
    next: String,
    full_line: String,
    lb_indices: BTreeSet<usize>,
}

impl Tokenizer {
    pub fn tokenize_next(&mut self) -> ParseResult<Token> {
        loop {
            let next_token = self.next_token()?.ok_or_else(|| self.invalid())?;
            if !next_token.is_whitespace() {
                return ParseResult::Ok(next_token);
            }
        }
    }

    fn next_token(&mut self) -> ParseResult<Option<Token>> {
        if self.next.is_empty() {
            return ParseResult::Ok(Option::Some(self.empty_line()?));
        }
        let next_token = self.adjust_for_multiline()?;
        if next_token.is_some() {
            return ParseResult::Ok(next_token);
        }
        if self.next.starts_with("|#") {
            return ParseResult::Err(
                ParserException::of(
                    "Illegal sequence '|#': may only appear as the end of a comment",
                    self.line_info(),
                )
                .into(),
            );
        }
        for matcher in TokenType::matchers() {
            if let Option::Some((token, end)) = matcher(&self.next) {
                let line_info = self.line_info();
                self.next.drain(0..end);
                return ParseResult::Ok(Option::Some(Token::new(token, line_info)));
            }
        }
        ParseResult::Ok(Option::None)
    }

    fn invalid(&self) -> ParserException {
        match InvalidToken::parse(&self.next) {
            Option::Some(t) => self.invalid_token_err(t),
            Option::None => self.std_token_err(),
        }
    }

    fn empty_line(&mut self) -> ParseResult<Token> {
        assert!(self.next.is_empty());
        ParseResult::Ok(match self.read_line()? {
            Option::None => Token::epsilon(self.line_info()),
            Option::Some(mut next_line) => {
                next_line.truncate(next_line.trim_end().len());
                next_line = next_line.nfkd().collect::<String>();
                self.next = next_line.clone();
                self.full_line = next_line;
                self.lb_indices.clear();
                self.append_escaped_lines()?;
                Token::newline(self.line_info())
            }
        })
    }

    fn adjust_for_multiline(&mut self) -> ParseResult<Option<Token>> {
        ParseResult::Ok(if self.multiline_comment() {
            let info = self.concat_comment()?;
            Option::Some(Token::new(TokenType::Whitespace, info))
        } else if self.multiline_string() {
            let (text, info) = self.concat_string()?;
            Option::Some(Token::new(TokenType::String(text), info))
        } else {
            Option::None
        })
    }

    fn multiline_comment(&self) -> bool {
        self.next.starts_with("#|") && !self.next[2..].contains("|#")
    }

    fn multiline_string(&self) -> bool {
        let mut chars = self.next.chars();
        let mut next = match chars.next() {
            Option::Some(x) => x,
            Option::None => return false,
        };
        while "refbcy".contains(next) {
            next = match chars.next() {
                Option::Some(x) => x,
                Option::None => return false,
            };
        }
        let delim = match next {
            '"' | '\'' => next,
            _ => return false,
        };
        let mut backslashes = 0usize;
        for chr in chars {
            if chr == '\\' {
                backslashes += 1;
            } else if chr == delim {
                if backslashes % 2 == 0 {
                    return false; // String terminates, no multiline
                } else {
                    backslashes = 0;
                }
            } else {
                backslashes = 0;
            }
        }
        true
    }

    fn concat_comment(&mut self) -> ParseResult<LineInfo> {
        debug_assert!(self.next.starts_with("#|"));
        let line_info = self.line_info();
        loop {
            if let Option::Some(x) = self.next.find("|#") {
                // TODO? clone_into() when stable (#41263)
                self.next = self.full_line[x + 2..].to_owned();
                return ParseResult::Ok(line_info);
            }
            let next_line = self.read_line()?.expect("TODO: Error message");
            self.full_line = next_line;
            self.append_escaped_lines()?;
        }
    }

    fn concat_string(&mut self) -> ParseResult<(String, LineInfo)> {
        let line_info = self.line_info();
        let mut sequence = self.next.clone();
        let delim = '"'; // FIXME
        loop {
            let next_line = self.read_line()?.unwrap();
            let mut backslashes = 0usize;
            for (i, chr) in self.full_line.char_indices() {
                if chr == '\\' {
                    backslashes += 1;
                } else if chr == delim {
                    if backslashes % 2 == 0 {
                        sequence.push_str(&next_line[..i + 1]);
                        // TODO? clone_into() when stable (#41263)
                        self.next = next_line[i + 1..].to_owned();
                        self.full_line = next_line;
                        return ParseResult::Ok((sequence, line_info));
                    } else {
                        backslashes = 0;
                    }
                } else {
                    backslashes = 0;
                }
            }
            sequence.push_str(&next_line);
        }
    }

    fn append_escaped_lines(&mut self) -> ParseResult<()> {
        while self.next.ends_with('\\') {
            self.next.truncate(self.next.len() - 1);
            let next_line = self.read_line()?.unwrap();
            self.next.push_str(&next_line);
            self.full_line = self.next.clone();
            self.lb_indices.insert(self.full_line.len());
        }
        ParseResult::Ok(())
    }

    fn invalid_token_err(&self, token: InvalidToken) -> ParserException {
        self.token_err(token.message())
    }

    fn std_token_err(&self) -> ParserException {
        self.token_err("Invalid syntax")
    }

    fn token_err<T: ToString>(&self, msg: T) -> ParserException {
        ParserException::of(msg, self.line_info())
    }

    fn line_info(&self) -> LineInfo {
        LineInfo::new(
            self.file_name.clone(),
            self.line_number(),
            self.full_line.clone(),
            self.line_index(),
        )
    }

    fn read_line(&mut self) -> ParseResult<Option<String>> {
        let mut buffer = String::new();
        match self.reader.read_line(&mut buffer) {
            Result::Ok(x) => ParseResult::Ok(if x == 0 {
                Option::None
            } else {
                Option::Some(buffer)
            }),
            Result::Err(e) => ParseResult::Err(ParserException::of(e, self.line_info()).into()),
        }
    }

    fn line_number(&self) -> usize {
        todo!()
    }

    fn line_index(&self) -> usize {
        todo!()
    }
}
