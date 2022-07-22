use crate::parser::error::{InvalidToken, ParseResult, ParserException};
use crate::parser::line_info::LineInfo;
use crate::parser::token::{Token, TokenType};
use crate::parser::token_list::TokenList;
use std::collections::BTreeSet;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Cursor, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

use super::token::STRING_PREFIXES;

#[derive(Debug)]
pub struct Tokenizer {
    reader: Reader,
    file_name: Arc<Path>,
    next: String,
    full_line: Arc<str>,
    lb_indices: BTreeSet<usize>,
    line_number: usize,
}

#[derive(Debug)]
enum Reader {
    File(BufReader<File>),
    String(Cursor<Vec<u8>>),
}

impl Tokenizer {
    fn from_file(path: PathBuf) -> io::Result<ParseResult<Tokenizer>> {
        let mut tokenizer = Tokenizer {
            file_name: (&*path).into(),
            reader: Reader::File(BufReader::new(File::open(path)?)),
            next: String::new(),
            full_line: Arc::from(""),
            lb_indices: BTreeSet::new(),
            line_number: 0,
        };
        // Get rid of leading newline
        Ok(tokenizer.next_token().map(|_| tokenizer))
    }

    fn from_str(str: &str, path: PathBuf, line_number: usize) -> ParseResult<Tokenizer> {
        let mut tokenizer = Tokenizer {
            reader: Reader::String(Cursor::new(str.as_bytes().to_vec())),
            file_name: path.into(),
            next: String::new(),
            full_line: Arc::from(""),
            lb_indices: BTreeSet::new(),
            line_number,
        };
        // Get rid of leading newline
        tokenizer.next_token()?;
        Ok(tokenizer)
    }

    pub fn tokenize_next(&mut self) -> ParseResult<Token> {
        loop {
            let next_token = self.next_token()?.ok_or_else(|| self.invalid())?;
            if !next_token.is_whitespace() {
                return ParseResult::Ok(next_token);
            }
        }
    }

    pub fn parse(f: PathBuf) -> io::Result<ParseResult<TokenList>> {
        Ok(Tokenizer::from_file(f)?.map(TokenList::new))
    }

    pub fn parse_str(str: &str, path: PathBuf, line_no: usize) -> ParseResult<TokenList> {
        Ok(TokenList::new(Tokenizer::from_str(str, path, line_no)?))
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
                let text = self.next.drain(0..end).collect::<String>();
                return ParseResult::Ok(Option::Some(Token::new(token, text, line_info)));
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
        // FIXME: Files ending with a newline don't return the newline
        ParseResult::Ok(match self.read_line()? {
            Option::None => Token::epsilon(self.line_info()),
            Option::Some(next_line) => {
                let next_trimmed = next_line.trim_end();
                let next_line = next_trimmed.nfkd().collect::<String>();
                self.full_line = (&*next_line).into();
                self.next = next_line;
                self.lb_indices.clear();
                self.append_escaped_lines()?;
                Token::newline(self.line_info())
            }
        })
    }

    fn adjust_for_multiline(&mut self) -> ParseResult<Option<Token>> {
        ParseResult::Ok(if self.multiline_comment() {
            let info = self.concat_comment()?;
            Option::Some(Token::new(TokenType::Whitespace, "", info))
        } else if let Option::Some(delim) = self.multiline_string() {
            let (text, info) = self.concat_string(delim)?;
            Option::Some(Token::new(TokenType::String(text.clone()), text, info))
        } else {
            Option::None
        })
    }

    fn multiline_comment(&self) -> bool {
        self.next.starts_with("#|") && !self.next[2..].contains("|#")
    }

    fn multiline_string(&self) -> Option<char> {
        let mut chars = self.next.chars();
        let mut next = match chars.next() {
            Option::Some(x) => x,
            Option::None => return None,
        };
        while STRING_PREFIXES.contains(next) {
            next = match chars.next() {
                Option::Some(x) => x,
                Option::None => return None,
            };
        }
        let delim = match next {
            '"' | '\'' => next,
            _ => return None,
        };
        let mut backslashes = 0usize;
        for chr in chars {
            if chr == '\\' {
                backslashes += 1;
            } else if chr == delim {
                if backslashes % 2 == 0 {
                    return None; // String terminates, no multiline
                } else {
                    backslashes = 0;
                }
            } else {
                backslashes = 0;
            }
        }
        Some(delim)
    }

    fn concat_comment(&mut self) -> ParseResult<LineInfo> {
        debug_assert!(self.next.starts_with("#|"));
        let line_info = self.line_info();
        loop {
            // FIXME: Multiline comment ending on same line as comment begin
            if let Option::Some(x) = self.full_line.find("|#") {
                // TODO? clone_into() when stable (#41263)
                self.next = self.full_line[x + 2..].to_owned();
                return ParseResult::Ok(line_info);
            }
            let next_line = self.read_line()?.expect("Unclosed comment at end of file");
            self.full_line = (&*next_line).into();
            self.append_escaped_lines()?;
        }
    }

    fn concat_string(&mut self, delim: char) -> ParseResult<(String, LineInfo)> {
        let line_info = self.line_info();
        let mut sequence = self.next.clone();
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
                        self.full_line = (&*next_line).into();
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
            self.next.pop();
            let next_line = self.read_line()?.unwrap();
            self.next.push_str(&next_line);
            self.full_line = (&*self.next).into();
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
                self.line_number += 1;
                Option::Some(buffer)
            }),
            Result::Err(e) => ParseResult::Err(ParserException::of(e, self.line_info()).into()),
        }
    }

    fn line_number(&self) -> usize {
        self.line_number
    }

    fn line_index(&self) -> usize {
        // FIXME? Deal with non-Ascii chars
        let full_len = self.full_line.len() - self.next.len();
        if self.lb_indices.is_empty() {
            full_len
        } else {
            let index = self.lb_indices.range(..full_len).next_back();
            full_len - *index.unwrap_or(&0)
        }
    }
}

impl Read for Reader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Reader::File(f) => f.read(buf),
            Reader::String(s) => s.read(buf),
        }
    }
}

impl BufRead for Reader {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        match self {
            Reader::File(f) => f.fill_buf(),
            Reader::String(s) => s.fill_buf(),
        }
    }

    fn consume(&mut self, amt: usize) {
        match self {
            Reader::File(f) => f.consume(amt),
            Reader::String(s) => s.consume(amt),
        }
    }

    fn read_line(&mut self, buf: &mut String) -> std::io::Result<usize> {
        match self {
            Reader::File(f) => f.read_line(buf),
            Reader::String(s) => s.read_line(buf),
        }
    }
}
