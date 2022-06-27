use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::string_like::{StringLikeNode, StringPrefix};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::tokenizer::Tokenizer;
use std::collections::HashSet;
use std::fmt::{Display, Write};
use std::iter::Peekable;
use std::mem::take;
use std::str::Chars;

#[derive(Debug)]
pub struct FormattedStringNode {
    line_info: LineInfo,
    flags: HashSet<StringPrefix>,
    strings: Vec<String>,
    tests: Vec<TestNode>,
    formats: Vec<FormatInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FormatInfo {
    pub fill: char,
    pub align: char,
    pub sign: char,
    pub hash: bool,
    pub zero: bool,
    pub min_width: usize,
    pub precision: usize,
    pub fmt_type: char,
    line_info: LineInfo,
}

impl FormattedStringNode {
    pub fn new(
        line_info: LineInfo,
        flags: HashSet<StringPrefix>,
        strings: Vec<String>,
        tests: Vec<TestNode>,
        formats: Vec<FormatInfo>,
    ) -> Self {
        Self {
            line_info,
            flags,
            strings,
            tests,
            formats,
        }
    }

    pub fn get_strings(&self) -> &[String] {
        &self.strings
    }

    pub fn get_tests(&self) -> &[TestNode] {
        &self.tests
    }

    pub fn get_formats(&self) -> &[FormatInfo] {
        &self.formats
    }

    pub fn parse(text: String, info: LineInfo) -> ParseResult<FormattedStringNode> {
        let mut inside = StringLikeNode::get_contents(&text);
        let prefixes = StringLikeNode::get_prefixes(&text)?;
        let is_raw = prefixes.contains(&StringPrefix::Raw);
        let mut strings = Vec::new();
        let mut tests = Vec::new();
        let mut formats = Vec::new();
        let mut current = String::new();
        while let Option::Some(new_start) = inside.find('{') {
            let (pre, post) = inside.split_at(new_start);
            if is_escaped(pre) {
                current += pre;
                current.push('{'); // Push leading brace with raw text
                inside = &post[1..]; // Get rid of leading brace
                inside = &inside[new_start + 1..];
                continue;
            }
            current += pre;
            strings.push(if is_raw {
                take(&mut current)
            } else {
                let text = StringLikeNode::process_escapes(&current, &info)?;
                current.clear();
                text
            });
            let new_end = match size_of_brace(post, is_raw) {
                Option::Some(x) => new_start + x,
                Option::None => {
                    return Err(ParserError::Normal(ParserException::of(
                        "Unmatched braces in f-string",
                        info,
                    )))
                }
            };
            let (format, size) = FormatInfo::parse(&inside[new_start..new_end - 1], info.clone())?;
            formats.push(format);
            let end = new_end - size;
            tests.push(Self::parse_test(&inside[new_start + 1..end - 1], &info)?);
            inside = &inside[new_end..];
        }
        if !inside.is_empty() {
            strings.push(if is_raw {
                inside.to_string()
            } else {
                StringLikeNode::process_escapes(inside, &info)?
            });
        }
        Ok(Self::new(info, prefixes, strings, tests, formats))
    }

    fn parse_test(section: &str, line_info: &LineInfo) -> ParseResult<TestNode> {
        let mut tokens = Tokenizer::parse_str(
            section,
            line_info.get_path().to_path_buf(),
            line_info.get_line_number(),
        )?;
        let test = TestNode::parse(&mut tokens)?;
        if !matches!(tokens.token_type()?, TokenType::Epsilon) {
            Err(tokens.default_error())
        } else {
            Ok(test)
        }
    }
}

const FORMAT_INVALID: &str = "\"'[](){}";
const ALIGN_VALID: &str = "><=^";
const SIGN_VALID: &str = " +-";
const TYPE_VALID: &str = "bcdoxXneEfFgG%rs";

impl FormatInfo {
    pub fn empty() -> FormatInfo {
        FormatInfo {
            fill: '\0',
            align: '\0',
            sign: '\0',
            hash: false,
            zero: false,
            min_width: 0,
            precision: 0,
            fmt_type: '\0',
            line_info: LineInfo::empty(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.only_type() && self.fmt_type == '\0'
    }

    pub fn only_type(&self) -> bool {
        self.fill == '\0'
            && self.align == '\0'
            && self.sign == '\0'
            && !self.hash
            && !self.zero
            && self.min_width == 0
            && self.precision == 0
    }

    fn parse(text: &str, line_info: LineInfo) -> ParseResult<(FormatInfo, usize)> {
        let (specifier, bang_place) = match Self::specifier(text) {
            Option::Some((s, b)) => (s, b),
            Option::None => return Ok((Self::empty(), 0)),
        };
        if specifier.is_empty() {
            return Ok((Self::empty(), 1));
        }
        let mut chars = specifier.chars().peekable();
        let (fill, align) = if Self::starts_align(chars.clone()) {
            (chars.next().unwrap(), chars.next().unwrap())
        } else {
            (
                '\0',
                chars.next_if(|&x| ALIGN_VALID.contains(x)).unwrap_or('\0'),
            )
        };
        let sign = chars.next_if(|&x| SIGN_VALID.contains(x)).unwrap_or('\0');
        let hash = chars.next_if(|&x| x == '#').is_some();
        let zero = chars.next_if(|&x| x == '0').is_some();
        let min_width = parse_int(&mut chars);
        let precision = if chars.next_if(|&x| x == '.').is_some() {
            parse_int(&mut chars)
        } else {
            0
        };
        let fmt_type = chars.next_if(|&x| TYPE_VALID.contains(x)).unwrap_or('\0');
        let line_info = line_info.substring(bang_place + 2);
        Ok((
            FormatInfo {
                fill,
                align,
                sign,
                hash,
                zero,
                min_width,
                precision,
                fmt_type,
                line_info,
            },
            text.len() - bang_place,
        ))
    }

    fn specifier(text: &str) -> Option<(&str, usize)> {
        let i = text.rfind('!')?;
        let (pre, post) = text.split_at(i);
        if pre.ends_with('!')
            || post.starts_with("!=")
            || post.chars().any(|c| FORMAT_INVALID.contains(c))
        {
            Option::None
        } else {
            Option::Some((&post[1..], i))
        }
    }

    fn starts_align(mut chars: impl Iterator<Item = char>) -> bool {
        chars.next().is_some()
            && chars
                .next()
                .map_or_else(|| false, |x| ALIGN_VALID.contains(x))
    }
}

impl Lined for FormattedStringNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for FormatInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

fn is_escaped(text: &str) -> bool {
    let mut backslash_count = 0usize;
    for chr in text.chars().rev() {
        if chr != '\\' {
            return backslash_count % 2 != 0;
        } else {
            backslash_count += 1;
        }
    }
    backslash_count % 2 != 0
}

fn size_of_brace(text: &str, is_raw: bool) -> Option<usize> {
    let mut net_braces = 0usize;
    for (i, chr) in text.char_indices() {
        match chr {
            '{' => {
                net_braces += 1;
            }
            '}' => {
                if is_raw || net_braces > 1 || !is_escaped(text) {
                    net_braces -= 1;
                }
            }
            _ => {}
        }
        if net_braces == 0 {
            return Some(i + 1);
        }
    }
    None
}

fn parse_int(chars: &mut Peekable<Chars<'_>>) -> usize {
    let mut result = 0;
    while let Option::Some(x) = chars.next_if(|x| x.is_ascii_digit()) {
        result *= 10;
        result += x.to_digit(10).unwrap() as usize;
    }
    result
}

impl Display for FormatInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            return Ok(());
        }
        f.write_char('!')?;
        if self.fill != '\0' {
            f.write_char(self.fill)?;
        }
        if self.align != '\0' {
            f.write_char(self.align)?;
        }
        if self.sign != '\0' {
            f.write_char(self.sign)?;
        }
        if self.hash {
            f.write_char('#')?;
        }
        if self.zero {
            f.write_char('0')?;
        }
        if self.min_width != 0 {
            write!(f, "{}", self.min_width)?;
        }
        if self.precision != 0 {
            write!(f, "{}", self.precision)?;
        }
        if self.fmt_type != '\0' {
            f.write_char(self.fmt_type)?;
        }
        Ok(())
    }
}
