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
    pub align: AlignType,
    pub sign: FormatSign,
    pub hash: bool,
    pub zero: bool,
    pub min_width: usize,
    pub precision: usize,
    pub fmt_type: FormatType,
    line_info: LineInfo,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub enum AlignType {
    #[default]
    None,
    Left,
    Right,
    AfterSign,
    Center,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub enum FormatSign {
    #[default]
    None,
    Plus,
    Minus,
    Space,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub enum FormatType {
    #[default]
    None,
    Binary,
    Character,
    Decimal,
    Octal,
    LowerHex,
    UpperHex,
    Number,
    Scientific,
    UpperSci,
    Fixed,
    UpperFixed,
    General,
    UpperGeneral,
    Percentage,
    Str,
    Repr,
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

    pub fn get_flags(&self) -> &HashSet<StringPrefix> {
        &self.flags
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

impl FormatInfo {
    pub fn empty() -> FormatInfo {
        FormatInfo {
            fill: '\0',
            align: AlignType::None,
            sign: FormatSign::None,
            hash: false,
            zero: false,
            min_width: 0,
            precision: 0,
            fmt_type: FormatType::None,
            line_info: LineInfo::empty(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.only_type() && self.fmt_type == FormatType::None
    }

    pub fn only_type(&self) -> bool {
        self.fill == '\0'
            && self.align == AlignType::None
            && self.sign == FormatSign::None
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
        // TODO: Reduce double-parsing here
        let (fill, align) = if Self::starts_align(chars.clone()) {
            (
                chars.next().unwrap(),
                AlignType::from_char(chars.next().unwrap()).unwrap(),
            )
        } else {
            ('\0', AlignType::parse(&mut chars))
        };
        let sign = FormatSign::parse(&mut chars);
        let hash = chars.next_if(|&x| x == '#').is_some();
        let zero = chars.next_if(|&x| x == '0').is_some();
        let min_width = parse_int(&mut chars);
        let precision = if chars.next_if(|&x| x == '.').is_some() {
            parse_int(&mut chars)
        } else {
            0
        };
        let fmt_type = FormatType::parse(&mut chars);
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
        chars.next().is_some() && chars.next().and_then(AlignType::from_char).is_some()
    }
}

impl AlignType {
    pub fn parse(chars: &mut Peekable<impl Iterator<Item = char>>) -> Self {
        match chars.peek().and_then(|&x| Self::from_char(x)) {
            Option::None => Self::None,
            Option::Some(val) => {
                chars.next();
                val
            }
        }
    }

    pub const fn from_char(x: char) -> Option<Self> {
        match x {
            '<' => Some(Self::Left),
            '>' => Some(Self::Right),
            '=' => Some(Self::AfterSign),
            '^' => Some(Self::Center),
            _ => None,
        }
    }

    pub const fn to_char(self) -> Option<char> {
        match self {
            AlignType::None => None,
            AlignType::Left => Some('<'),
            AlignType::Right => Some('>'),
            AlignType::AfterSign => Some('='),
            AlignType::Center => Some('^'),
        }
    }

    pub const fn to_byte(self) -> u8 {
        match self {
            AlignType::None | AlignType::Left => b'<',
            AlignType::Right => b'>',
            AlignType::AfterSign => b'=',
            AlignType::Center => b'^',
        }
    }
}

impl FormatSign {
    pub fn parse(chars: &mut Peekable<impl Iterator<Item = char>>) -> Self {
        match chars.peek().and_then(|&x| Self::from_char(x)) {
            Option::None => Self::None,
            Option::Some(val) => {
                chars.next();
                val
            }
        }
    }

    pub const fn from_char(x: char) -> Option<Self> {
        match x {
            '+' => Some(Self::Plus),
            '-' => Some(Self::Minus),
            ' ' => Some(Self::Space),
            _ => None,
        }
    }

    pub const fn to_char(self) -> Option<char> {
        match self {
            FormatSign::None => None,
            FormatSign::Plus => Some('+'),
            FormatSign::Minus => Some('-'),
            FormatSign::Space => Some(' '),
        }
    }

    pub const fn to_byte(self) -> u8 {
        match self {
            FormatSign::None => b'-',
            FormatSign::Plus => b'+',
            FormatSign::Minus => b'-',
            FormatSign::Space => b' ',
        }
    }
}

impl FormatType {
    pub fn parse(chars: &mut Peekable<impl Iterator<Item = char>>) -> Self {
        match chars.peek().and_then(|&x| Self::from_char(x)) {
            Option::None => Self::None,
            Option::Some(val) => {
                chars.next();
                val
            }
        }
    }

    pub const fn from_char(x: char) -> Option<Self> {
        match x {
            'b' => Some(Self::Binary),
            'c' => Some(Self::Character),
            'd' => Some(Self::Decimal),
            'o' => Some(Self::Octal),
            'x' => Some(Self::LowerHex),
            'X' => Some(Self::UpperHex),
            'n' => Some(Self::Number),
            'e' => Some(Self::Scientific),
            'E' => Some(Self::UpperSci),
            'f' => Some(Self::Fixed),
            'F' => Some(Self::UpperFixed),
            'g' => Some(Self::General),
            'G' => Some(Self::UpperGeneral),
            '%' => Some(Self::Percentage),
            's' => Some(Self::Str),
            'r' => Some(Self::Repr),
            _ => None,
        }
    }

    pub const fn to_char(self) -> Option<char> {
        match self {
            Self::None => None,
            Self::Binary => Some('b'),
            Self::Character => Some('c'),
            Self::Decimal => Some('d'),
            Self::Octal => Some('o'),
            Self::LowerHex => Some('x'),
            Self::UpperHex => Some('X'),
            Self::Number => Some('n'),
            Self::Scientific => Some('e'),
            Self::UpperSci => Some('E'),
            Self::Fixed => Some('f'),
            Self::UpperFixed => Some('F'),
            Self::General => Some('g'),
            Self::UpperGeneral => Some('G'),
            Self::Percentage => Some('%'),
            Self::Str => Some('s'),
            Self::Repr => Some('r'),
        }
    }

    pub const fn to_byte(self) -> u8 {
        match self {
            Self::None => b's',
            Self::Binary => b'b',
            Self::Character => b'c',
            Self::Decimal => b'd',
            Self::Octal => b'o',
            Self::LowerHex => b'x',
            Self::UpperHex => b'X',
            Self::Number => b'n',
            Self::Scientific => b'e',
            Self::UpperSci => b'E',
            Self::Fixed => b'f',
            Self::UpperFixed => b'F',
            Self::General => b'g',
            Self::UpperGeneral => b'G',
            Self::Percentage => b'%',
            Self::Str => b's',
            Self::Repr => b'r',
        }
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
        if let Option::Some(chr) = self.align.to_char() {
            f.write_char(chr)?;
        }
        if let Option::Some(chr) = self.sign.to_char() {
            f.write_char(chr)?;
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
        if let Option::Some(chr) = self.fmt_type.to_char() {
            f.write_char(chr)?;
        }
        Ok(())
    }
}
