use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::formatted_string::FormattedStringNode;
use crate::parser::line_info::LineInfo;
use crate::parser::string::StringNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use std::collections::HashSet;
use std::str::Chars;

#[derive(Debug)]
pub enum StringLikeNode {
    Standard(StringNode),
    Formatted(FormattedStringNode),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum StringPrefix {
    Formatted,
    Raw,
    Regex,
    Bytes,
    Char,
    Byte,
}

impl StringLikeNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<StringLikeNode> {
        let (info, token) = tokens.next_token()?.deconstruct();
        match token {
            TokenType::String(x) => {
                if Self::get_prefixes(&x)?.contains(&StringPrefix::Formatted) {
                    FormattedStringNode::parse(x, info).map(StringLikeNode::Formatted)
                } else {
                    StringNode::parse(x, info).map(StringLikeNode::Standard)
                }
            }
            _ => panic!("Expected a string"),
        }
    }

    pub fn get_prefixes(text: &str) -> ParseResult<HashSet<StringPrefix>> {
        StringPrefix::get_prefixes(&text[..Self::prefix_count(text)])
    }

    pub fn get_contents(text: &str) -> &str {
        let prefix_count = Self::prefix_count(text);
        &text[prefix_count + 1..text.len() - 1]
    }

    pub fn process_escapes(text: &str, info: &LineInfo) -> ParseResult<String> {
        let mut sb = String::with_capacity(text.len());
        let mut trailing_backslash = false;
        for slice in text.split('\\') {
            if !trailing_backslash {
                sb.push_str(slice);
                trailing_backslash = true;
            } else {
                let mut chars = slice.chars();
                match chars.next() {
                    // None means multiple backslashes in a row
                    None => {
                        sb.push('\\');
                        trailing_backslash = false;
                    }
                    Some(chr) => {
                        if let Option::Some(ch) = Self::escape_to_chr(chr, &mut chars, info)? {
                            sb.push(ch);
                        }
                        sb.push_str(chars.as_str());
                        trailing_backslash = true;
                    }
                }
            }
        }
        Ok(sb)
    }

    fn escape_to_chr(
        signifier: char,
        chars: &mut Chars<'_>,
        info: &LineInfo,
    ) -> ParseResult<Option<char>> {
        match signifier {
            '\n' => Ok(None),
            '\\' => Ok(Some('\\')),
            '"' => Ok(Some('"')),
            '{' => Ok(Some('{')),
            '}' => Ok(Some('}')),
            '\'' => Ok(Some('\'')),
            '0' => Ok(Some('\0')),
            'a' => Ok(Some('\x07')),
            'b' => Ok(Some('\x08')),
            'f' => Ok(Some('\x0c')),
            'n' => Ok(Some('\n')),
            'r' => Ok(Some('\r')),
            't' => Ok(Some('\t')),
            'v' => Ok(Some('\x0b')),
            'o' => Ok(Some(Self::process_octal_literal(info, chars)?)),
            'x' => Ok(Some(Self::process_hex_literal(info, chars)?)),
            'u' => Ok(Some(Self::process_unicode_literal(info, chars)?)),
            'U' => Ok(Some(Self::process_big_unicode_literal(info, chars)?)),
            x => Err(ParserError::Normal(ParserException::of(
                format!("Unknown escape sequence: \\{x}"),
                info,
            ))),
        }
    }

    fn process_octal_literal(info: &LineInfo, chars: &mut Chars<'_>) -> ParseResult<char> {
        let ch1 = chars.next().unwrap();
        let ch2 = chars.next().unwrap();
        let dig1 = Self::ch_to_digit(ch1, 8, info, "octal")?;
        let dig2 = Self::ch_to_digit(ch2, 8, info, "octal")?;
        match char::from_u32(dig1 * 8 + dig2) {
            Option::Some(x) => Ok(x),
            Option::None => Err(ParserError::Normal(ParserException::of(
                format!(
                    "Invalid character for octal literal: \\o{:2o} is not a valid char",
                    dig1 * 8 + dig2
                ),
                info,
            ))),
        }
    }

    fn process_hex_literal(info: &LineInfo, chars: &mut Chars<'_>) -> ParseResult<char> {
        let ch1 = chars.next().unwrap();
        let ch2 = chars.next().unwrap();
        let dig1 = Self::ch_to_digit(ch1, 16, info, "hex")?;
        let dig2 = Self::ch_to_digit(ch2, 16, info, "hex")?;
        match char::from_u32(dig1 * 16 + dig2) {
            Option::Some(x) => Ok(x),
            Option::None => Err(ParserError::Normal(ParserException::of(
                format!(
                    "Invalid character for hex literal: \\x{:2x} is not a valid char",
                    dig1 * 16 + dig2
                ),
                info,
            ))),
        }
    }

    fn process_unicode_literal(info: &LineInfo, chars: &mut Chars<'_>) -> ParseResult<char> {
        let ch1 = chars.next().unwrap();
        let ch2 = chars.next().unwrap();
        let ch3 = chars.next().unwrap();
        let ch4 = chars.next().unwrap();
        let dig1 = Self::ch_to_digit(ch1, 16, info, "unicode")?;
        let dig2 = Self::ch_to_digit(ch2, 16, info, "unicode")?;
        let dig3 = Self::ch_to_digit(ch3, 16, info, "unicode")?;
        let dig4 = Self::ch_to_digit(ch4, 16, info, "unicode")?;
        match char::from_u32(Self::from_be_digits(16, &[dig1, dig2, dig3, dig4])) {
            Option::Some(x) => Ok(x),
            Option::None => Err(ParserError::Normal(ParserException::of(
                format!(
                    "Invalid character for hex literal: \\x{:2x} is not a valid char",
                    dig1 * 16 + dig2
                ),
                info,
            ))),
        }
    }

    fn process_big_unicode_literal(info: &LineInfo, chars: &mut Chars<'_>) -> ParseResult<char> {
        let mut digs = [0; 8];
        for dig in &mut digs {
            let ch = chars.next().unwrap();
            *dig = Self::ch_to_digit(ch, 16, info, "unicode")?;
        }
        let u32_char = Self::from_be_digits(16, &digs);
        match char::from_u32(u32_char) {
            Option::Some(x) => Ok(x),
            Option::None => Err(ParserError::Normal(ParserException::of(
                format!("Invalid character for hex literal: \\U{u32_char:4x} is not a valid char"),
                info,
            ))),
        }
    }

    fn prefix_count(text: &str) -> usize {
        text.find(Self::delimiter(text)).unwrap()
    }

    fn delimiter(text: &str) -> char {
        return text.chars().last().unwrap();
    }

    fn ch_to_digit(c: char, radix: u32, info: &LineInfo, literal_type: &str) -> ParseResult<u32> {
        match c.to_digit(radix) {
            Option::Some(x) => Ok(x),
            Option::None => Err(ParserError::Normal(ParserException::of(
                format!("Invalid character for {literal_type} literal: {c}"),
                info,
            ))),
        }
    }

    #[inline]
    fn from_be_digits(radix: u32, digits: &[u32]) -> u32 {
        let mut result = 0;
        for &digit in digits {
            result *= radix;
            result += digit;
        }
        result
    }
}

impl StringPrefix {
    const INVALID_TOGETHER: [StringPrefix; 4] = [
        StringPrefix::Regex,
        StringPrefix::Bytes,
        StringPrefix::Char,
        StringPrefix::Byte,
    ];
    const INVALID_2: [StringPrefix; 3] = [
        StringPrefix::Formatted,
        StringPrefix::Char,
        StringPrefix::Byte,
    ];

    pub const fn value(&self) -> char {
        match self {
            StringPrefix::Formatted => 'f',
            StringPrefix::Raw => 'r',
            StringPrefix::Regex => 'e',
            StringPrefix::Bytes => 'b',
            StringPrefix::Char => 'c',
            StringPrefix::Byte => 'y',
        }
    }

    pub const fn get_prefix(c: char) -> Option<StringPrefix> {
        match c {
            'f' => Some(StringPrefix::Formatted),
            'r' => Some(StringPrefix::Raw),
            'e' => Some(StringPrefix::Regex),
            'b' => Some(StringPrefix::Bytes),
            'c' => Some(StringPrefix::Char),
            'y' => Some(StringPrefix::Byte),
            _ => None,
        }
    }

    fn get_prefixes(chars: &str) -> ParseResult<HashSet<StringPrefix>> {
        let mut prefixes = HashSet::new();
        let mut has_unique = false;
        let mut has_unique_2 = false;
        for c in chars.chars() {
            let prefix = Self::get_prefix(c).unwrap();
            if prefixes.contains(&prefix) {
                return Err(ParserException::of(
                    "Invalid prefix combination ".to_owned() + chars,
                    LineInfo::empty(),
                )
                .into());
            }
            if Self::INVALID_TOGETHER.contains(&prefix) {
                if has_unique {
                    return Err(ParserException::with_note(
                        format!("Invalid prefix combination: {chars}"),
                        "The characters 'rbcy' may not appear in a string prefix together",
                        LineInfo::empty(),
                    )
                    .into());
                } else {
                    has_unique = true;
                }
            }
            if Self::INVALID_2.contains(&prefix) {
                if has_unique_2 {
                    return Err(ParserException::with_note(
                        format!("Invalid prefix combination: {chars}"),
                        "The characters 'fcy' may not appear in a string prefix together",
                        LineInfo::empty(),
                    )
                    .into());
                } else {
                    has_unique_2 = true;
                }
            }
            prefixes.insert(prefix);
        }
        Ok(prefixes)
    }
}

impl From<StringLikeNode> for TestNode {
    fn from(x: StringLikeNode) -> Self {
        match x {
            StringLikeNode::Standard(s) => TestNode::String(s),
            StringLikeNode::Formatted(s) => TestNode::Formatted(s),
        }
    }
}
