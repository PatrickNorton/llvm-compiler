use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::formatted_string::FormattedStringNode;
use crate::parser::line_info::LineInfo;
use crate::parser::string::StringNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use std::collections::HashSet;
use std::str::CharIndices;

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
        let mut chars = text.char_indices();
        while let Option::Some((i, chr)) = chars.next() {
            if chr != '\\' {
                sb.push(chr);
                continue;
            }
            let (_, chr2) = chars.next().unwrap();
            match chr2 {
                '\n' => {}
                '\\' => sb.push('\\'),
                '"' => sb.push('"'),
                '{' => sb.push('{'),
                '}' => sb.push('}'),
                '\'' => sb.push('\''),
                '0' => sb.push('\0'),
                'a' => sb.push('\x07'),
                'b' => sb.push('\x08'),
                'f' => sb.push('\x0c'),
                'n' => sb.push('\n'),
                'r' => sb.push('\r'),
                't' => sb.push('\t'),
                'v' => sb.push('\x0b'),
                'o' => sb.push(Self::process_octal_literal(info, &mut chars)?),
                'x' => sb.push(Self::process_hex_literal(info, &mut chars)?),
                'u' => sb.push(Self::process_unicode_literal(info, &mut chars)?),
                'U' => sb.push(Self::process_big_unicode_literal(info, &mut chars)?),
                _ => {
                    return Err(ParserError::Normal(ParserException::of(
                        format!("Unknown escape sequence: {}", &text[i..i + 2]),
                        info,
                    )))
                }
            }
        }
        ParseResult::Ok(sb)
    }

    fn process_octal_literal(info: &LineInfo, chars: &mut CharIndices<'_>) -> ParseResult<char> {
        let ch1 = chars.next().unwrap().1;
        let ch2 = chars.next().unwrap().1;
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

    fn process_hex_literal(info: &LineInfo, chars: &mut CharIndices<'_>) -> ParseResult<char> {
        let ch1 = chars.next().unwrap().1;
        let ch2 = chars.next().unwrap().1;
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

    fn process_unicode_literal(info: &LineInfo, chars: &mut CharIndices<'_>) -> ParseResult<char> {
        let ch1 = chars.next().unwrap().1;
        let ch2 = chars.next().unwrap().1;
        let ch3 = chars.next().unwrap().1;
        let ch4 = chars.next().unwrap().1;
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

    fn process_big_unicode_literal(
        info: &LineInfo,
        chars: &mut CharIndices<'_>,
    ) -> ParseResult<char> {
        let mut digs = [0; 8];
        for dig in &mut digs {
            let ch = chars.next().unwrap().1;
            *dig = Self::ch_to_digit(ch, 16, info, "unicode")?;
        }
        let u32_char = Self::from_be_digits(16, &digs);
        match char::from_u32(u32_char) {
            Option::Some(x) => Ok(x),
            Option::None => Err(ParserError::Normal(ParserException::of(
                format!(
                    "Invalid character for hex literal: \\U{:4x} is not a valid char",
                    u32_char
                ),
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
                format!("Invalid character for {} literal: {}", literal_type, c),
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
                    return Err(ParserException::of(
                        format!(
                            "Invalid prefix combination: {}\n\
                        Note: The characters 'rbcy' may not appear in a string prefix together",
                            chars
                        ),
                        LineInfo::empty(),
                    )
                    .into());
                } else {
                    has_unique = true;
                }
            }
            if Self::INVALID_2.contains(&prefix) {
                if has_unique_2 {
                    return Err(ParserException::of(
                        format!(
                            "Invalid prefix combination: {}\n\
                        Note: The characters 'fcy' may not appear in a string prefix together",
                            chars
                        ),
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
