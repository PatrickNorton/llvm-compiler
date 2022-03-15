use crate::parser::aug_assign::AugAssignTypeNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::dotted::DotPrefix;
use crate::parser::inc_dec::IncDecType;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::number::Number;
use crate::parser::operator::OperatorTypeNode;
use crate::parser::operator_fn::OpFuncTypeNode;
use crate::parser::operator_sp::OpSpTypeNode;
use once_cell::sync::Lazy;
use regex::Regex;
use unicode_xid::UnicodeXID;

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    sequence: String,
    line_info: LineInfo,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenType {
    /// Whitespace. Matches comments, spaces, and escaped newlines. Should
    /// not make it past the tokenizer.
    Whitespace,
    /// End of the file.
    Epsilon,
    /// Newlines,
    Newline,
    /// Descriptor words, such as public or static.
    Descriptor(DescriptorNode),
    /// Language-reserved keywords, like if, try, or enum.
    Keyword(Keyword),
    /// Open braces. Each token this matches corresponds with a [closing brace](b).
    ///
    /// [b]: TokenType::OpenBrace
    OpenBrace(char),
    /// Closing braces. Each token this matches corresponds with an [opening brace](b).
    ///
    /// [b]: TokenType::CloseBrace
    CloseBrace(char),
    /// The comma, such as in between items in a list.
    Comma,
    /// Augmented assignment operators, such as += or -=.
    AugAssign(AugAssignTypeNode),
    /// The magical arrow unicorn, for function return types.
    Arrow,
    /// The even more magical double arrow bi-corn.
    DoubleArrow,
    /// The ellipsis unicorn.
    Ellipsis,
    /// Dots that aren't an ellipsis.
    Dot(DotPrefix),
    /// For increment and decrement operations.
    Increment(IncDecType),
    /// Bog-standard operators, like + or <<
    Operator(OperatorTypeNode),
    /// Assignment, both static and dynamic (:=).
    Assign(bool),
    /// String literals of all sorts.
    String(String),
    /// Numbers in all bases and decimals.
    Number(Number),
    /// Special operator names, for operator overload definitions.
    OperatorSp(OpSpTypeNode),
    /// Variable names.
    Name(String),
    /// Backslash-preceded operator functions, such as \\+ or \\<<.
    OpFunc(OpFuncTypeNode),
    /// Colons, for slices.
    Colon,
    /// The at symbol, for decorators.
    At,
    /// The dollar sign, for annotations.
    Dollar,
}

impl Token {
    pub fn new(token: TokenType, sequence: impl ToString, info: LineInfo) -> Token {
        Token {
            token_type: token,
            sequence: sequence.to_string(),
            line_info: info,
        }
    }

    pub fn deconstruct(self) -> (LineInfo, TokenType) {
        (self.line_info, self.token_type)
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn into_sequence(self) -> String {
        self.sequence
    }

    pub fn get_sequence(&self) -> &str {
        &self.sequence
    }

    pub fn epsilon(info: LineInfo) -> Token {
        Token::new(TokenType::Epsilon, String::new(), info)
    }

    pub fn newline(info: LineInfo) -> Token {
        Token::new(TokenType::Newline, "\n", info)
    }

    pub fn is_whitespace(&self) -> bool {
        matches!(self.token_type, TokenType::Whitespace)
    }

    pub fn equals(&self, text: &str) -> bool {
        self.sequence == text
    }

    pub fn is_kwd(&self, kwd: Keyword) -> bool {
        self.token_type == TokenType::Keyword(kwd)
    }
}

impl TokenType {
    pub fn matchers() -> impl Iterator<Item = MatcherFn> {
        MATCHERS.into_iter()
    }

    /// If this token is one that precedes a "literal brace", e.g. the beginning
    /// of a `dict` or `set` literal (in particular, not a code block).
    pub fn precedes_literal_brace(&self) -> bool {
        matches!(
            self,
            TokenType::OpenBrace(_)
                | TokenType::Newline
                | TokenType::Keyword(_)
                | TokenType::Comma
                | TokenType::Operator(_)
                | TokenType::Colon
                | TokenType::At
                | TokenType::Dollar
                | TokenType::Assign(_)
        )
    }
}

impl Lined for Token {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

type MatcherFn = fn(&str) -> Option<(TokenType, usize)>;

const MATCHER_LEN: usize = 26;
const MATCHERS: [MatcherFn; MATCHER_LEN] = [
    whitespace,
    epsilon,
    newline,
    DescriptorNode::pattern,
    Keyword::pattern,
    open_brace,
    close_brace,
    comma,
    AugAssignTypeNode::pattern,
    arrow,
    double_arrow,
    ellipsis,
    dot,
    arrow,
    double_arrow,
    increment,
    OperatorTypeNode::pattern,
    assign,
    string,
    Number::parse,
    name,
    OpSpTypeNode::pattern,
    OpFuncTypeNode::pattern,
    colon,
    at,
    dollar,
];

static WHITESPACE_PATTERN: Lazy<Regex> =
    Lazy::new(|| Regex::new("^(#\\|(.|[\r\n])*?\\|#|#.*|[\t ]+|\\\\[\r\n])").unwrap());

fn whitespace(input: &str) -> Option<(TokenType, usize)> {
    if let Option::Some(m) = WHITESPACE_PATTERN.find(input) {
        assert_eq!(m.start(), 0);
        Option::Some((TokenType::Whitespace, m.end()))
    } else {
        Option::None
    }
}

fn epsilon(input: &str) -> Option<(TokenType, usize)> {
    input.is_empty().then(|| (TokenType::Epsilon, 0))
}

fn newline(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with("\r\n") {
        Option::Some((TokenType::Newline, 2))
    } else {
        match input.chars().next()? {
            x @ ('\n' | '\x0b' | '\x0c' | '\r' | '\u{85}' | '\u{2028}' | '\u{2029}') => {
                Option::Some((TokenType::Newline, x.len_utf8()))
            }
            _ => Option::None,
        }
    }
}

fn open_brace(input: &str) -> Option<(TokenType, usize)> {
    match input.chars().next()? {
        x @ ('(' | '[' | '{') => Option::Some((TokenType::OpenBrace(x), 1)),
        _ => Option::None,
    }
}

fn close_brace(input: &str) -> Option<(TokenType, usize)> {
    match input.chars().next()? {
        x @ (')' | ']' | '}') => Option::Some((TokenType::CloseBrace(x), 1)),
        _ => Option::None,
    }
}

fn comma(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with(',') {
        Option::Some((TokenType::Comma, 1))
    } else {
        Option::None
    }
}

fn arrow(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with("->") {
        Option::Some((TokenType::Arrow, 2))
    } else {
        Option::None
    }
}

fn double_arrow(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with("=>") {
        Option::Some((TokenType::DoubleArrow, 2))
    } else {
        Option::None
    }
}

fn ellipsis(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with("...") {
        Option::Some((TokenType::Ellipsis, 3))
    } else {
        Option::None
    }
}

fn dot(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with('.') {
        Option::Some((TokenType::Dot(DotPrefix::None), 1))
    } else if input.starts_with("?.") {
        Option::Some((TokenType::Dot(DotPrefix::Question), 2))
    } else if input.starts_with("!!.") {
        Option::Some((TokenType::Dot(DotPrefix::DoubleBang), 3))
    } else {
        Option::None
    }
}

fn increment(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with("++") {
        Option::Some((TokenType::Increment(IncDecType::Plus), 2))
    } else if input.starts_with("--") {
        Option::Some((TokenType::Increment(IncDecType::Minus), 2))
    } else {
        Option::None
    }
}

fn assign(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with('=') {
        Option::Some((TokenType::Assign(false), 1))
    } else if input.starts_with("?=") {
        Option::Some((TokenType::Assign(true), 2))
    } else {
        Option::None
    }
}

const STRING_PREFIXES: &str = "refbcy";

fn string(input: &str) -> Option<(TokenType, usize)> {
    let mut cursor = input.char_indices().peekable();
    while STRING_PREFIXES.contains(cursor.peek()?.1) {
        cursor.next()?;
    }
    let terminator = cursor.next()?.1;
    if terminator != '"' && terminator != '\'' {
        return Option::None;
    }
    let mut backslash_count = 0;
    for (i, chr) in cursor {
        match chr {
            '"' => {
                if terminator == '"' && backslash_count % 2 == 0 {
                    let value = input[..i + 1].to_string();
                    return Option::Some((TokenType::String(value), i + 1));
                }
                backslash_count = 0;
            }
            '\'' => {
                if terminator == '\'' && backslash_count % 2 == 0 {
                    let value = input[..i + 1].to_string();
                    return Option::Some((TokenType::String(value), i + 1));
                }
                backslash_count = 0;
            }
            '\\' => backslash_count += 1,
            _ => backslash_count = 0,
        }
    }
    Option::None
}

fn name(input: &str) -> Option<(TokenType, usize)> {
    if !UnicodeXID::is_xid_start(input.chars().next()?) && !input.starts_with('_') {
        Option::None
    } else {
        let count = input
            .chars()
            .take_while(|&ch| UnicodeXID::is_xid_continue(ch))
            .map(char::len_utf8)
            .sum();
        if count == "operator".len() && input.starts_with("operator") {
            Option::None
        } else if count > 0 {
            Option::Some((TokenType::Name(input[..count].to_string()), count))
        } else {
            Option::None
        }
    }
}

fn colon(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with(':') {
        Option::Some((TokenType::Colon, 1))
    } else {
        Option::None
    }
}

fn at(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with('@') {
        Option::Some((TokenType::At, 1))
    } else {
        Option::None
    }
}

fn dollar(input: &str) -> Option<(TokenType, usize)> {
    if input.starts_with('$') {
        Option::Some((TokenType::Dollar, 1))
    } else {
        Option::None
    }
}
