use crate::hash_map;
use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Keyword {
    Class,
    Func,
    If,
    For,
    Elif,
    Else,
    Do,
    Dotimes,
    Method,
    While,
    In,
    From,
    Import,
    Export,
    Typeget,
    Break,
    Continue,
    Return,
    Property,
    Enter,
    Exit,
    Try,
    Except,
    Finally,
    With,
    As,
    Assert,
    Del,
    Yield,
    Context,
    Lambda,
    Raise,
    Typedef,
    Some,
    Interface,
    Switch,
    Case,
    Enum,
    Default,
    Goto,
    Defer,
    Var,
    Sync,
    Generic,
    Union,
}

static VALUES: Lazy<HashMap<&'static str, Keyword>> = Lazy::new(|| {
    hash_map!(
        "class" => Keyword::Class,
        "func" => Keyword::Func,
        "if" => Keyword::If,
        "for" => Keyword::For,
        "elif" => Keyword::Elif,
        "else" => Keyword::Else,
        "do" => Keyword::Do,
        "dotimes" => Keyword::Dotimes,
        "method" => Keyword::Method,
        "while" => Keyword::While,
        "in" => Keyword::In,
        "from" => Keyword::From,
        "import" => Keyword::Import,
        "export" => Keyword::Export,
        "typeget" => Keyword::Typeget,
        "break" => Keyword::Break,
        "continue" => Keyword::Continue,
        "return" => Keyword::Return,
        "property" => Keyword::Property,
        "enter" => Keyword::Enter,
        "exit" => Keyword::Exit,
        "try" => Keyword::Try,
        "except" => Keyword::Except,
        "finally" => Keyword::Finally,
        "with" => Keyword::With,
        "as" => Keyword::As,
        "assert" => Keyword::Assert,
        "del" => Keyword::Del,
        "yield" => Keyword::Yield,
        "context" => Keyword::Context,
        "lambda" => Keyword::Lambda,
        "raise" => Keyword::Raise,
        "typedef" => Keyword::Typedef,
        "some" => Keyword::Some,
        "interface" => Keyword::Interface,
        "switch" => Keyword::Switch,
        "case" => Keyword::Case,
        "enum" => Keyword::Enum,
        "default" => Keyword::Default,
        "goto" => Keyword::Goto,
        "defer" => Keyword::Defer,
        "var" => Keyword::Var,
        "sync" => Keyword::Sync,
        "generic" => Keyword::Generic,
        "union" => Keyword::Union,
    )
});

impl Keyword {
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        for (key, value) in &*VALUES {
            if input.starts_with(key) {
                return Option::Some((TokenType::Keyword(*value), input.len()));
            }
        }
        Option::None
    }

    pub fn parse_left(self, tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        debug_assert_eq!(tokens.token_type()?, &TokenType::Keyword(self));
        todo!("Keyword parsing")
    }

    pub fn name(self) -> &'static str {
        match self {
            Keyword::Class => "class",
            Keyword::Func => "func",
            Keyword::If => "if",
            Keyword::For => "for",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
            Keyword::Do => "do",
            Keyword::Dotimes => "dotimes",
            Keyword::Method => "method",
            Keyword::While => "while",
            Keyword::In => "in",
            Keyword::From => "from",
            Keyword::Import => "import",
            Keyword::Export => "export",
            Keyword::Typeget => "typeget",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Return => "return",
            Keyword::Property => "property",
            Keyword::Enter => "enter",
            Keyword::Exit => "exit",
            Keyword::Try => "try",
            Keyword::Except => "except",
            Keyword::Finally => "finally",
            Keyword::With => "with",
            Keyword::As => "as",
            Keyword::Assert => "assert",
            Keyword::Del => "del",
            Keyword::Yield => "yield",
            Keyword::Context => "context",
            Keyword::Lambda => "lambda",
            Keyword::Raise => "raise",
            Keyword::Typedef => "typedef",
            Keyword::Some => "some",
            Keyword::Interface => "interface",
            Keyword::Switch => "switch",
            Keyword::Case => "case",
            Keyword::Enum => "enum",
            Keyword::Default => "default",
            Keyword::Goto => "goto",
            Keyword::Defer => "defer",
            Keyword::Var => "var",
            Keyword::Sync => "sync",
            Keyword::Generic => "generic",
            Keyword::Union => "union",
        }
    }
}
