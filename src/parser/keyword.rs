use crate::macros::hash_map;
use crate::parser::assert::AssertStatementNode;
use crate::parser::base::IndependentNode;
use crate::parser::break_stmt::BreakStatementNode;
use crate::parser::class_def::ClassDefinitionNode;
use crate::parser::context::ContextDefinitionNode;
use crate::parser::continue_stmt::ContinueStatementNode;
use crate::parser::defer::DeferStatementNode;
use crate::parser::delete::DeleteStatementNode;
use crate::parser::do_stmt::DoStatementNode;
use crate::parser::dotimes::DotimesStatementNode;
use crate::parser::enum_def::EnumDefinitionNode;
use crate::parser::error::ParseResult;
use crate::parser::for_loop::ForStatementNode;
use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::generalizable::parse_generalizable;
use crate::parser::if_stmt::IfStatementNode;
use crate::parser::import::ImportExportNode;
use crate::parser::interface::InterfaceDefinitionNode;
use crate::parser::lambda::LambdaNode;
use crate::parser::method::MethodDefinitionNode;
use crate::parser::property::PropertyDefinitionNode;
use crate::parser::raise_stmt::RaiseStatementNode;
use crate::parser::return_stmt::ReturnStatementNode;
use crate::parser::switch_stmt::SwitchStatementNode;
use crate::parser::synchronized::SynchronizedStatementNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::try_stmt::TryStatementNode;
use crate::parser::typedef::TypedefStatementNode;
use crate::parser::union_def::UnionDefinitionNode;
use crate::parser::while_stmt::WhileStatementNode;
use crate::parser::with_stmt::WithStatementNode;
use crate::parser::yield_stmt::YieldStatementNode;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use unicode_xid::UnicodeXID;

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
    Nobreak,
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
        "nobreak" => Keyword::Nobreak,
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
            if input.starts_with(key)
                && input[key.len()..]
                    .chars()
                    .next()
                    .map_or(true, |x| !UnicodeXID::is_xid_continue(x))
            {
                return Option::Some((TokenType::Keyword(*value), key.len()));
            }
        }
        Option::None
    }

    pub fn parse_left(self, tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        debug_assert_eq!(tokens.token_type()?, &TokenType::Keyword(self));
        match self {
            Keyword::Class => ClassDefinitionNode::parse(tokens).map(IndependentNode::ClassDef),
            Keyword::Func => {
                FunctionDefinitionNode::parse(tokens).map(IndependentNode::FunctionDef)
            }
            Keyword::If => IfStatementNode::parse(tokens).map(IndependentNode::If),
            Keyword::For => ForStatementNode::parse(tokens).map(IndependentNode::For),
            Keyword::Elif => Err(tokens.error("elif must have a preceding if")),
            Keyword::Else => Err(tokens.error("else must have a preceding if")),
            Keyword::Do => DoStatementNode::parse(tokens).map(IndependentNode::Do),
            Keyword::Dotimes => DotimesStatementNode::parse(tokens).map(IndependentNode::Dotimes),
            Keyword::Method => MethodDefinitionNode::parse(tokens).map(IndependentNode::Method),
            Keyword::While => WhileStatementNode::parse(tokens).map(IndependentNode::While),
            Keyword::In => Err(tokens.error("in does not begin any statements")),
            Keyword::From => ImportExportNode::parse(tokens).map(IndependentNode::Import),
            Keyword::Import => ImportExportNode::parse(tokens).map(IndependentNode::Import),
            Keyword::Export => ImportExportNode::parse(tokens).map(IndependentNode::Import),
            Keyword::Typeget => ImportExportNode::parse(tokens).map(IndependentNode::Import),
            Keyword::Break => BreakStatementNode::parse(tokens).map(IndependentNode::Break),
            Keyword::Continue => {
                ContinueStatementNode::parse(tokens).map(IndependentNode::Continue)
            }
            Keyword::Nobreak => Err(tokens.error("nobreak must be part of a loop")),
            Keyword::Return => ReturnStatementNode::parse(tokens).map(IndependentNode::Return),
            Keyword::Property => {
                PropertyDefinitionNode::parse(tokens).map(IndependentNode::Property)
            }
            Keyword::Enter => Err(tokens.error("enter must be in a property block")),
            Keyword::Exit => Err(tokens.error("exit must be in a property block")),
            Keyword::Try => TryStatementNode::parse(tokens).map(IndependentNode::Try),
            Keyword::Except => Err(tokens.error("except must be in a try statement")),
            Keyword::Finally => Err(tokens.error("finally must be in a try statement")),
            Keyword::With => WithStatementNode::parse(tokens).map(IndependentNode::With),
            Keyword::As => Err(tokens.error("as must be with a with or import/typeget")),
            Keyword::Assert => AssertStatementNode::parse(tokens).map(IndependentNode::Assert),
            Keyword::Del => DeleteStatementNode::parse(tokens).map(IndependentNode::Delete),
            Keyword::Yield => YieldStatementNode::parse(tokens).map(IndependentNode::Yield),
            Keyword::Context => ContextDefinitionNode::parse(tokens).map(IndependentNode::Context),
            Keyword::Lambda => LambdaNode::parse(tokens, false)
                .map(TestNode::Lambda)
                .map(IndependentNode::Test),
            Keyword::Raise => RaiseStatementNode::parse(tokens, false)
                .map(TestNode::Raise)
                .map(IndependentNode::Test),
            Keyword::Typedef => TypedefStatementNode::parse(tokens).map(IndependentNode::Typedef),
            Keyword::Some => TestNode::parse(tokens).map(IndependentNode::Test),
            Keyword::Interface => {
                InterfaceDefinitionNode::parse(tokens).map(IndependentNode::Interface)
            }
            Keyword::Switch => SwitchStatementNode::parse(tokens)
                .map(TestNode::Switch)
                .map(IndependentNode::Test),
            Keyword::Case => Err(tokens.error("Case statements are illegal outside switch")),
            Keyword::Enum => EnumDefinitionNode::parse(tokens).map(IndependentNode::Enum),
            Keyword::Default => Err(tokens.error("Default statements are illegal outside switch")),
            Keyword::Goto => Err(tokens.error("This language doesn't use goto, go use C++")),
            Keyword::Defer => DeferStatementNode::parse(tokens).map(IndependentNode::Defer),
            Keyword::Var => IndependentNode::parse_var(tokens),
            Keyword::Sync => SynchronizedStatementNode::parse(tokens).map(IndependentNode::Sync),
            Keyword::Generic => parse_generalizable(tokens),
            Keyword::Union => UnionDefinitionNode::parse(tokens).map(IndependentNode::Union),
        }
    }

    pub const fn name(self) -> &'static str {
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
            Keyword::Nobreak => "nobreak",
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
