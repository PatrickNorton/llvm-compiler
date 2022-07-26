use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::{TypeLikeNode, TypeNode};
use crate::parser::typed_var::TypedVariableNode;

use super::error::ParserException;

#[derive(Debug)]
pub struct VariableNode {
    line_info: LineInfo,
    name: String,
}

#[derive(Debug)]
pub enum VarLikeNode {
    Typed(TypedVariableNode),
    Variable(VariableNode),
}

impl VariableNode {
    pub const fn new(line_info: LineInfo, name: String) -> VariableNode {
        VariableNode { line_info, name }
    }

    pub fn empty() -> Self {
        Self {
            line_info: LineInfo::empty(),
            name: String::new(),
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn is_empty(&self) -> bool {
        self.name.is_empty()
    }

    pub fn parse_on_name(tokens: &mut TokenList) -> ParseResult<VariableNode> {
        if matches!(tokens.token_type()?, TokenType::Name(_)) {
            Self::parse(tokens)
        } else {
            ParseResult::Ok(Self::empty())
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<VariableNode> {
        Self::parse_newline(tokens, false)
    }

    pub fn parse_newline(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<VariableNode> {
        let (line_info, token_type) = tokens.next_tok(ignore_newlines)?.deconstruct();
        match token_type {
            TokenType::Name(string) => ParseResult::Ok(VariableNode::new(line_info, string)),
            tok => ParseResult::Err(
                ParserException::of(format!("Expected name, got {:?}", tok), line_info).into(),
            ),
        }
    }

    pub fn parse_on_keyword(tokens: &mut TokenList, keyword: Keyword) -> ParseResult<VariableNode> {
        if let TokenType::Keyword(key) = *(tokens.token_type()?) {
            if key == keyword {
                tokens.next_token()?;
                return Self::parse(tokens);
            }
        }
        Ok(VariableNode::empty())
    }

    pub fn parse_ellipsis(tokens: &mut TokenList) -> ParseResult<VariableNode> {
        let next = tokens.next_token()?;
        assert!(matches!(next.token_type(), TokenType::Ellipsis));
        let info = next.deconstruct().0;
        ParseResult::Ok(VariableNode::new(info, "...".to_string()))
    }
}

impl VarLikeNode {
    pub fn parse_list(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<Vec<VarLikeNode>> {
        let mut vars = Vec::new();
        while TypeNode::next_is_type(tokens)? {
            vars.push(Self::parse(tokens, ignore_newlines)?);
            if !tokens.token_equals(",")? {
                break;
            }
            tokens.next_tok(ignore_newlines)?;
        }
        Ok(vars)
    }

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<VarLikeNode> {
        let size = TypeLikeNode::size_of_type(tokens, 0)?;
        if size == 0 || !matches!(tokens.get_token(size)?.token_type(), TokenType::Name(_)) {
            Ok(VarLikeNode::Variable(VariableNode::parse_newline(
                tokens,
                ignore_newlines,
            )?))
        } else {
            Ok(VarLikeNode::Typed(TypedVariableNode::parse(
                tokens,
                ignore_newlines,
            )?))
        }
    }

    pub fn get_variable(&self) -> &VariableNode {
        match self {
            VarLikeNode::Typed(t) => t.get_variable(),
            VarLikeNode::Variable(v) => v,
        }
    }
}

impl Lined for VariableNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for VarLikeNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            VarLikeNode::Typed(t) => t.line_info(),
            VarLikeNode::Variable(v) => v.line_info(),
        }
    }
}

impl<'a> TryFrom<&'a TestNode> for &'a VariableNode {
    type Error = ();

    fn try_from(value: &'a TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Name(NameNode::Variable(v)) => Ok(v),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a NameNode> for &'a VariableNode {
    type Error = ();

    fn try_from(value: &'a NameNode) -> Result<Self, Self::Error> {
        match value {
            NameNode::Variable(v) => Ok(v),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a VarLikeNode> for &'a VariableNode {
    type Error = ();

    fn try_from(value: &'a VarLikeNode) -> Result<Self, Self::Error> {
        match value {
            VarLikeNode::Variable(v) => Ok(v),
            _ => Err(()),
        }
    }
}
