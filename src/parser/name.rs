use crate::parser::dotted::DottedVariableNode;
use crate::parser::error::ParseResult;
use crate::parser::fn_call::FunctionCallNode;
use crate::parser::index::IndexNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator_fn::EscapedOperatorNode;
use crate::parser::operator_sp::SpecialOpNameNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub enum NameNode {
    Dotted(DottedVariableNode),
    EscapedOp(EscapedOperatorNode),
    Function(FunctionCallNode),
    Index(IndexNode),
    SpecialOp(SpecialOpNameNode),
    Variable(VariableNode),
}

impl NameNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<NameNode> {
        Self::parse_newline(tokens, true)
    }

    pub fn parse_newline(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<NameNode> {
        assert!(matches!(tokens.token_type()?, TokenType::Name(_)) || tokens.token_equals("(")?);
        let name = match tokens.token_type()? {
            TokenType::Name(_) => NameNode::Variable(VariableNode::parse(tokens)?),
            _ => {
                assert!(tokens.token_equals("(")?);
                tokens.next_tok(true)?;
                let name = Self::parse_newline(tokens, true)?;
                tokens.expect(")", true)?;
                name
            }
        };
        Self::parse_post(tokens, name, ignore_newlines)
    }

    fn parse_post(
        tokens: &mut TokenList,
        name: NameNode,
        ignore_newlines: bool,
    ) -> ParseResult<NameNode> {
        let value = Self::parse_post_braces(tokens, name, ignore_newlines)?;
        DottedVariableNode::parse_name_post_dots(tokens, value, ignore_newlines)
    }

    pub(super) fn parse_post_braces(
        tokens: &mut TokenList,
        name: NameNode,
        ignore_newlines: bool,
    ) -> ParseResult<NameNode> {
        let new_name = TestNode::parse_post_braces(tokens, TestNode::Name(name), ignore_newlines)?;
        if let TestNode::Name(name) = new_name {
            ParseResult::Ok(name)
        } else {
            ParseResult::Err(tokens.internal_error("Error in post-brace parsing"))
        }
    }
}

impl Lined for NameNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            NameNode::Dotted(d) => d.line_info(),
            NameNode::EscapedOp(e) => e.line_info(),
            NameNode::Function(f) => f.line_info(),
            NameNode::Index(i) => i.line_info(),
            NameNode::SpecialOp(s) => s.line_info(),
            NameNode::Variable(v) => v.line_info(),
        }
    }
}
