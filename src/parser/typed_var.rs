use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::{TypeLikeNode, TypeNode};
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct TypedVariableNode {
    line_info: LineInfo,
    type_node: TypeLikeNode,
    var: VariableNode,
}

impl TypedVariableNode {
    pub fn new(type_node: TypeLikeNode, var: VariableNode) -> TypedVariableNode {
        TypedVariableNode {
            line_info: type_node.line_info().clone(),
            type_node,
            var,
        }
    }

    pub fn get_type(&self) -> &TypeLikeNode {
        &self.type_node
    }

    pub fn get_variable(&self) -> &VariableNode {
        &self.var
    }

    pub fn get_name(&self) -> &str {
        self.var.get_name()
    }

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TypedVariableNode> {
        let type_node = TypeLikeNode::parse(tokens, ignore_newlines)?;
        let var = VariableNode::parse_newline(tokens, ignore_newlines)?;
        Ok(TypedVariableNode::new(type_node, var))
    }

    pub fn parse_list(tokens: &mut TokenList) -> ParseResult<Vec<TypedVariableNode>> {
        let mut vars = Vec::new();
        while TypeNode::next_is_type(tokens)? {
            vars.push(Self::parse(tokens, false)?);
            if !tokens.token_equals(",")? {
                break;
            }
            tokens.next_token()?;
        }
        Ok(vars)
    }

    pub fn parse_list_on_keyword(
        tokens: &mut TokenList,
        keyword: Keyword,
    ) -> ParseResult<Vec<TypedVariableNode>> {
        if parse_if_matches!(tokens, TokenType::Keyword(k) if k == &keyword)?.is_some() {
            Self::parse_list(tokens)
        } else {
            Ok(Vec::new())
        }
    }
}

impl Lined for TypedVariableNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
