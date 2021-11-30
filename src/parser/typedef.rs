use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::{TypeLikeNode, TypeNode};

#[derive(Debug)]
pub struct TypedefStatementNode {
    line_info: LineInfo,
    name: TypeNode,
    type_val: TypeLikeNode,
}

impl TypedefStatementNode {
    pub fn new(line_info: LineInfo, name: TypeNode, type_val: TypeLikeNode) -> Self {
        Self {
            line_info,
            name,
            type_val,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<TypedefStatementNode> {
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Lambda)));
        let name = TypeNode::parse(tokens)?;
        let (_, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::As)));
        let type_val = TypeLikeNode::parse(tokens, false)?;
        Ok(TypedefStatementNode::new(line_info, name, type_val))
    }
}

impl Lined for TypedefStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
