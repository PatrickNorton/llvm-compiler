use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;

#[derive(Debug)]
pub struct TypedefStatementNode {
    line_info: LineInfo,
    name: TypeNode,
    type_val: TypeNode,
}

impl TypedefStatementNode {
    pub fn new(line_info: LineInfo, name: TypeNode, type_val: TypeNode) -> Self {
        Self {
            line_info,
            name,
            type_val,
        }
    }

    pub fn get_name(&self) -> &TypeNode {
        &self.name
    }

    pub fn get_type(&self) -> &TypeNode {
        &self.type_val
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<TypedefStatementNode> {
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Lambda)));
        let name = TypeNode::parse(tokens)?;
        let (_, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::As)));
        let type_val = TypeNode::parse(tokens)?;
        Ok(TypedefStatementNode::new(line_info, name, type_val))
    }
}

impl Lined for TypedefStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> TryFrom<&'a IndependentNode> for &'a TypedefStatementNode {
    type Error = ();

    fn try_from(value: &'a IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::Typedef(t) => Ok(t),
            _ => Err(()),
        }
    }
}
