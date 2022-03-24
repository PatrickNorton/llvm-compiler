use crate::parser::name::NameNode;
use crate::parser::token::TokenType;

use super::base::IndependentNode;
use super::definition::DefinitionNode;
use super::error::ParseResult;
use super::token_list::TokenList;

pub fn parse_left_decorator(tokens: &mut TokenList) -> ParseResult<DefinitionNode> {
    assert!(matches!(tokens.token_type()?, TokenType::At));
    let mut decorators = Vec::new();
    while let TokenType::At = tokens.token_type()? {
        tokens.next_token()?;
        decorators.push(NameNode::parse(tokens)?);
        tokens.pass_newlines()?;
    }
    let mut stmt = parse_decoratable(tokens)?;
    stmt.add_decorators(decorators);
    Ok(stmt)
}

fn parse_decoratable(tokens: &mut TokenList) -> ParseResult<DefinitionNode> {
    let node = IndependentNode::parse(tokens)?;
    node.try_into()
        .map_err(|_| tokens.error("Illegal decorator"))
}
