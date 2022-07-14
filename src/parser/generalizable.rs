use crate::parser::base::IndependentNode;
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;

/// A node which supports the addition of generics.
pub trait Generalizable {
    fn get_generics(&self) -> &[TypeNode];

    fn add_generics(&mut self, generics: Vec<TypeNode>);
}

pub(super) fn parse_generalizable(tokens: &mut TokenList) -> ParseResult<IndependentNode> {
    let (_, tok) = tokens.next_token()?.deconstruct();
    assert!(matches!(tok, TokenType::Keyword(Keyword::Generic)));
    let types = TypeNode::parse_list(tokens, false)?;
    tokens.pass_newlines()?;
    let mut node = IndependentNode::parse(tokens)?;
    match try_generalizable(&mut node) {
        Result::Ok(gen) => {
            gen.add_generics(types);
            Ok(node)
        }
        Result::Err(generalized) => Err(ParserException::of(
            "Attempted to generalize non-generalizable node",
            generalized,
        )
        .into()),
    }
}

fn try_generalizable(
    x: &mut IndependentNode,
) -> Result<&mut dyn Generalizable, &mut IndependentNode> {
    x.try_into()
}

impl<'a> TryFrom<&'a mut IndependentNode> for &'a mut dyn Generalizable {
    type Error = &'a mut IndependentNode;

    fn try_from(value: &'a mut IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::FunctionDef(f) => Ok(f),
            IndependentNode::Method(m) => Ok(m),
            IndependentNode::OpDef(o) => Ok(o),
            val => Err(val),
        }
    }
}
