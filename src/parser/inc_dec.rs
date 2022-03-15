use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum IncDecType {
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct IncDecNode {
    line_info: LineInfo,
    sign: IncDecType,
    variable: NameNode,
}

impl IncDecNode {
    pub fn new(sign: IncDecType, variable: NameNode) -> Self {
        Self {
            line_info: variable.line_info().clone(),
            sign,
            variable,
        }
    }

    pub fn get_variable(&self) -> &NameNode {
        &self.variable
    }

    pub fn get_sign(&self) -> IncDecType {
        self.sign
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<IncDecNode> {
        let variable = NameNode::parse(tokens)?;
        let increment = match *tokens.next_token()?.token_type() {
            TokenType::Increment(x) => x,
            _ => return Err(tokens.internal_error("Expected increment or decrement")),
        };
        Ok(IncDecNode::new(increment, variable))
    }
}

impl Lined for IncDecNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
