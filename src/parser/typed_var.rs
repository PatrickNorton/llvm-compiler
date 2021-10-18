use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeLikeNode;
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

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TypedVariableNode> {
        let type_node = TypeLikeNode::parse(tokens, ignore_newlines)?;
        let var = VariableNode::parse_newline(tokens, ignore_newlines)?;
        Ok(TypedVariableNode::new(type_node, var))
    }
}

impl Lined for TypedVariableNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
