use crate::parser::base::IndependentNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::descriptor::DECLARATION_VALID;
use crate::parser::error::ParseResult;
use crate::parser::line_info::LineInfo;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::variable::VariableNode;
use std::collections::HashSet;

#[derive(Debug)]
pub struct DeclarationNode {
    line_info: LineInfo,
    type_node: TypeLikeNode,
    name: VariableNode,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
}

impl DeclarationNode {
    pub fn new(type_node: TypeLikeNode, name: VariableNode) -> Self {
        Self {
            line_info: type_node.line_info().clone(),
            type_node,
            name,
            descriptors: HashSet::new(),
            annotations: Vec::new(),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DeclarationNode> {
        let type_node = TypeLikeNode::parse(tokens, false)?;
        let name = VariableNode::parse(tokens)?;
        Ok(DeclarationNode::new(type_node, name))
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DECLARATION_VALID
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        &self.annotations
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        self.annotations = annotations;
    }
}

impl Lined for DeclarationNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl From<DeclarationNode> for IndependentNode {
    fn from(x: DeclarationNode) -> Self {
        Self::Declaration(x)
    }
}