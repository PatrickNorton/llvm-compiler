use std::collections::HashSet;

use crate::parser::descriptor::{DescriptorNode, DECLARATION_VALID};
use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::test_list::TestListNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::typed_var::TypedVariableNode;

#[derive(Debug)]
pub struct DeclaredAssignmentNode {
    line_info: LineInfo,
    is_colon: bool,
    assigned: Vec<TypedVariableNode>,
    value: TestListNode,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
}

impl DeclaredAssignmentNode {
    pub fn new(
        line_info: LineInfo,
        is_colon: bool,
        assigned: Vec<TypedVariableNode>,
        value: TestListNode,
    ) -> Self {
        Self {
            line_info,
            is_colon,
            assigned,
            value,
            descriptors: HashSet::new(),
            annotations: Vec::new(),
        }
    }

    pub fn get_types(&self) -> &[TypedVariableNode] {
        &self.assigned
    }

    pub fn is_colon(&self) -> bool {
        self.is_colon
    }

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
    }

    pub fn get_values(&self) -> &TestListNode {
        &self.value
    }

    pub fn get_mutability(&self) -> Option<DescriptorNode> {
        self.descriptors.iter().find(|x| x.is_mut_node()).cloned()
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DeclaredAssignmentNode> {
        let info = tokens.line_info()?.clone();
        let assigned = TypedVariableNode::parse_list(tokens)?;
        let is_colon = match tokens.token_type()? {
            &TokenType::Assign(x) => {
                tokens.next_token()?;
                x
            }
            _ => return Err(tokens.error_expected("assignment operator")),
        };
        let value = TestListNode::parse(tokens, false)?;
        Ok(DeclaredAssignmentNode::new(info, is_colon, assigned, value))
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

impl Lined for DeclaredAssignmentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
