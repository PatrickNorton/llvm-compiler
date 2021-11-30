use crate::parser::descriptor::{DescriptorNode, DECLARATION_VALID};
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::expect_matches;
use crate::parser::name::NameNode;
use crate::parser::test_list::TestListNode;
use crate::parser::token::TokenType;
use crate::parser::typed_var::TypedVariableNode;
use std::collections::HashSet;

use crate::parser::error::ParseResult;
use crate::parser::token_list::TokenList;

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

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DeclaredAssignmentNode> {
        let info = tokens.line_info()?.clone();
        let assigned = TypedVariableNode::parse_list(tokens)?;
        let is_colon = tokens.token_equals(":=")?;
        expect_matches!(tokens, TokenType::Assign(_), "assignment operator")?;
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
