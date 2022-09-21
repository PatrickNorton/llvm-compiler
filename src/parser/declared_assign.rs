use std::collections::HashSet;

use crate::parser::descriptor::{DescriptorNode, DECLARATION_VALID};
use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::test_list::TestListNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::typed_var::TypedVariableNode;

/// A node representing a declared assignment.
///
/// # Syntax
/// ```text
/// TypeNode VariableNode *("," TypeNode VariableNode) [","] ("=" | ":=")
///     TestNode *("," TestNode) [","]
/// ```
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
    /// Creates a new [`DeclaredAssignmentNode`].
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

    /// The list of typed variables in the declared assignment.
    pub fn get_types(&self) -> &[TypedVariableNode] {
        &self.assigned
    }

    /// Whether the assignment has a colon or not.
    pub fn is_colon(&self) -> bool {
        self.is_colon
    }

    /// The set of descriptors attached to this node.
    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
    }

    /// The list of expressions being assigned.
    pub fn get_values(&self) -> &TestListNode {
        &self.value
    }

    /// The mutability attached to this node, or [`None`] if it does not exist.
    pub fn get_mutability(&self) -> Option<DescriptorNode> {
        self.descriptors.iter().find(|x| x.is_mut_node()).cloned()
    }

    /// Parses a [`DeclaredAssignmentNode`] from the given list of tokens.
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

    /// The set of descriptors that are valid to attach to this node.
    ///
    /// For more information, see [`DescribableNode::valid_descriptors`]
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DECLARATION_VALID
    }

    /// Adds the given descriptors to the current node.
    ///
    /// For more information, see [`DescribableNode::add_descriptors`].
    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    /// The list of annotations associated with the given node.
    ///
    /// For more information, see [`AnnotatableNode::get_annotations`].
    pub fn get_annotations(&self) -> &Vec<NameNode> {
        &self.annotations
    }

    /// Sets this node's annotations to the given list.
    ///
    /// For more information, see [`AnnotatableNode::add_annotations`].
    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        self.annotations = annotations;
    }
}

impl Lined for DeclaredAssignmentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
