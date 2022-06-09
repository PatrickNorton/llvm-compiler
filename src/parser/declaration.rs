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

/// A node representing a variable declaration.
///
/// # Syntax
/// ```
/// TypeNode VariableNode
/// ```
#[derive(Debug)]
pub struct DeclarationNode {
    line_info: LineInfo,
    type_node: TypeLikeNode,
    name: VariableNode,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
}

impl DeclarationNode {
    /// Creates a new [`DeclarationNode`].
    pub fn new(type_node: TypeLikeNode, name: VariableNode) -> Self {
        Self {
            line_info: type_node.line_info().clone(),
            type_node,
            name,
            descriptors: HashSet::new(),
            annotations: Vec::new(),
        }
    }

    /// The type of the declaration.
    pub fn get_type(&self) -> &TypeLikeNode {
        &self.type_node
    }

    /// The name that is being declared.
    pub fn get_name(&self) -> &VariableNode {
        &self.name
    }

    /// Parses a declaration from the given list of tokens.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<DeclarationNode> {
        let type_node = TypeLikeNode::parse(tokens, false)?;
        let name = VariableNode::parse(tokens)?;
        Ok(DeclarationNode::new(type_node, name))
    }

    /// The set of descriptors that are valid for this node.
    ///
    /// For more information, see [`DescribableNode::valid_descriptors`].
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DECLARATION_VALID
    }

    /// Adds the descriptors to the node.
    ///
    /// This assumes that there is only one descriptor in the set and that
    /// descriptor is a valid mutability. For more information, see
    /// [`DescribableNode::add_descriptors`].
    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    /// The set of descriptors attached to this node.
    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
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
