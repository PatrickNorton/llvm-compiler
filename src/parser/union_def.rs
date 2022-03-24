use std::collections::HashSet;

use crate::parser::class_def::ClassBodyNode;
use crate::parser::descriptor::{DescriptorNode, DEFINITION_VALID};
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;

#[derive(Debug)]
pub struct UnionDefinitionNode {
    line_info: LineInfo,
    name: TypeNode,
    superclasses: Vec<TypeNode>,
    body: ClassBodyNode,
    descriptors: HashSet<DescriptorNode>,
    decorators: Vec<NameNode>,
    annotations: Vec<NameNode>,
}

impl UnionDefinitionNode {
    pub fn new(
        line_info: LineInfo,
        name: TypeNode,
        superclasses: Vec<TypeNode>,
        body: ClassBodyNode,
    ) -> Self {
        Self {
            line_info,
            name,
            superclasses,
            body,
            descriptors: HashSet::new(),
            decorators: Vec::new(),
            annotations: Vec::new(),
        }
    }

    pub fn get_superclasses(&self) -> &[TypeNode] {
        &self.superclasses
    }

    pub fn get_body(&self) -> &ClassBodyNode {
        &self.body
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DEFINITION_VALID
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        &self.annotations
    }

    pub fn get_decorators(&self) -> &Vec<NameNode> {
        &self.decorators
    }

    pub fn add_decorators(&mut self, decorators: Vec<NameNode>) {
        self.decorators = decorators;
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        self.annotations = annotations;
    }

    pub fn get_name(&self) -> &TypeNode {
        &self.name
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<UnionDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Union)));
        if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            return Err(tokens.error("union keyword must be followed by union name"));
        }
        let name = TypeNode::parse(tokens)?;
        let superclasses = TypeNode::parse_list_on_keyword(tokens, Keyword::From)?;
        Ok(UnionDefinitionNode::new(
            info,
            name,
            superclasses,
            ClassBodyNode::parse(tokens)?,
        ))
    }
}

impl Lined for UnionDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
