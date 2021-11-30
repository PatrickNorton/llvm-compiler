use crate::parser::base::IndependentNode;
use crate::parser::declaration::DeclarationNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::definition::DefinitionNode;
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::line_info::Lined;
use crate::parser::macros::parse_if_matches;
use crate::parser::name::NameNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub enum AnnotatableNode {
    Definition(DefinitionNode),
    Declaration(DeclarationNode),
    DeclaredAssignment(DeclaredAssignmentNode),
}

impl AnnotatableNode {
    pub fn parse_left_annotation(tokens: &mut TokenList) -> ParseResult<AnnotatableNode> {
        assert!(matches!(tokens.token_type()?, TokenType::Dollar));
        let mut annotations = Vec::new();
        while parse_if_matches!(tokens, TokenType::Dollar)?.is_some() {
            annotations.push(NameNode::parse(tokens)?);
            tokens.pass_newlines()?;
        }
        let mut statement = Self::parse(tokens)?;
        if !statement.get_annotations().is_empty() {
            Err(tokens.error("Attempted to annotate twice"))
        } else {
            statement.add_annotations(annotations);
            Ok(statement)
        }
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        match self {
            AnnotatableNode::Definition(d) => d.get_annotations(),
            AnnotatableNode::Declaration(d) => d.get_annotations(),
            AnnotatableNode::DeclaredAssignment(d) => d.get_annotations(),
        }
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        match self {
            AnnotatableNode::Definition(d) => d.add_annotations(annotations),
            AnnotatableNode::Declaration(d) => d.add_annotations(annotations),
            AnnotatableNode::DeclaredAssignment(d) => d.add_annotations(annotations),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<AnnotatableNode> {
        IndependentNode::parse(tokens)?.try_into().map_err(|stmt| {
            ParserException::of("Attempted to use un-annotatable node", stmt).into()
        })
    }
}

impl Lined for AnnotatableNode {
    fn line_info(&self) -> &super::line_info::LineInfo {
        match self {
            AnnotatableNode::Definition(d) => d.line_info(),
            AnnotatableNode::Declaration(d) => d.line_info(),
            AnnotatableNode::DeclaredAssignment(d) => d.line_info(),
        }
    }
}

impl TryFrom<IndependentNode> for AnnotatableNode {
    type Error = IndependentNode;

    fn try_from(value: IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::Declaration(d) => Ok(AnnotatableNode::Declaration(d)),
            IndependentNode::DeclaredAssign(d) => Ok(AnnotatableNode::DeclaredAssignment(d)),
            value => value.try_into().map(AnnotatableNode::Definition),
        }
    }
}

impl From<AnnotatableNode> for IndependentNode {
    fn from(value: AnnotatableNode) -> Self {
        match value {
            AnnotatableNode::Definition(d) => d.into(),
            AnnotatableNode::Declaration(d) => IndependentNode::Declaration(d),
            AnnotatableNode::DeclaredAssignment(d) => IndependentNode::DeclaredAssign(d),
        }
    }
}
