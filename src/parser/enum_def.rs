use std::collections::HashSet;

use crate::parser::class_def::ClassBodyNode;
use crate::parser::descriptor::{DescriptorNode, DEFINITION_VALID};
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::fn_call::FunctionCallNode;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::name::NameNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct EnumDefinitionNode {
    line_info: LineInfo,
    name: TypeNode,
    superclasses: Vec<TypeNode>,
    names: Vec<EnumKeywordNode>,
    body: ClassBodyNode,
    descriptors: HashSet<DescriptorNode>,
    decorators: Vec<NameNode>,
    annotations: Vec<NameNode>,
}

#[derive(Debug)]
pub enum EnumKeywordNode {
    Function(FunctionCallNode),
    Variable(VariableNode),
}

impl EnumDefinitionNode {
    pub fn new(
        line_info: LineInfo,
        name: TypeNode,
        superclasses: Vec<TypeNode>,
        names: Vec<EnumKeywordNode>,
        body: ClassBodyNode,
    ) -> Self {
        Self {
            line_info,
            name,
            superclasses,
            names,
            body,
            descriptors: HashSet::new(),
            decorators: Vec::new(),
            annotations: Vec::new(),
        }
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DEFINITION_VALID
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

    pub fn parse(tokens: &mut TokenList) -> ParseResult<EnumDefinitionNode> {
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Enum)));
        let name = TypeNode::parse(tokens)?;
        let superclasses = TypeNode::parse_list_on_keyword(tokens, Keyword::From)?;
        tokens.expect("{", true)?;
        let mut names = Vec::new();
        loop {
            names.push(EnumKeywordNode::parse(tokens)?);
            if parse_if_matches!(tokens, TokenType::Comma)?.is_none() {
                break;
            }
        }
        tokens.pass_newlines()?;
        let body = ClassBodyNode::parse_enum(tokens)?;
        Ok(EnumDefinitionNode::new(
            line_info,
            name,
            superclasses,
            names,
            body,
        ))
    }
}

impl EnumKeywordNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<EnumKeywordNode> {
        if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            return Err(tokens.error("Enum keyword must start with a variable name"));
        }
        NameNode::parse(tokens)?
            .try_into()
            .map_err(|t| ParserException::of("Unexpected keyword", t).into())
    }
}

impl TryFrom<NameNode> for EnumKeywordNode {
    type Error = NameNode;

    fn try_from(value: NameNode) -> Result<Self, Self::Error> {
        match value {
            NameNode::Variable(v) => Ok(EnumKeywordNode::Variable(v)),
            NameNode::Function(f) => Ok(EnumKeywordNode::Function(f)),
            x => Err(x),
        }
    }
}

impl Lined for EnumDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for EnumKeywordNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            EnumKeywordNode::Function(f) => f.line_info(),
            EnumKeywordNode::Variable(v) => v.line_info(),
        }
    }
}
