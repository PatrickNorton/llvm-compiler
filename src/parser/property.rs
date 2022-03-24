use std::collections::HashSet;

use crate::parser::descriptor::{DescriptorNode, DECLARATION_VALID};
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::typed_arg::TypedArgumentListNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct PropertyDefinitionNode {
    line_info: LineInfo,
    name: VariableNode,
    type_node: TypeLikeNode,
    getter: StatementBodyNode,
    set_args: TypedArgumentListNode,
    setter: StatementBodyNode,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
    decorators: Vec<NameNode>,
}

impl PropertyDefinitionNode {
    pub fn new(
        line_info: LineInfo,
        name: VariableNode,
        type_node: TypeLikeNode,
        getter: StatementBodyNode,
        set_args: TypedArgumentListNode,
        setter: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            name,
            type_node,
            getter,
            set_args,
            setter,
            descriptors: HashSet::new(),
            annotations: Vec::new(),
            decorators: Vec::new(),
        }
    }

    pub fn get_name(&self) -> &VariableNode {
        &self.name
    }

    pub fn get_type(&self) -> &TypeLikeNode {
        &self.type_node
    }

    pub fn get_get(&self) -> &StatementBodyNode {
        &self.getter
    }

    pub fn get_set_args(&self) -> &TypedArgumentListNode {
        &self.set_args
    }

    pub fn get_set(&self) -> &StatementBodyNode {
        &self.setter
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DECLARATION_VALID
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

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        self.annotations = annotations;
    }

    pub fn get_decorators(&self) -> &Vec<NameNode> {
        &self.decorators
    }

    pub fn add_decorators(&mut self, decorators: Vec<NameNode>) {
        self.decorators = decorators;
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<PropertyDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Do)));
        let type_node = TypeLikeNode::parse(tokens, false)?;
        let name = VariableNode::parse(tokens)?;
        tokens.expect("{", true)?;
        let getter = StatementBodyNode::parse_on_token(tokens, "get")?;
        tokens.pass_newlines()?;
        let (setter, set_args) = if tokens.token_equals("set")? {
            tokens.next_token()?;
            if !tokens.token_equals("(")? {
                return Err(tokens
                    .error("'set' in a property definition must be followed by an argument list"));
            }
            (
                TypedArgumentListNode::parse(tokens)?,
                StatementBodyNode::parse(tokens)?,
            )
        } else {
            (TypedArgumentListNode::empty(), StatementBodyNode::empty())
        };
        tokens.pass_newlines()?;
        if !tokens.token_equals("}")? {
            return Err(
                tokens.error_with_first("Only set and get are allowed in context statements, not")
            );
        }
        tokens.next_token()?;
        Ok(PropertyDefinitionNode::new(
            info, name, type_node, getter, setter, set_args,
        ))
    }
}

impl Lined for PropertyDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
