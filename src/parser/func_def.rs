use std::collections::HashSet;

use crate::parser::descriptor::{DescriptorNode, FUNCTION_VALID};
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::typed_arg::TypedArgumentListNode;
use crate::parser::variable::VariableNode;

use super::generalizable::Generalizable;

#[derive(Debug)]
pub struct FunctionDefinitionNode {
    line_info: LineInfo,
    name: VariableNode,
    args: TypedArgumentListNode,
    ret_val: Vec<TypeNode>,
    body: StatementBodyNode,
    descriptors: HashSet<DescriptorNode>,
    decorators: Vec<NameNode>,
    annotations: Vec<NameNode>,
    generics: Vec<TypeNode>,
}

impl FunctionDefinitionNode {
    pub fn new(
        line_info: LineInfo,
        name: VariableNode,
        args: TypedArgumentListNode,
        ret_val: Vec<TypeNode>,
        body: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            name,
            args,
            ret_val,
            body,
            descriptors: HashSet::new(),
            decorators: Vec::new(),
            annotations: Vec::new(),
            generics: Vec::new(),
        }
    }

    pub fn get_name(&self) -> &VariableNode {
        &self.name
    }

    pub fn get_args(&self) -> &TypedArgumentListNode {
        &self.args
    }

    pub fn get_ret_val(&self) -> &[TypeNode] {
        &self.ret_val
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &FUNCTION_VALID
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

    pub fn parse(tokens: &mut TokenList) -> ParseResult<FunctionDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Func)));
        let name = VariableNode::parse(tokens)?;
        let args = TypedArgumentListNode::parse(tokens)?;
        let ret_val = TypeNode::parse_ret_val(tokens, false)?;
        let body = StatementBodyNode::parse(tokens)?;
        Ok(FunctionDefinitionNode::new(info, name, args, ret_val, body))
    }
}

impl Generalizable for FunctionDefinitionNode {
    fn get_generics(&self) -> &[TypeNode] {
        &self.generics
    }

    fn add_generics(&mut self, generics: Vec<TypeNode>) {
        self.generics = generics;
    }
}

impl Lined for FunctionDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
