use std::collections::HashSet;

use crate::parser::descriptor::{DescriptorNode, METHOD_VALID};
use crate::parser::error::ParseResult;
use crate::parser::generic_stmt::GenericFunctionNode;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::typed_arg::TypedArgumentListNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct MethodDefinitionNode {
    line_info: LineInfo,
    name: VariableNode,
    args: TypedArgumentListNode,
    ret_val: Vec<TypeNode>,
    body: StatementBodyNode,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
    decorators: Vec<NameNode>,
    generics: Vec<TypeNode>,
}

impl MethodDefinitionNode {
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
            annotations: Vec::new(),
            decorators: Vec::new(),
            generics: Vec::new(),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<MethodDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Method)));
        let name = VariableNode::parse(tokens)?;
        let args = TypedArgumentListNode::parse(tokens)?;
        let ret_val = TypeNode::parse_ret_val(tokens, false)?;
        let body = StatementBodyNode::parse(tokens)?;
        Ok(MethodDefinitionNode::new(info, name, args, ret_val, body))
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &METHOD_VALID
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

    pub fn add_generics(&mut self, generics: Vec<TypeNode>) {
        self.generics = generics;
    }

    pub fn from_generic(
        tokens: &mut TokenList,
        generic: GenericFunctionNode,
    ) -> ParseResult<MethodDefinitionNode> {
        assert!(tokens.token_equals("{")?);
        let body = StatementBodyNode::parse(tokens)?;
        Ok(generic.into_def(body))
    }
}

impl Lined for MethodDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
