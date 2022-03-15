use std::collections::HashSet;

use crate::parser::descriptor::{DescriptorNode, DEFINITION_VALID};
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::method::MethodDefinitionNode;
use crate::parser::operator_def::OperatorDefinitionNode;
use crate::parser::operator_sp::SpecialOpNameNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::typed_arg::TypedArgumentListNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub enum GenericDefinitionNode {
    Function(GenericFunctionNode),
    Operator(GenericOperatorNode),
}

#[derive(Debug)]
pub struct GenericFunctionNode {
    line_info: LineInfo,
    name: VariableNode,
    args: TypedArgumentListNode,
    retvals: Vec<TypeNode>,
    descriptors: HashSet<DescriptorNode>,
}

#[derive(Debug)]
pub struct GenericOperatorNode {
    line_info: LineInfo,
    op_code: SpecialOpNameNode,
    args: TypedArgumentListNode,
    retvals: Vec<TypeNode>,
    descriptors: HashSet<DescriptorNode>,
}

impl GenericDefinitionNode {
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DEFINITION_VALID
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        match self {
            GenericDefinitionNode::Function(f) => f.add_descriptor(descriptors),
            GenericDefinitionNode::Operator(o) => o.add_descriptor(descriptors),
        }
    }

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        match self {
            GenericDefinitionNode::Function(f) => f.get_descriptors(),
            GenericDefinitionNode::Operator(o) => o.get_descriptors(),
        }
    }
}

impl GenericFunctionNode {
    pub fn new(
        line_info: LineInfo,
        name: VariableNode,
        args: TypedArgumentListNode,
        retvals: Vec<TypeNode>,
    ) -> Self {
        Self {
            line_info,
            name,
            args,
            retvals,
            descriptors: HashSet::new(),
        }
    }

    pub fn get_name(&self) -> &VariableNode {
        &self.name
    }

    pub fn get_args(&self) -> &TypedArgumentListNode {
        &self.args
    }

    pub fn get_retvals(&self) -> &[TypeNode] {
        &self.retvals
    }

    pub fn add_descriptor(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
    }

    pub fn into_def(self, body: StatementBodyNode) -> MethodDefinitionNode {
        MethodDefinitionNode::new(self.line_info, self.name, self.args, self.retvals, body)
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<GenericFunctionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Method)));
        let name = VariableNode::parse(tokens)?;
        let args = TypedArgumentListNode::parse(tokens)?;
        let retval = TypeNode::parse_ret_val(tokens, false)?;
        Ok(GenericFunctionNode::new(info, name, args, retval))
    }
}

impl GenericOperatorNode {
    pub fn new(
        line_info: LineInfo,
        op_code: SpecialOpNameNode,
        args: TypedArgumentListNode,
        retvals: Vec<TypeNode>,
    ) -> Self {
        Self {
            line_info,
            op_code,
            args,
            retvals,
            descriptors: HashSet::new(),
        }
    }

    pub fn get_op_code(&self) -> &SpecialOpNameNode {
        &self.op_code
    }

    pub fn get_args(&self) -> &TypedArgumentListNode {
        &self.args
    }

    pub fn get_ret_types(&self) -> &[TypeNode] {
        &self.retvals
    }

    pub fn add_descriptor(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
    }

    pub fn into_def(self, body: StatementBodyNode, is_eq_stmt: bool) -> OperatorDefinitionNode {
        OperatorDefinitionNode::new(self.op_code, self.retvals, self.args, body, is_eq_stmt)
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<GenericOperatorNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Method)));
        let name = SpecialOpNameNode::parse(tokens)?;
        let args = TypedArgumentListNode::parse_on_open_brace(tokens)?;
        let retval = if let TokenType::Arrow = tokens.token_type()? {
            TypeNode::parse_ret_val(tokens, false)?
        } else {
            Vec::new()
        };
        Ok(GenericOperatorNode::new(info, name, args, retval))
    }
}

impl Lined for GenericDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            GenericDefinitionNode::Function(f) => f.line_info(),
            GenericDefinitionNode::Operator(o) => o.line_info(),
        }
    }
}

impl Lined for GenericFunctionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for GenericOperatorNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
