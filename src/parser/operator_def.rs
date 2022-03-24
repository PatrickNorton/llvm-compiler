use std::collections::HashSet;

use crate::parser::base::IndependentNode;
use crate::parser::descriptor::{DescriptorNode, DEFINITION_VALID};
use crate::parser::error::{ParseResult, ParserInternalError};
use crate::parser::generic_stmt::GenericOperatorNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::operator_sp::SpecialOpNameNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::typed_arg::TypedArgumentListNode;

#[derive(Debug)]
pub struct OperatorDefinitionNode {
    line_info: LineInfo,
    op_code: SpecialOpNameNode,
    ret_type: Vec<TypeNode>,
    args: TypedArgumentListNode,
    body: StatementBodyNode,
    is_eq_stmt: bool,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
    decorators: Vec<NameNode>,
    generics: Vec<TypeNode>,
}

#[derive(Debug)]
pub struct SpecialOpAssignmentNode {
    line_info: LineInfo,
    name: SpecialOpNameNode,
    assignment: TestNode,
    is_colon: bool,
    descriptors: HashSet<DescriptorNode>,
}

impl OperatorDefinitionNode {
    pub fn new(
        op_code: SpecialOpNameNode,
        ret_type: Vec<TypeNode>,
        args: TypedArgumentListNode,
        body: StatementBodyNode,
        is_eq_stmt: bool,
    ) -> Self {
        Self {
            line_info: op_code.line_info().clone(),
            op_code,
            ret_type,
            args,
            body,
            is_eq_stmt,
            descriptors: HashSet::new(),
            annotations: Vec::new(),
            decorators: Vec::new(),
            generics: Vec::new(),
        }
    }

    pub fn get_op_code(&self) -> &SpecialOpNameNode {
        &self.op_code
    }

    pub fn get_args(&self) -> &TypedArgumentListNode {
        &self.args
    }

    pub fn get_ret_types(&self) -> &[TypeNode] {
        &self.ret_type
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
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

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
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

    pub fn add_generics(&mut self, generics: Vec<TypeNode>) {
        self.generics = generics;
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<OperatorDefinitionNode> {
        let op_code = match tokens.next_token()?.deconstruct() {
            (i, TokenType::OperatorSp(s)) => SpecialOpNameNode::new(i, s),
            (info, _) => {
                return Err(ParserInternalError::of("Expected special operator", info).into())
            }
        };
        let args = TypedArgumentListNode::parse_on_open_brace(tokens)?;
        let ret_val = if let TokenType::Arrow = tokens.token_type()? {
            TypeNode::parse_ret_val(tokens, false)?
        } else {
            Vec::new()
        };
        let is_eq_stmt = tokens.token_equals("=")?;
        let body = Self::parse_body(tokens, is_eq_stmt)?;
        Ok(OperatorDefinitionNode::new(
            op_code, ret_val, args, body, is_eq_stmt,
        ))
    }

    pub fn from_generic(
        tokens: &mut TokenList,
        node: GenericOperatorNode,
    ) -> ParseResult<OperatorDefinitionNode> {
        assert!(tokens.token_equals("{")?);
        let is_eq_stmt = tokens.token_equals("=")?;
        let body = Self::parse_body(tokens, is_eq_stmt)?;
        Ok(node.into_def(body, is_eq_stmt))
    }

    fn parse_body(tokens: &mut TokenList, is_eq_stmt: bool) -> ParseResult<StatementBodyNode> {
        debug_assert!(is_eq_stmt == tokens.token_equals("=")?);
        if is_eq_stmt {
            tokens.next_token()?;
            let value = TestNode::parse_newline(tokens, false)?;
            Ok(StatementBodyNode::new(
                value.line_info().clone(),
                vec![IndependentNode::Test(value)],
            ))
        } else {
            StatementBodyNode::parse(tokens)
        }
    }
}

impl SpecialOpAssignmentNode {
    pub fn new(name: SpecialOpNameNode, assignment: TestNode, is_colon: bool) -> Self {
        Self {
            line_info: name.line_info().clone(),
            name,
            assignment,
            is_colon,
            descriptors: HashSet::new(),
        }
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

    pub fn parse(tokens: &mut TokenList) -> ParseResult<SpecialOpAssignmentNode> {
        let name = match tokens.next_token()?.deconstruct() {
            (i, TokenType::OperatorSp(s)) => SpecialOpNameNode::new(i, s),
            (info, _) => {
                return Err(ParserInternalError::of("Expected special operator", info).into())
            }
        };
        let is_colon = match tokens.next_token()?.deconstruct() {
            (_, TokenType::Assign(is_colon)) => is_colon,
            (info, _) => return Err(ParserInternalError::of("Expected assignment", info).into()),
        };
        let assignment = if let TokenType::OperatorSp(tok) = *tokens.token_type()? {
            let (info, _) = tokens.next_token()?.deconstruct();
            TestNode::Name(NameNode::SpecialOp(SpecialOpNameNode::new(info, tok)))
        } else {
            TestNode::parse(tokens)?
        };
        Ok(SpecialOpAssignmentNode::new(name, assignment, is_colon))
    }
}

impl Lined for OperatorDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for SpecialOpAssignmentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
