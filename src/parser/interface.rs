use std::collections::HashSet;

use crate::parser::base::IndependentNode;
use crate::parser::class_def::ClassStatementNode;
use crate::parser::descriptor::{DescriptorNode, INTERFACE_VALID};
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::generic_stmt::{
    GenericDefinitionNode, GenericFunctionNode, GenericOperatorNode,
};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::method::MethodDefinitionNode;
use crate::parser::name::NameNode;
use crate::parser::operator_def::{OperatorDefinitionNode, SpecialOpAssignmentNode};
use crate::parser::static_block::StaticBlockNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;

#[derive(Debug)]
pub struct InterfaceDefinitionNode {
    line_info: LineInfo,
    name: TypeNode,
    superclasses: Vec<TypeNode>,
    body: InterfaceBodyNode,
    descriptors: HashSet<DescriptorNode>,
    decorators: Vec<NameNode>,
    annotations: Vec<NameNode>,
}

#[derive(Debug)]
pub struct InterfaceBodyNode {
    line_info: LineInfo,
    values: Vec<InterfaceStatementNode>,
}

#[derive(Debug)]
pub enum InterfaceStatementNode {
    ClassStmt(ClassStatementNode),
    Generic(GenericDefinitionNode),
}

impl InterfaceDefinitionNode {
    pub fn new(
        line_info: LineInfo,
        name: TypeNode,
        superclasses: Vec<TypeNode>,
        body: InterfaceBodyNode,
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

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &INTERFACE_VALID
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

    pub fn get_name(&self) -> &TypeNode {
        &self.name
    }

    pub fn get_superclasses(&self) -> &[TypeNode] {
        &self.superclasses
    }

    pub fn get_body(&self) -> &InterfaceBodyNode {
        &self.body
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<InterfaceDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Class)));
        if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            return Err(tokens.error("class keyword must be followed by class name"));
        }
        let name = TypeNode::parse(tokens)?;
        let superclasses = TypeNode::parse_list_on_keyword(tokens, Keyword::From)?;
        Ok(InterfaceDefinitionNode::new(
            info,
            name,
            superclasses,
            InterfaceBodyNode::parse(tokens)?,
        ))
    }
}

impl InterfaceBodyNode {
    pub fn new(line_info: LineInfo, values: Vec<InterfaceStatementNode>) -> Self {
        Self { line_info, values }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<InterfaceBodyNode> {
        if !tokens.token_equals("{")? {
            return Err(tokens.error("The body of a class must be enclosed in curly brackets"));
        }
        let (info, _) = tokens.next_tok(true)?.deconstruct();
        let mut statements = Vec::new();
        while !tokens.token_equals("}")? {
            statements.push(InterfaceStatementNode::parse(tokens)?);
            if !tokens.token_equals("}")? {
                tokens.expect_newline()?;
            }
        }
        tokens.next_token()?;
        Ok(InterfaceBodyNode::new(info, statements))
    }
}

impl InterfaceStatementNode {
    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        match self {
            InterfaceStatementNode::ClassStmt(c) => c.add_descriptors(descriptors),
            InterfaceStatementNode::Generic(g) => g.add_descriptors(descriptors),
        }
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        match self {
            InterfaceStatementNode::ClassStmt(c) => c.valid_descriptors(),
            InterfaceStatementNode::Generic(g) => g.valid_descriptors(),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<InterfaceStatementNode> {
        if tokens.token_equals("static")? && tokens.token_eq_at(1, "{")? {
            return StaticBlockNode::parse(tokens)
                .map(ClassStatementNode::StaticBlock)
                .map(InterfaceStatementNode::ClassStmt);
        }
        let descriptors = DescriptorNode::parse_list(tokens)?;
        let mut op = if let TokenType::OperatorSp(_) = tokens.token_type()? {
            if let TokenType::Assign(_) = tokens.token_type_at(1)? {
                InterfaceStatementNode::ClassStmt(ClassStatementNode::SpecialOp(
                    SpecialOpAssignmentNode::parse(tokens)?,
                ))
            } else {
                let op = GenericOperatorNode::parse(tokens)?;
                if tokens.token_equals("{")? {
                    InterfaceStatementNode::ClassStmt(ClassStatementNode::Operator(
                        OperatorDefinitionNode::from_generic(tokens, op)?,
                    ))
                } else {
                    InterfaceStatementNode::Generic(GenericDefinitionNode::Operator(op))
                }
            }
        } else if let TokenType::Keyword(Keyword::Method) = tokens.token_type()? {
            let func = GenericFunctionNode::parse(tokens)?;
            if tokens.token_equals("{")? {
                InterfaceStatementNode::ClassStmt(ClassStatementNode::Method(
                    MethodDefinitionNode::from_generic(tokens, func)?,
                ))
            } else {
                InterfaceStatementNode::Generic(GenericDefinitionNode::Function(func))
            }
        } else {
            let stmt = IndependentNode::parse(tokens)?;
            match stmt.try_into() {
                Result::Ok(stmt) => stmt,
                Result::Err(stmt) => {
                    return Err(ParserException::of(
                        "Illegal statement in interface definition",
                        stmt,
                    )
                    .into())
                }
            }
        };
        op.add_descriptors(descriptors);
        Ok(op)
    }
}

impl Lined for InterfaceDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for InterfaceBodyNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for InterfaceStatementNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            InterfaceStatementNode::ClassStmt(c) => c.line_info(),
            InterfaceStatementNode::Generic(g) => g.line_info(),
        }
    }
}

impl TryFrom<IndependentNode> for InterfaceStatementNode {
    type Error = IndependentNode;

    fn try_from(node: IndependentNode) -> Result<Self, Self::Error> {
        // NOTE: If generic definitions are added to IndependentNode, this needs
        // to be updated
        ClassStatementNode::try_from(node).map(InterfaceStatementNode::ClassStmt)
    }
}

impl From<InterfaceStatementNode> for IndependentNode {
    fn from(value: InterfaceStatementNode) -> Self {
        match value {
            InterfaceStatementNode::ClassStmt(c) => c.into(),
            InterfaceStatementNode::Generic(_) => todo!(),
        }
    }
}

impl<'a> TryFrom<&'a IndependentNode> for &'a InterfaceDefinitionNode {
    type Error = ();

    fn try_from(value: &'a IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::Interface(i) => Ok(i),
            _ => Err(()),
        }
    }
}

impl<'a> IntoIterator for &'a InterfaceBodyNode {
    type Item = &'a InterfaceStatementNode;

    type IntoIter = <&'a [InterfaceStatementNode] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}
