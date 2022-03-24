use std::collections::HashSet;

use crate::parser::base::IndependentNode;
use crate::parser::context::ContextDefinitionNode;
use crate::parser::declaration::DeclarationNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::definition::BaseClassNode;
use crate::parser::descriptor::{DescriptorNode, DEFINITION_VALID};
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::method::MethodDefinitionNode;
use crate::parser::name::NameNode;
use crate::parser::operator_def::{OperatorDefinitionNode, SpecialOpAssignmentNode};
use crate::parser::property::PropertyDefinitionNode;
use crate::parser::static_block::StaticBlockNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;

#[derive(Debug)]
pub struct ClassDefinitionNode {
    line_info: LineInfo,
    name: TypeNode,
    superclasses: Vec<TypeNode>,
    body: ClassBodyNode,
    descriptors: HashSet<DescriptorNode>,
    decorators: Vec<NameNode>,
    annotations: Vec<NameNode>,
}

#[derive(Debug)]
pub struct ClassBodyNode {
    line_info: LineInfo,
    statements: Vec<ClassStatementNode>,
}

#[derive(Debug)]
pub enum ClassStatementNode {
    BaseClass(BaseClassNode),
    Context(ContextDefinitionNode),
    Declaration(DeclarationNode),
    DeclaredAssign(DeclaredAssignmentNode),
    Method(MethodDefinitionNode),
    Operator(OperatorDefinitionNode),
    Property(PropertyDefinitionNode),
    SpecialOp(SpecialOpAssignmentNode),
    StaticBlock(StaticBlockNode),
}

impl ClassDefinitionNode {
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

    pub fn get_superclasses(&self) -> &[TypeNode] {
        &self.superclasses
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

    pub fn get_name(&self) -> &TypeNode {
        &self.name
    }

    pub fn get_body(&self) -> &ClassBodyNode {
        &self.body
    }

    pub fn str_name(&self) -> &str {
        self.name.str_name()
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ClassDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Class)));
        if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            return Err(tokens.error("class keyword must be followed by class name"));
        }
        let name = TypeNode::parse(tokens)?;
        let superclasses = TypeNode::parse_list_on_keyword(tokens, Keyword::From)?;
        Ok(ClassDefinitionNode::new(
            info,
            name,
            superclasses,
            ClassBodyNode::parse(tokens)?,
        ))
    }
}

impl ClassBodyNode {
    pub fn new(line_info: LineInfo, statements: Vec<ClassStatementNode>) -> Self {
        Self {
            line_info,
            statements,
        }
    }

    pub fn from_vec(statements: Vec<ClassStatementNode>) -> Self {
        Self {
            line_info: statements
                .first()
                .map_or_else(LineInfo::empty, |x| x.line_info().clone()),
            statements,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ClassBodyNode> {
        if !tokens.token_equals("{")? {
            return Err(tokens.error("The body of a class must be enclosed in curly brackets"));
        }
        let (info, _) = tokens.next_tok(true)?.deconstruct();
        let cb = Self::parse_until_token(info, tokens, "}")?;
        assert!(tokens.token_equals("}")?);
        tokens.next_token()?;
        Ok(cb)
    }

    pub fn parse_enum(tokens: &mut TokenList) -> ParseResult<ClassBodyNode> {
        let cb = Self::parse_until_token(tokens.line_info()?.clone(), tokens, "}")?;
        assert!(tokens.token_equals("}")?);
        tokens.next_token()?;
        Ok(cb)
    }

    fn parse_until_token(
        info: LineInfo,
        tokens: &mut TokenList,
        text: &str,
    ) -> ParseResult<ClassBodyNode> {
        let mut statements = Vec::new();
        while !tokens.token_equals(text)? {
            statements.push(ClassStatementNode::parse(tokens)?);
            if !tokens.token_equals(text)? {
                tokens.expect_newline()?;
            }
        }
        Ok(ClassBodyNode::new(info, statements))
    }
}

impl ClassStatementNode {
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        match self {
            ClassStatementNode::BaseClass(b) => b.valid_descriptors(),
            ClassStatementNode::Context(c) => c.valid_descriptors(),
            ClassStatementNode::Declaration(d) => d.valid_descriptors(),
            ClassStatementNode::DeclaredAssign(d) => d.valid_descriptors(),
            ClassStatementNode::Method(m) => m.valid_descriptors(),
            ClassStatementNode::Operator(o) => o.valid_descriptors(),
            ClassStatementNode::Property(p) => p.valid_descriptors(),
            ClassStatementNode::SpecialOp(s) => s.valid_descriptors(),
            ClassStatementNode::StaticBlock(s) => s.valid_descriptors(),
        }
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        match self {
            ClassStatementNode::BaseClass(b) => b.add_descriptors(descriptors),
            ClassStatementNode::Context(c) => c.add_descriptors(descriptors),
            ClassStatementNode::Declaration(d) => d.add_descriptors(descriptors),
            ClassStatementNode::DeclaredAssign(d) => d.add_descriptors(descriptors),
            ClassStatementNode::Method(m) => m.add_descriptors(descriptors),
            ClassStatementNode::Operator(o) => o.add_descriptors(descriptors),
            ClassStatementNode::Property(p) => p.add_descriptors(descriptors),
            ClassStatementNode::SpecialOp(s) => s.add_descriptors(descriptors),
            ClassStatementNode::StaticBlock(s) => s.add_descriptors(descriptors),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ClassStatementNode> {
        if tokens.token_equals("static")? && tokens.token_eq_at(1, "{")? {
            StaticBlockNode::parse(tokens).map(ClassStatementNode::StaticBlock)
        } else {
            IndependentNode::parse(tokens)?
                .try_into()
                .map_err(|stmt| ParserException::of("Invalid class statement", stmt).into())
        }
    }
}

impl Lined for ClassDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for ClassBodyNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for ClassStatementNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            ClassStatementNode::BaseClass(b) => b.line_info(),
            ClassStatementNode::Context(c) => c.line_info(),
            ClassStatementNode::Declaration(d) => d.line_info(),
            ClassStatementNode::DeclaredAssign(d) => d.line_info(),
            ClassStatementNode::Method(m) => m.line_info(),
            ClassStatementNode::Operator(o) => o.line_info(),
            ClassStatementNode::Property(p) => p.line_info(),
            ClassStatementNode::SpecialOp(s) => s.line_info(),
            ClassStatementNode::StaticBlock(s) => s.line_info(),
        }
    }
}

impl TryFrom<IndependentNode> for ClassStatementNode {
    type Error = IndependentNode;

    fn try_from(value: IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::ClassDef(c) => {
                Ok(ClassStatementNode::BaseClass(BaseClassNode::Class(c)))
            }
            IndependentNode::Enum(e) => Ok(ClassStatementNode::BaseClass(BaseClassNode::Enum(e))),
            IndependentNode::Interface(e) => {
                Ok(ClassStatementNode::BaseClass(BaseClassNode::Interface(e)))
            }
            IndependentNode::Union(e) => Ok(ClassStatementNode::BaseClass(BaseClassNode::Union(e))),
            IndependentNode::Context(c) => Ok(ClassStatementNode::Context(c)),
            IndependentNode::Declaration(d) => Ok(ClassStatementNode::Declaration(d)),
            IndependentNode::DeclaredAssign(d) => Ok(ClassStatementNode::DeclaredAssign(d)),
            IndependentNode::Method(m) => Ok(ClassStatementNode::Method(m)),
            IndependentNode::OpDef(o) => Ok(ClassStatementNode::Operator(o)),
            IndependentNode::Property(p) => Ok(ClassStatementNode::Property(p)),
            IndependentNode::OpAssign(s) => Ok(ClassStatementNode::SpecialOp(s)),
            val => Err(val),
        }
    }
}

impl From<ClassStatementNode> for IndependentNode {
    fn from(value: ClassStatementNode) -> Self {
        match value {
            ClassStatementNode::BaseClass(b) => b.into(),
            ClassStatementNode::Context(c) => IndependentNode::Context(c),
            ClassStatementNode::Declaration(d) => IndependentNode::Declaration(d),
            ClassStatementNode::DeclaredAssign(d) => IndependentNode::DeclaredAssign(d),
            ClassStatementNode::Method(m) => IndependentNode::Method(m),
            ClassStatementNode::Operator(o) => IndependentNode::OpDef(o),
            ClassStatementNode::Property(p) => IndependentNode::Property(p),
            ClassStatementNode::SpecialOp(s) => IndependentNode::OpAssign(s),
            ClassStatementNode::StaticBlock(_) => todo!(),
        }
    }
}

impl<'a> IntoIterator for &'a ClassBodyNode {
    type Item = &'a ClassStatementNode;

    type IntoIter = <&'a [ClassStatementNode] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.iter()
    }
}
