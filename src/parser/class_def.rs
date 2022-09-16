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

/// The node representing a class definition.
///
/// # Syntax
/// ```text
/// *DescriptorNode "class" TypeNode ["from" TypeNode] ClassBodyNode
/// ```
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

/// The body of a class.
///
/// This class is the equivalent of [`StatementBodyNode`], but only allowing
/// [`ClassStatementNodes`](ClassStatementNode).
///
/// # Syntax
/// ```text
/// "{" *ClassStatementNode "}"
/// ```
#[derive(Debug)]
pub struct ClassBodyNode {
    line_info: LineInfo,
    statements: Vec<ClassStatementNode>,
}

/// A statement that is valid in a class body.
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
    /// Creates a new [`ClassDefinitionNode`].
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

    /// The set of descriptors that may be prepended to this node.
    ///
    /// For more information, see [`BaseClassNode::valid_descriptors`].
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &DEFINITION_VALID
    }

    /// Adds the descriptors to the node.
    ///
    /// For more information, see [`DescribableNode::add_descriptors`].
    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    /// The set of descriptors this node has.
    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        &self.descriptors
    }

    /// The set of annotations attached to this node.
    pub fn get_annotations(&self) -> &Vec<NameNode> {
        &self.annotations
    }

    /// The types listed as superclasses of this class.
    pub fn get_superclasses(&self) -> &[TypeNode] {
        &self.superclasses
    }

    /// Adds the annotationns to the node.
    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        self.annotations = annotations;
    }

    /// The decorators associated with this node.
    pub fn get_decorators(&self) -> &Vec<NameNode> {
        &self.decorators
    }

    /// Adds the decorators to the node.
    pub fn add_decorators(&mut self, decorators: Vec<NameNode>) {
        self.decorators = decorators;
    }

    /// The name of the type, along with any generics it might have.
    pub fn get_name(&self) -> &TypeNode {
        &self.name
    }

    /// The body of the class definition.
    pub fn get_body(&self) -> &ClassBodyNode {
        &self.body
    }

    /// Parses a class definition from the list of tokens.
    ///
    /// The token list must begin with the `class` keyword, or the method will
    /// panic.
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
    /// Creates a new [`ClassBodyNode`] with the given line info and list of
    /// statements.
    pub fn new(line_info: LineInfo, statements: Vec<ClassStatementNode>) -> Self {
        Self {
            line_info,
            statements,
        }
    }

    /// Creates a new [`ClassBodyNode`] from the given list of statements.
    ///
    /// The [`LineInfo`] associated with this node is the info associated with
    /// the first statement, or [`LineInfo::empty`] if the statement list is
    /// empty.
    pub fn from_vec(statements: Vec<ClassStatementNode>) -> Self {
        Self {
            line_info: statements
                .first()
                .map_or_else(LineInfo::empty, |x| x.line_info().clone()),
            statements,
        }
    }

    /// Parses a [`ClassBodyNode`] from the list of tokens.
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

    /// Parses the [`ClassBodyNode`] of an enum.
    ///
    /// Since enum definitions begin with a list of variants (which is assumed
    /// to be already parsed), this does not check for an opening brace, just a
    /// closing brace. In all other ways, it is identical to [`Self::parse`].
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
    /// The set of valid descriptors associated with this node.
    ///
    /// This method delegates to identically-named implementations on each of
    /// its subclasses; see them for more details.
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

    /// Adds the set of valid descriptors to the given node.
    ///
    /// This method delegates to identically-named implementations on each of
    /// its subclasses; see them for more details.
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

    /// Parses a [`ClassStatementNode`] from the given list of tokens.
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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parser::tokenizer::Tokenizer;

    use super::ClassDefinitionNode;

    #[test]
    fn empty_class() {
        let mut token_list = Tokenizer::parse_str("class Foo {}", PathBuf::from("/"), 0).unwrap();
        let class_def = ClassDefinitionNode::parse(&mut token_list).unwrap();
        assert_eq!(class_def.get_name().str_name(), "Foo");
        assert!(class_def.get_superclasses().is_empty());
        assert_eq!(class_def.get_body().into_iter().count(), 0); // No is_empty method
    }

    #[test]
    fn class_supers() {
        let mut token_list =
            Tokenizer::parse_str("class Foo from Bar {}", PathBuf::from("/"), 0).unwrap();
        let class_def = ClassDefinitionNode::parse(&mut token_list).unwrap();
        assert_eq!(class_def.get_name().str_name(), "Foo");
        assert_eq!(class_def.get_superclasses().len(), 1);
        assert_eq!(class_def.get_superclasses()[0].str_name(), "Bar");
        assert_eq!(class_def.get_body().into_iter().count(), 0);
    }
}
