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

use super::class_def::ClassStatementNode;
use super::context::ContextDefinitionNode;
use super::definition::BaseClassRef;
use super::func_def::FunctionDefinitionNode;
use super::interface::InterfaceStatementNode;
use super::line_info::LineInfo;
use super::method::MethodDefinitionNode;
use super::operator_def::OperatorDefinitionNode;
use super::property::PropertyDefinitionNode;

/// A node that can be preceded by an annotation.
///
/// An annotation is a tag preceding a node that starts with a dollar sign
/// (`$`). This is used for compiler-intrinsic things, such as `$cfg(foo)` and
/// `$test`.It might, however, be possible for user-defined annotations to
/// exist in the future.
///
/// Not to be confused with [decorations](super::decorator), which begin with an
/// at sign (`@`) and are used to wrap a function with another function.
#[derive(Debug)]
pub enum AnnotatableNode {
    Definition(DefinitionNode),
    Declaration(DeclarationNode),
    DeclaredAssignment(DeclaredAssignmentNode),
}

/// A reference to a node that can be preceded by an annotation.
///
/// This is the by-reference equivalent of [`AnnotatableNode`]; see it for more
/// information.
#[derive(Debug)]
pub enum AnnotatableRef<'a> {
    BaseClass(BaseClassRef<'a>),
    Context(&'a ContextDefinitionNode),
    Declaration(&'a DeclarationNode),
    DeclaredAssignment(&'a DeclaredAssignmentNode),
    Function(&'a FunctionDefinitionNode),
    Method(&'a MethodDefinitionNode),
    Operator(&'a OperatorDefinitionNode),
    Property(&'a PropertyDefinitionNode),
}

impl AnnotatableNode {
    /// Parses an annotation from the given [`TokenList`].
    ///
    /// The first token in the list must be a dollar sign.
    ///
    /// # Annotation syntax
    /// ```text
    /// "$" NameNode
    /// ```
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

    /// The list of annotations associated with the given node.
    pub fn get_annotations(&self) -> &Vec<NameNode> {
        match self {
            AnnotatableNode::Definition(d) => d.get_annotations(),
            AnnotatableNode::Declaration(d) => d.get_annotations(),
            AnnotatableNode::DeclaredAssignment(d) => d.get_annotations(),
        }
    }

    /// Replace the node's annotations with the given list.
    ///
    /// # Important note
    /// While this function is called `add_annotations`, it is assumed that it
    /// is not called more than once per object, and as such, it simply replaces
    /// the annotation list with the new list. This behavior is not strictly
    /// defined and may change in the future.
    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        match self {
            AnnotatableNode::Definition(d) => d.add_annotations(annotations),
            AnnotatableNode::Declaration(d) => d.add_annotations(annotations),
            AnnotatableNode::DeclaredAssignment(d) => d.add_annotations(annotations),
        }
    }

    /// Parse an annotatable node from a list of tokens.
    ///
    /// Note that this does not parse the annotations in front of the node; if
    /// you see an annotation, it is best to use [`Self::parse_left_annotation`]
    /// instead.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AnnotatableNode> {
        IndependentNode::parse(tokens)?.try_into().map_err(|stmt| {
            ParserException::of("Attempted to use un-annotatable node", stmt).into()
        })
    }

    /// The annotations associated with the given [`ClassStatementNode`], if
    /// that node supports annotations.
    ///
    /// The two [`ClassStatementNode`]s at the moment that do not allow
    /// annotations are [special-operator assignment](SpecialOpAssignNode) and
    /// [static blocks](StaticBlockNode).
    pub fn try_get_annotations(node: &ClassStatementNode) -> Option<&Vec<NameNode>> {
        match node {
            ClassStatementNode::BaseClass(b) => Some(b.get_annotations()),
            ClassStatementNode::Context(c) => Some(c.get_annotations()),
            ClassStatementNode::Declaration(d) => Some(d.get_annotations()),
            ClassStatementNode::DeclaredAssign(d) => Some(d.get_annotations()),
            ClassStatementNode::Method(m) => Some(m.get_annotations()),
            ClassStatementNode::Operator(o) => Some(o.get_annotations()),
            ClassStatementNode::Property(p) => Some(p.get_annotations()),
            ClassStatementNode::SpecialOp(_) => None,
            ClassStatementNode::StaticBlock(_) => None,
        }
    }

    /// The annotations associated with the given [`InterfaceStatementNode`], if
    /// that node supports annotations.
    ///
    /// This works identically to [`Self::try_get_annotations`] for all
    /// statements that are also a [`ClassStatementNode`]; the only node for
    /// which that is not the case is
    /// [generic definitions](GenericDefinitionNode), which do not support
    /// annotations.
    pub fn try_interface_annotations(node: &InterfaceStatementNode) -> Option<&Vec<NameNode>> {
        match node {
            InterfaceStatementNode::ClassStmt(c) => Self::try_get_annotations(c),
            InterfaceStatementNode::Generic(_) => None,
        }
    }
}

impl<'a> AnnotatableRef<'a> {
    /// The list of annotations associated with the given node.
    ///
    /// Equivalent to [`AnnotatableNode::get_annotations`].
    pub fn get_annotations(&self) -> &[NameNode] {
        match self {
            AnnotatableRef::BaseClass(b) => b.get_annotations(),
            AnnotatableRef::Context(c) => c.get_annotations(),
            AnnotatableRef::Declaration(d) => d.get_annotations(),
            AnnotatableRef::DeclaredAssignment(d) => d.get_annotations(),
            AnnotatableRef::Function(f) => f.get_annotations(),
            AnnotatableRef::Method(m) => m.get_annotations(),
            AnnotatableRef::Operator(o) => o.get_annotations(),
            AnnotatableRef::Property(p) => p.get_annotations(),
        }
    }

    pub fn is_definition(&self) -> bool {
        // TODO? Type safety
        !matches!(
            self,
            AnnotatableRef::Declaration(_) | AnnotatableRef::DeclaredAssignment(_)
        )
    }
}

impl Lined for AnnotatableNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            AnnotatableNode::Definition(d) => d.line_info(),
            AnnotatableNode::Declaration(d) => d.line_info(),
            AnnotatableNode::DeclaredAssignment(d) => d.line_info(),
        }
    }
}

impl<'a> Lined for AnnotatableRef<'a> {
    fn line_info(&self) -> &LineInfo {
        match self {
            AnnotatableRef::BaseClass(b) => b.line_info(),
            AnnotatableRef::Context(c) => c.line_info(),
            AnnotatableRef::Declaration(d) => d.line_info(),
            AnnotatableRef::DeclaredAssignment(d) => d.line_info(),
            AnnotatableRef::Function(f) => f.line_info(),
            AnnotatableRef::Method(m) => m.line_info(),
            AnnotatableRef::Operator(o) => o.line_info(),
            AnnotatableRef::Property(p) => p.line_info(),
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
