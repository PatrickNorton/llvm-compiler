use std::collections::HashSet;

use crate::parser::aug_assign::AugmentedAssignmentNode;
use crate::parser::base::IndependentNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::dotted::DottedVariableNode;
use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::number::NumberNode;
use crate::parser::string_like::StringLikeNode;
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub enum AssignStatementNode {
    Augmented(AugmentedAssignmentNode),
    Declared(DeclaredAssignmentNode),
    Normal(AssignmentNode),
}

/// Any node that can be on the left side of an equals sign.
#[derive(Debug)]
pub enum AssignableNode {
    Name(NameNode),
}

/// The node representing an assignment (non-declared).
///
/// # Syntax
/// ```text
/// AssignableNode *("," AssignableNode) [","] ("="|":=") TestNode
///     *("," TestNode) [","]
/// ```
#[derive(Debug)]
pub struct AssignmentNode {
    is_colon: bool,
    name: Vec<AssignableNode>,
    value: TestListNode,
    line_info: LineInfo,
    mutability: Option<DescriptorNode>,
}

impl AssignStatementNode {
    /// Parses an assignment from the given [`TokenList`].
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssignStatementNode> {
        if let TokenType::Keyword(Keyword::Var) = tokens.token_type()? {
            DeclaredAssignmentNode::parse(tokens).map(AssignStatementNode::Declared)
        } else if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            AssignmentNode::parse(tokens).map(AssignStatementNode::Normal)
        } else {
            let var_size = tokens.size_of_variable()?;
            if let TokenType::Assign(_) | TokenType::Comma = tokens.token_type_at(var_size)? {
                AssignmentNode::parse(tokens).map(AssignStatementNode::Normal)
            } else {
                DeclaredAssignmentNode::parse(tokens).map(AssignStatementNode::Declared)
            }
        }
    }
}

impl AssignableNode {
    /// Parses an [`AssignableNode`] from the given [`TokenList`].
    ///
    /// This is assumed to be used in the left half of an assignment-like
    /// statement (e.g. assignments, augmented assignments), and the error
    /// messages assume that.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssignableNode> {
        assert!(tokens.line_contains(|x| matches!(x.token_type(), TokenType::Assign(_)))?);
        match tokens.token_type()? {
            TokenType::Name(_) => NameNode::parse(tokens).map(AssignableNode::Name),
            TokenType::Number(_) => {
                if let TokenType::Dot(_) = tokens.token_type_at(1)? {
                    let num = NumberNode::parse(tokens)?;
                    DottedVariableNode::from_expr(tokens, TestNode::Number(num), false)
                        .map(NameNode::Dotted)
                        .map(AssignableNode::Name)
                } else {
                    Err(tokens.error("Cannot assign to numeric literal"))
                }
            }
            TokenType::String(_) => {
                if let TokenType::Dot(_) = tokens.token_type_at(1)? {
                    let string = StringLikeNode::parse(tokens)?;
                    DottedVariableNode::from_expr(tokens, string.into(), false)
                        .map(NameNode::Dotted)
                        .map(AssignableNode::Name)
                } else {
                    Err(tokens.error("Cannot assign to string literal"))
                }
            }
            TokenType::OpenBrace(_) => {
                let t = TestNode::parse_open_brace(tokens, false)?;
                if let TokenType::Dot(_) = tokens.token_type()? {
                    DottedVariableNode::from_expr(tokens, t, false)
                        .map(NameNode::Dotted)
                        .map(AssignableNode::Name)
                } else {
                    match t.try_into() {
                        Result::Ok(t) => Ok(t),
                        Err(t) => Err(ParserError::Normal(ParserException::of(
                            "Cannot assign to node",
                            t,
                        ))),
                    }
                }
            }
            _ => Err(tokens.error_with_first("Un-assignable value")),
        }
    }
}

impl AssignmentNode {
    /// Creates a new [`AssignmentNode`].
    pub fn new(
        is_colon: bool,
        name: Vec<AssignableNode>,
        value: TestListNode,
        line_info: LineInfo,
        mutability: Option<DescriptorNode>,
    ) -> Self {
        Self {
            is_colon,
            name,
            value,
            line_info,
            mutability,
        }
    }

    /// If the assignment has a colon (for dynamic assignment).
    pub fn is_colon(&self) -> bool {
        self.is_colon
    }

    /// The names that are assigned to.
    pub fn get_names(&self) -> &[AssignableNode] {
        &self.name
    }

    /// The values on the right-hand side of the assignment.
    pub fn get_values(&self) -> &TestListNode {
        &self.value
    }

    /// The set of descriptors that are valid for this node.
    ///
    /// For more information, see [`DescribableNode::valid_descriptors`].
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        todo!()
    }

    /// Adds the descriptors to the node.
    ///
    /// This assumes that there is only one descriptor in the set and that
    /// descriptor is a valid mutability. For more information, see
    /// [`DescribableNode::add_descriptors`].
    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        debug_assert!(descriptors.len() == 1 && self.mutability.is_none());
        self.mutability = descriptors.into_iter().next();
    }

    /// Parses an [`AssignmentNode`] from the given list of tokens.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssignmentNode> {
        let mut names = Vec::new();
        while !matches!(tokens.token_type()?, TokenType::Assign(_)) {
            names.push(AssignableNode::parse(tokens)?);
            if !matches!(tokens.token_type()?, TokenType::Comma) {
                break;
            }
            tokens.next_token()?;
        }
        debug_assert!(tokens.token_eq_either("=", ":=")?);
        let is_colon = tokens.token_equals(":=")?;
        tokens.next_token()?;
        let value = TestListNode::parse(tokens, false)?;
        let line_info = names[0].line_info().clone();
        Ok(AssignmentNode::new(
            is_colon,
            names,
            value,
            line_info,
            Option::None,
        ))
    }
}

impl TryFrom<TestNode> for AssignableNode {
    type Error = TestNode;

    fn try_from(value: TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Name(n) => Ok(AssignableNode::Name(n)),
            x => Err(x),
        }
    }
}

impl From<AssignableNode> for TestNode {
    fn from(node: AssignableNode) -> Self {
        match node {
            AssignableNode::Name(n) => TestNode::Name(n),
        }
    }
}

impl From<AssignStatementNode> for IndependentNode {
    fn from(node: AssignStatementNode) -> Self {
        match node {
            AssignStatementNode::Augmented(a) => IndependentNode::AugAssign(a),
            AssignStatementNode::Declared(d) => IndependentNode::DeclaredAssign(d),
            AssignStatementNode::Normal(n) => IndependentNode::Assign(n),
        }
    }
}

impl Lined for AssignableNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            AssignableNode::Name(name) => name.line_info(),
        }
    }
}

impl Lined for AssignmentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parser::name::NameNode;
    use crate::parser::test_node::TestNode;
    use crate::parser::tokenizer::Tokenizer;

    use super::{AssignableNode, AssignmentNode};

    #[test]
    fn simple_assign() {
        let mut tokens = Tokenizer::parse_str("foo = 7", PathBuf::from("/"), 0).unwrap();
        let node = AssignmentNode::parse(&mut tokens).unwrap();
        assert!(matches!(
            node.get_names(),
            &[AssignableNode::Name(NameNode::Variable(_))]
        ));
        assert!(!node.is_colon());
        assert_eq!(node.get_values().len(), 1);
        assert!(matches!(node.get_values()[0], TestNode::Number(_)));
    }

    #[test]
    fn colon_assign() {
        let mut tokens = Tokenizer::parse_str("foo := 7", PathBuf::from("/"), 0).unwrap();
        let node = AssignmentNode::parse(&mut tokens).unwrap();
        assert!(matches!(
            node.get_names(),
            &[AssignableNode::Name(NameNode::Variable(_))]
        ));
        assert!(node.is_colon());
        assert_eq!(node.get_values().len(), 1);
        assert!(matches!(node.get_values()[0], TestNode::Number(_)));
    }

    #[test]
    fn multiple_assign() {
        let mut tokens = Tokenizer::parse_str("foo, bar = 7, 9", PathBuf::from("/"), 0).unwrap();
        let node = AssignmentNode::parse(&mut tokens).unwrap();
        assert!(matches!(
            node.get_names(),
            &[
                AssignableNode::Name(NameNode::Variable(_)),
                AssignableNode::Name(NameNode::Variable(_))
            ]
        ));
        assert!(!node.is_colon());
        assert_eq!(node.get_values().len(), 2);
        assert!(matches!(node.get_values()[0], TestNode::Number(_)));
        assert!(matches!(node.get_values()[1], TestNode::Number(_)));
    }

    #[test]
    fn unequal_assign() {
        // There is no parse-time requirement that the two sides of the
        // assignment have the same length; that is checked in the compilation
        // pass. This allows splats to exist.
        let mut tokens = Tokenizer::parse_str("foo = 7, 9", PathBuf::from("/"), 0).unwrap();
        let node = AssignmentNode::parse(&mut tokens).unwrap();
        assert!(matches!(
            node.get_names(),
            &[AssignableNode::Name(NameNode::Variable(_))]
        ));
        assert!(!node.is_colon());
        assert_eq!(node.get_values().len(), 2);
        assert!(matches!(node.get_values()[0], TestNode::Number(_)));
        assert!(matches!(node.get_values()[1], TestNode::Number(_)));
    }
}
