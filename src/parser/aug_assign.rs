use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

use super::operator::OperatorTypeNode;

/// The type of augmented assignment, e.g. `+=`, `*=`, `??=`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AugAssignTypeNode {
    Add,
    Subtract,
    Multiply,
    Divide,
    FloorDiv,
    Power,
    LeftBitshift,
    RightBitshift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    Modulo,
    NullCoerce,
    BoolAnd,
    BoolOr,
    BoolXor,
}

/// An augmented assignment statement, e.g. statements including `+=`, `-=`,
/// etc.
///
/// # Syntax
/// ```text
/// NameNode AugAssignTypeNode TestNode
/// ```
#[derive(Debug)]
pub struct AugmentedAssignmentNode {
    line_info: LineInfo,
    operator: AugAssignTypeNode,
    name: NameNode,
    value: Box<TestNode>,
}

const VALUES: &[AugAssignTypeNode] = &[
    AugAssignTypeNode::Add,
    AugAssignTypeNode::Subtract,
    AugAssignTypeNode::Multiply,
    AugAssignTypeNode::Divide,
    AugAssignTypeNode::FloorDiv,
    AugAssignTypeNode::Power,
    AugAssignTypeNode::LeftBitshift,
    AugAssignTypeNode::RightBitshift,
    AugAssignTypeNode::BitwiseAnd,
    AugAssignTypeNode::BitwiseOr,
    AugAssignTypeNode::BitwiseXor,
    AugAssignTypeNode::BitwiseNot,
    AugAssignTypeNode::Modulo,
    AugAssignTypeNode::NullCoerce,
    AugAssignTypeNode::BoolAnd,
    AugAssignTypeNode::BoolOr,
    AugAssignTypeNode::BoolXor,
];

impl AugAssignTypeNode {
    /// Attempts to parse a [augmented assignment token](TokenType::AugAssign)
    /// from the start of the given string, returning that and the number of
    /// characters parsed if found, or `None` if the string does not start with
    /// a valid token.
    ///
    /// # Examples
    /// ```
    /// assert_eq!(
    ///     AugAssignTypeNode::pattern("+="),
    ///     Some((TokenType::AugAssign(AugAssignTypeNode::Add), 2)),
    /// );
    ///
    /// assert_eq!(
    ///     AugAssignTypeNode::pattern("/= foo bar"),
    ///     Some((TokenType::AugAssign(AugAssignTypeNode::Divide), 2)),
    /// );
    ///
    /// assert_eq!(AugAssignTypeNode::pattern("foo bar"), None);
    /// ```
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        for &value in VALUES {
            let sequence = value.sequence().to_owned() + "=";
            if input.starts_with(&sequence) {
                return Some((TokenType::AugAssign(value), sequence.len()));
            }
        }
        None
    }

    /// Returns the textual representation of the token, excluding the trailing
    /// equals sign.
    ///
    /// # Examples
    /// ```
    /// assert_eq!(AugAssignTypeNode::Add.sequence(), "+");
    /// assert_eq!(AugAssignTypeNode::Multiply.sequence(), "*");
    /// assert_eq!(AugAssignTypeNode::Power.sequence(), "**");
    /// assert_eq!(AugAssignTypeNode::NullCoerce.sequence(), "??");
    /// assert_eq!(AugAssignTypeNode::BoolAnd.sequence(), "and");
    /// ```
    pub const fn sequence(&self) -> &'static str {
        match self {
            AugAssignTypeNode::Add => "+",
            AugAssignTypeNode::Subtract => "-",
            AugAssignTypeNode::Multiply => "*",
            AugAssignTypeNode::Divide => "/",
            AugAssignTypeNode::FloorDiv => "//",
            AugAssignTypeNode::Power => "**",
            AugAssignTypeNode::LeftBitshift => "<<",
            AugAssignTypeNode::RightBitshift => ">>",
            AugAssignTypeNode::BitwiseAnd => "&",
            AugAssignTypeNode::BitwiseOr => "|",
            AugAssignTypeNode::BitwiseXor => "^",
            AugAssignTypeNode::BitwiseNot => "~",
            AugAssignTypeNode::Modulo => "%",
            AugAssignTypeNode::NullCoerce => "??",
            AugAssignTypeNode::BoolAnd => "and",
            AugAssignTypeNode::BoolOr => "or",
            AugAssignTypeNode::BoolXor => "xor",
        }
    }

    /// Converts the [`AugAssignTypeNode`] into its corresponding
    /// [`OperatorTypeNode`].
    ///
    /// # Examples
    /// ```
    /// assert_eq!(AugAssignTypeNode::Add.get_operator(), OperatorTypeNode::Add);
    /// assert_eq!(
    ///     AugAssignTypeNode::Multiply.get_operator(),
    ///     OperatorTypeNode::Multiply
    /// );
    /// assert_eq!(
    ///     AugAssignTypeNode::Power.get_operator(),
    ///     OperatorTypeNode::Power
    /// );
    /// assert_eq!(
    ///     AugAssignTypeNode::NullCoerce.get_operator(),
    ///     OperatorTypeNode::NullCoerce
    /// );
    /// assert_eq!(
    ///     AugAssignTypeNode::BoolAnd.get_operator(),
    ///     OperatorTypeNode::BoolAnd
    /// );
    /// ```
    pub const fn get_operator(&self) -> OperatorTypeNode {
        match self {
            AugAssignTypeNode::Add => OperatorTypeNode::Add,
            AugAssignTypeNode::Subtract => OperatorTypeNode::Subtract,
            AugAssignTypeNode::Multiply => OperatorTypeNode::Multiply,
            AugAssignTypeNode::Divide => OperatorTypeNode::Divide,
            AugAssignTypeNode::FloorDiv => OperatorTypeNode::FloorDiv,
            AugAssignTypeNode::Power => OperatorTypeNode::Power,
            AugAssignTypeNode::LeftBitshift => OperatorTypeNode::LeftBitshift,
            AugAssignTypeNode::RightBitshift => OperatorTypeNode::RightBitshift,
            AugAssignTypeNode::BitwiseAnd => OperatorTypeNode::BitwiseAnd,
            AugAssignTypeNode::BitwiseOr => OperatorTypeNode::BitwiseOr,
            AugAssignTypeNode::BitwiseXor => OperatorTypeNode::BitwiseXor,
            AugAssignTypeNode::BitwiseNot => OperatorTypeNode::BitwiseNot,
            AugAssignTypeNode::Modulo => OperatorTypeNode::Modulo,
            AugAssignTypeNode::NullCoerce => OperatorTypeNode::NullCoerce,
            AugAssignTypeNode::BoolAnd => OperatorTypeNode::BoolAnd,
            AugAssignTypeNode::BoolOr => OperatorTypeNode::BoolOr,
            AugAssignTypeNode::BoolXor => OperatorTypeNode::BoolXor,
        }
    }
}

impl AugmentedAssignmentNode {
    /// Creates a new [`AugmentedAssignmentNode`].
    pub fn new(operator: AugAssignTypeNode, name: NameNode, value: Box<TestNode>) -> Self {
        Self {
            line_info: name.line_info().clone(),
            operator,
            name,
            value,
        }
    }

    /// The [type](AugAssignTypeNode) of augmented assignment.
    pub fn get_operator(&self) -> AugAssignTypeNode {
        self.operator
    }

    /// The name that is assigned to in the statement.
    pub fn get_name(&self) -> &NameNode {
        &self.name
    }

    /// The value that is being assigned.
    pub fn get_value(&self) -> &TestNode {
        &self.value
    }

    /// Parses an augumented assignment statement from the front of the given
    /// list.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AugmentedAssignmentNode> {
        let var = NameNode::parse(tokens)?;
        match *(tokens.token_type()?) {
            TokenType::AugAssign(op) => {
                tokens.next_token()?;
                let assignment = TestNode::parse(tokens)?;
                Ok(AugmentedAssignmentNode::new(op, var, Box::new(assignment)))
            }
            _ => Err(tokens.error_expected("augmented assignment")),
        }
    }
}

impl Lined for AugmentedAssignmentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
