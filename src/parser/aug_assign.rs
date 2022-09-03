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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parser::aug_assign::AugAssignTypeNode;
    use crate::parser::name::NameNode;
    use crate::parser::operator::OperatorTypeNode;
    use crate::parser::test_node::TestNode;
    use crate::parser::token::TokenType;
    use crate::parser::tokenizer::Tokenizer;

    use super::AugmentedAssignmentNode;

    fn variable_name(node: &TestNode) -> &str {
        match node {
            TestNode::Name(NameNode::Variable(v)) => v.get_name(),
            x => panic!("Expected variable, got {:?}", x),
        }
    }

    fn name_name(node: &NameNode) -> &str {
        match node {
            NameNode::Variable(v) => v.get_name(),
            x => panic!("Expected variable, got {:?}", x),
        }
    }

    #[test]
    fn aug_assign_pattern() {
        assert_eq!(
            AugAssignTypeNode::pattern("+="),
            Some((TokenType::AugAssign(AugAssignTypeNode::Add), 2)),
        );
        assert_eq!(
            AugAssignTypeNode::pattern("/= foo bar"),
            Some((TokenType::AugAssign(AugAssignTypeNode::Divide), 2)),
        );
        assert_eq!(AugAssignTypeNode::pattern("foo bar"), None);
        assert_eq!(AugAssignTypeNode::pattern("!="), None);
    }

    #[test]
    fn aug_assign_sequence() {
        assert_eq!(AugAssignTypeNode::Add.sequence(), "+");
        assert_eq!(AugAssignTypeNode::Subtract.sequence(), "-");
        assert_eq!(AugAssignTypeNode::Multiply.sequence(), "*");
        assert_eq!(AugAssignTypeNode::Divide.sequence(), "/");
        assert_eq!(AugAssignTypeNode::FloorDiv.sequence(), "//");
        assert_eq!(AugAssignTypeNode::Power.sequence(), "**");
        assert_eq!(AugAssignTypeNode::LeftBitshift.sequence(), "<<");
        assert_eq!(AugAssignTypeNode::RightBitshift.sequence(), ">>");
        assert_eq!(AugAssignTypeNode::BitwiseAnd.sequence(), "&");
        assert_eq!(AugAssignTypeNode::BitwiseOr.sequence(), "|");
        assert_eq!(AugAssignTypeNode::BitwiseXor.sequence(), "^");
        assert_eq!(AugAssignTypeNode::BitwiseNot.sequence(), "~");
        assert_eq!(AugAssignTypeNode::Modulo.sequence(), "%");
        assert_eq!(AugAssignTypeNode::NullCoerce.sequence(), "??");
        assert_eq!(AugAssignTypeNode::BoolAnd.sequence(), "and");
        assert_eq!(AugAssignTypeNode::BoolOr.sequence(), "or");
        assert_eq!(AugAssignTypeNode::BoolXor.sequence(), "xor");
    }

    macro_rules! assert_aug_ops {
        ($($name:ident),* $(,)?) => {
            $(
                assert_eq!(
                    AugAssignTypeNode::$name.get_operator(),
                    OperatorTypeNode::$name
                );
            )*
        }
    }

    #[test]
    fn aug_assign_operator() {
        assert_aug_ops!(
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
        );
    }

    #[test]
    fn aug_assign_parse() {
        let mut token_list = Tokenizer::parse_str("x += y", PathBuf::from("/"), 0).unwrap();
        let node = AugmentedAssignmentNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_operator(), AugAssignTypeNode::Add);
        assert_eq!(name_name(node.get_name()), "x");
        assert_eq!(variable_name(node.get_value()), "y");
    }

    #[test]
    fn aug_assign_err() {
        let mut token_list = Tokenizer::parse_str("x y +=", PathBuf::from("/"), 0).unwrap();
        let node = AugmentedAssignmentNode::parse(&mut token_list);
        assert!(node.is_err(), "{:#?}", node);
    }
}
