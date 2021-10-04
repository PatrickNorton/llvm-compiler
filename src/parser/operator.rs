use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use once_cell::sync::Lazy;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum OperatorTypeNode {
    Add,
    Subtract,
    USubtract,
    Multiply,
    Divide,
    FloorDiv,
    Power,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    LeftBitshift,
    RightBitshift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    Modulo,
    BoolAnd,
    BoolOr,
    BoolNot,
    BoolXor,
    In,
    NotIn,
    Casted,
    Is,
    IsNot,
    NullCoerce,
    NotNull,
    Optional,
    Instanceof,
    NotInstanceof,
    Compare,
}

#[derive(Debug)]
pub struct OperatorNode {
    line_info: LineInfo,
    operator: OperatorTypeNode,
    arguments: Vec<ArgumentNode>,
}

const VALUES_LEN: usize = 35;
const VALUES: [OperatorTypeNode; VALUES_LEN] = [
    OperatorTypeNode::Add,
    OperatorTypeNode::Subtract,
    OperatorTypeNode::USubtract,
    OperatorTypeNode::Multiply,
    OperatorTypeNode::Divide,
    OperatorTypeNode::FloorDiv,
    OperatorTypeNode::Power,
    OperatorTypeNode::Equals,
    OperatorTypeNode::NotEquals,
    OperatorTypeNode::GreaterThan,
    OperatorTypeNode::LessThan,
    OperatorTypeNode::GreaterEqual,
    OperatorTypeNode::LessEqual,
    OperatorTypeNode::LeftBitshift,
    OperatorTypeNode::RightBitshift,
    OperatorTypeNode::BitwiseAnd,
    OperatorTypeNode::BitwiseOr,
    OperatorTypeNode::BitwiseXor,
    OperatorTypeNode::BitwiseNot,
    OperatorTypeNode::Modulo,
    OperatorTypeNode::BoolAnd,
    OperatorTypeNode::BoolOr,
    OperatorTypeNode::BoolNot,
    OperatorTypeNode::BoolXor,
    OperatorTypeNode::In,
    OperatorTypeNode::NotIn,
    OperatorTypeNode::Casted,
    OperatorTypeNode::Is,
    OperatorTypeNode::IsNot,
    OperatorTypeNode::NullCoerce,
    OperatorTypeNode::NotNull,
    OperatorTypeNode::Optional,
    OperatorTypeNode::Instanceof,
    OperatorTypeNode::NotInstanceof,
    OperatorTypeNode::Compare,
];

impl OperatorTypeNode {
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        static SORTED_VALUES: Lazy<[OperatorTypeNode; VALUES_LEN]> =
            Lazy::new(|| sort_str_len(VALUES));
        for &value in &*SORTED_VALUES {
            if input.starts_with(value.sequence()) {
                return Some((TokenType::Operator(value), value.sequence().len()));
            }
        }
        None
    }

    pub const fn sequence(&self) -> &'static str {
        match self {
            OperatorTypeNode::Add => "+",
            OperatorTypeNode::Subtract => "-",
            OperatorTypeNode::USubtract => "-",
            OperatorTypeNode::Multiply => "*",
            OperatorTypeNode::Divide => "/",
            OperatorTypeNode::FloorDiv => "//",
            OperatorTypeNode::Power => "**",
            OperatorTypeNode::Equals => "==",
            OperatorTypeNode::NotEquals => "!=",
            OperatorTypeNode::GreaterThan => ">",
            OperatorTypeNode::LessThan => "<",
            OperatorTypeNode::GreaterEqual => ">=",
            OperatorTypeNode::LessEqual => "<=",
            OperatorTypeNode::LeftBitshift => "<<",
            OperatorTypeNode::RightBitshift => ">>",
            OperatorTypeNode::BitwiseAnd => "&",
            OperatorTypeNode::BitwiseOr => "|",
            OperatorTypeNode::BitwiseXor => "^",
            OperatorTypeNode::BitwiseNot => "~",
            OperatorTypeNode::Modulo => "%",
            OperatorTypeNode::BoolAnd => "and",
            OperatorTypeNode::BoolOr => "or",
            OperatorTypeNode::BoolNot => "not",
            OperatorTypeNode::BoolXor => "xor",
            OperatorTypeNode::In => "in",
            OperatorTypeNode::NotIn => "not in",
            OperatorTypeNode::Casted => "casted",
            OperatorTypeNode::Is => "is",
            OperatorTypeNode::IsNot => "is not",
            OperatorTypeNode::NullCoerce => "??",
            OperatorTypeNode::NotNull => "!!",
            OperatorTypeNode::Optional => "?",
            OperatorTypeNode::Instanceof => "instanceof",
            OperatorTypeNode::NotInstanceof => "not instanceof",
            OperatorTypeNode::Compare => "<=>",
        }
    }

    pub const fn precedence(&self) -> usize {
        match self {
            OperatorTypeNode::Add => 3,
            OperatorTypeNode::Subtract => 3,
            OperatorTypeNode::USubtract => 1,
            OperatorTypeNode::Multiply => 2,
            OperatorTypeNode::Divide => 2,
            OperatorTypeNode::FloorDiv => 2,
            OperatorTypeNode::Power => 0,
            OperatorTypeNode::Equals => 7,
            OperatorTypeNode::NotEquals => 7,
            OperatorTypeNode::GreaterThan => 7,
            OperatorTypeNode::LessThan => 7,
            OperatorTypeNode::GreaterEqual => 7,
            OperatorTypeNode::LessEqual => 7,
            OperatorTypeNode::LeftBitshift => 4,
            OperatorTypeNode::RightBitshift => 4,
            OperatorTypeNode::BitwiseAnd => 5,
            OperatorTypeNode::BitwiseOr => 6,
            OperatorTypeNode::BitwiseXor => 6,
            OperatorTypeNode::BitwiseNot => 1,
            OperatorTypeNode::Modulo => 2,
            OperatorTypeNode::BoolAnd => 11,
            OperatorTypeNode::BoolOr => 12,
            OperatorTypeNode::BoolNot => 10,
            OperatorTypeNode::BoolXor => 13,
            OperatorTypeNode::In => 8,
            OperatorTypeNode::NotIn => 8,
            OperatorTypeNode::Casted => 14,
            OperatorTypeNode::Is => 8,
            OperatorTypeNode::IsNot => 8,
            OperatorTypeNode::NullCoerce => 0,
            OperatorTypeNode::NotNull => 0,
            OperatorTypeNode::Optional => 0,
            OperatorTypeNode::Instanceof => 9,
            OperatorTypeNode::NotInstanceof => 9,
            OperatorTypeNode::Compare => 7,
        }
    }

    pub const fn is_unary(&self) -> bool {
        matches!(
            self,
            OperatorTypeNode::USubtract
                | OperatorTypeNode::BitwiseNot
                | OperatorTypeNode::BoolNot
                | OperatorTypeNode::NotNull
                | OperatorTypeNode::Optional
        )
    }

    pub const fn is_postfix(&self) -> bool {
        matches!(self, OperatorTypeNode::NotNull | OperatorTypeNode::Optional)
    }
}

impl OperatorNode {
    pub fn new(
        line_info: LineInfo,
        operator: OperatorTypeNode,
        arguments: Vec<ArgumentNode>,
    ) -> Self {
        Self {
            line_info,
            operator,
            arguments,
        }
    }

    pub fn from_nodes(
        line_info: LineInfo,
        operator: OperatorTypeNode,
        arguments: Vec<TestNode>,
    ) -> Self {
        Self {
            line_info,
            operator,
            arguments: ArgumentNode::from_test_nodes(arguments),
        }
    }
}

fn sort_str_len<const N: usize>(mut value: [OperatorTypeNode; N]) -> [OperatorTypeNode; N] {
    value.sort_unstable_by_key(|k| k.sequence().len());
    value.reverse();
    value
}
