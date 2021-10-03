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

const VALUES: [OperatorTypeNode; 34] = [
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
        static SORTED_VALUES: Lazy<[OperatorTypeNode; 34]> = Lazy::new(|| sort_str_len(VALUES));
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
}

fn sort_str_len<const N: usize>(mut value: [OperatorTypeNode; N]) -> [OperatorTypeNode; N] {
    value.sort_unstable_by_key(|k| k.sequence().len());
    value.reverse();
    value
}
