use crate::parser::token::TokenType;

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

const VALUES: [AugAssignTypeNode; 17] = [
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
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        for value in VALUES {
            let sequence = value.sequence().to_owned() + "=";
            if input.starts_with(&sequence) {
                return Some((TokenType::AugAssign(value), sequence.len()));
            }
        }
        None
    }

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
}
