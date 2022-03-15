use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

use super::operator::OperatorTypeNode;

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

#[derive(Debug)]
pub struct AugmentedAssignmentNode {
    line_info: LineInfo,
    operator: AugAssignTypeNode,
    name: NameNode,
    value: Box<TestNode>,
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
    pub fn new(operator: AugAssignTypeNode, name: NameNode, value: Box<TestNode>) -> Self {
        Self {
            line_info: name.line_info().clone(),
            operator,
            name,
            value,
        }
    }

    pub fn get_operator(&self) -> AugAssignTypeNode {
        self.operator
    }

    pub fn get_name(&self) -> &NameNode {
        &self.name
    }

    pub fn get_value(&self) -> &TestNode {
        &self.value
    }

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
