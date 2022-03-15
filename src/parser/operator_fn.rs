use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use once_cell::sync::Lazy;

use super::name::NameNode;
use super::operator::OperatorTypeNode;
use super::test_node::TestNode;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpFuncTypeNode {
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
    Is,
    NullCoerce,
    Compare,
}

#[derive(Debug)]
pub struct EscapedOperatorNode {
    line_info: LineInfo,
    operator: OpFuncTypeNode,
}

const VALUES: [OpFuncTypeNode; 28] = [
    OpFuncTypeNode::Add,
    OpFuncTypeNode::Subtract,
    OpFuncTypeNode::USubtract,
    OpFuncTypeNode::Multiply,
    OpFuncTypeNode::Divide,
    OpFuncTypeNode::FloorDiv,
    OpFuncTypeNode::Power,
    OpFuncTypeNode::Equals,
    OpFuncTypeNode::NotEquals,
    OpFuncTypeNode::GreaterThan,
    OpFuncTypeNode::LessThan,
    OpFuncTypeNode::GreaterEqual,
    OpFuncTypeNode::LessEqual,
    OpFuncTypeNode::LeftBitshift,
    OpFuncTypeNode::RightBitshift,
    OpFuncTypeNode::BitwiseAnd,
    OpFuncTypeNode::BitwiseOr,
    OpFuncTypeNode::BitwiseXor,
    OpFuncTypeNode::BitwiseNot,
    OpFuncTypeNode::Modulo,
    OpFuncTypeNode::BoolAnd,
    OpFuncTypeNode::BoolOr,
    OpFuncTypeNode::BoolNot,
    OpFuncTypeNode::BoolXor,
    OpFuncTypeNode::In,
    OpFuncTypeNode::Is,
    OpFuncTypeNode::NullCoerce,
    OpFuncTypeNode::Compare,
];

impl OpFuncTypeNode {
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        static SORTED_VALUES: Lazy<[OpFuncTypeNode; 28]> = Lazy::new(|| sort_str_len(VALUES));
        let input = input.strip_prefix('\\')?;
        for &value in &*SORTED_VALUES {
            if input.starts_with(value.sequence()) {
                return Some((TokenType::OpFunc(value), value.sequence().len() + 1));
            }
        }
        None
    }

    pub const fn sequence(&self) -> &'static str {
        match self {
            OpFuncTypeNode::Add => "+",
            OpFuncTypeNode::Subtract => "-",
            OpFuncTypeNode::USubtract => "u-",
            OpFuncTypeNode::Multiply => "*",
            OpFuncTypeNode::Divide => "/",
            OpFuncTypeNode::FloorDiv => "//",
            OpFuncTypeNode::Power => "**",
            OpFuncTypeNode::Equals => "==",
            OpFuncTypeNode::NotEquals => "!=",
            OpFuncTypeNode::GreaterThan => ">",
            OpFuncTypeNode::LessThan => "<",
            OpFuncTypeNode::GreaterEqual => ">=",
            OpFuncTypeNode::LessEqual => "<=",
            OpFuncTypeNode::LeftBitshift => "<<",
            OpFuncTypeNode::RightBitshift => ">>",
            OpFuncTypeNode::BitwiseAnd => "&",
            OpFuncTypeNode::BitwiseOr => "|",
            OpFuncTypeNode::BitwiseXor => "^",
            OpFuncTypeNode::BitwiseNot => "~",
            OpFuncTypeNode::Modulo => "%",
            OpFuncTypeNode::BoolAnd => "and",
            OpFuncTypeNode::BoolOr => "or",
            OpFuncTypeNode::BoolNot => "not",
            OpFuncTypeNode::BoolXor => "xor",
            OpFuncTypeNode::In => "in",
            OpFuncTypeNode::Is => "is",
            OpFuncTypeNode::NullCoerce => "??",
            OpFuncTypeNode::Compare => "<=>",
        }
    }
}

impl EscapedOperatorNode {
    pub const fn new(line_info: LineInfo, operator: OpFuncTypeNode) -> Self {
        Self {
            line_info,
            operator,
        }
    }

    pub fn get_operator(&self) -> OpFuncTypeNode {
        self.operator
    }

    pub fn parse(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<EscapedOperatorNode> {
        let (info, token) = tokens.next_token()?.deconstruct();
        match token {
            TokenType::OpFunc(x) => {
                if ignore_newlines {
                    tokens.pass_newlines()?;
                }
                Ok(EscapedOperatorNode::new(info, x))
            }
            _ => panic!("Expected an escaped operator"),
        }
    }
}

impl OpFuncTypeNode {
    pub fn get_operator(&self) -> OperatorTypeNode {
        match self {
            OpFuncTypeNode::Add => OperatorTypeNode::Add,
            OpFuncTypeNode::Subtract => OperatorTypeNode::Subtract,
            OpFuncTypeNode::USubtract => OperatorTypeNode::USubtract,
            OpFuncTypeNode::Multiply => OperatorTypeNode::Multiply,
            OpFuncTypeNode::Divide => OperatorTypeNode::Divide,
            OpFuncTypeNode::FloorDiv => OperatorTypeNode::FloorDiv,
            OpFuncTypeNode::Power => OperatorTypeNode::Power,
            OpFuncTypeNode::Equals => OperatorTypeNode::Equals,
            OpFuncTypeNode::NotEquals => OperatorTypeNode::NotEquals,
            OpFuncTypeNode::GreaterThan => OperatorTypeNode::GreaterThan,
            OpFuncTypeNode::LessThan => OperatorTypeNode::LessThan,
            OpFuncTypeNode::GreaterEqual => OperatorTypeNode::GreaterEqual,
            OpFuncTypeNode::LessEqual => OperatorTypeNode::LessEqual,
            OpFuncTypeNode::LeftBitshift => OperatorTypeNode::LeftBitshift,
            OpFuncTypeNode::RightBitshift => OperatorTypeNode::RightBitshift,
            OpFuncTypeNode::BitwiseAnd => OperatorTypeNode::BitwiseAnd,
            OpFuncTypeNode::BitwiseOr => OperatorTypeNode::BitwiseOr,
            OpFuncTypeNode::BitwiseXor => OperatorTypeNode::BitwiseXor,
            OpFuncTypeNode::BitwiseNot => OperatorTypeNode::BitwiseNot,
            OpFuncTypeNode::Modulo => OperatorTypeNode::Modulo,
            OpFuncTypeNode::BoolAnd => OperatorTypeNode::BoolAnd,
            OpFuncTypeNode::BoolOr => OperatorTypeNode::BoolOr,
            OpFuncTypeNode::BoolNot => OperatorTypeNode::BoolNot,
            OpFuncTypeNode::BoolXor => OperatorTypeNode::BoolXor,
            OpFuncTypeNode::In => OperatorTypeNode::In,
            OpFuncTypeNode::Is => OperatorTypeNode::Is,
            OpFuncTypeNode::NullCoerce => OperatorTypeNode::NullCoerce,
            OpFuncTypeNode::Compare => OperatorTypeNode::Compare,
        }
    }
}

impl Lined for EscapedOperatorNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

fn sort_str_len<const N: usize>(mut value: [OpFuncTypeNode; N]) -> [OpFuncTypeNode; N] {
    value.sort_unstable_by_key(|k| k.sequence().len());
    value.reverse();
    value
}

impl<'a> TryFrom<&'a TestNode> for &'a EscapedOperatorNode {
    type Error = ();

    fn try_from(value: &'a TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Name(NameNode::EscapedOp(x)) => Ok(x),
            _ => Err(()),
        }
    }
}
