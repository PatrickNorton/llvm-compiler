use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use once_cell::sync::Lazy;

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
