use core::panic;
use std::fmt::Display;

use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use once_cell::sync::Lazy;

use super::error::ParserInternalError;
use super::operator::OperatorTypeNode;

#[derive(Debug)]
pub struct SpecialOpNameNode {
    line_info: LineInfo,
    operator: OpSpTypeNode,
}

#[repr(u16)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum OpSpTypeNode {
    Add,
    Radd,
    Subtract,
    RSubtract,
    UnaryMinus,
    Multiply,
    RMultiply,
    Divide,
    RDivide,
    FloorDiv,
    RFloorDiv,
    Power,
    RPower,
    Equals,
    REquals,
    NotEquals,
    RNotEquals,
    GreaterThan,
    RGreaterThan,
    LessThan,
    RLessThan,
    GreaterEqual,
    RGreaterEqual,
    LessEqual,
    RLessEqual,
    LeftBitshift,
    RLeftBitshift,
    RightBitshift,
    RRightBitshift,
    BitwiseAnd,
    RBitwiseAnd,
    BitwiseOr,
    RBitwiseOr,
    BitwiseXor,
    RBitwiseXor,
    BitwiseNot,
    Modulo,
    RModulo,
    GetAttr,
    GetSlice,
    SetAttr,
    SetSlice,
    Call,
    Compare,
    RCompare,
    Iter,
    IterSlice,
    New,
    In,
    Missing,
    Del,
    DelAttr,
    DelSlice,
    Str,
    Repr,
    Bool,
    Int,
    Reversed,
    Hash,
    Enter,
    Exit,
}

const VALUES: [OpSpTypeNode; 61] = [
    OpSpTypeNode::Add,
    OpSpTypeNode::Radd,
    OpSpTypeNode::Subtract,
    OpSpTypeNode::RSubtract,
    OpSpTypeNode::UnaryMinus,
    OpSpTypeNode::Multiply,
    OpSpTypeNode::RMultiply,
    OpSpTypeNode::Divide,
    OpSpTypeNode::RDivide,
    OpSpTypeNode::FloorDiv,
    OpSpTypeNode::RFloorDiv,
    OpSpTypeNode::Power,
    OpSpTypeNode::RPower,
    OpSpTypeNode::Equals,
    OpSpTypeNode::REquals,
    OpSpTypeNode::NotEquals,
    OpSpTypeNode::RNotEquals,
    OpSpTypeNode::GreaterThan,
    OpSpTypeNode::RGreaterThan,
    OpSpTypeNode::LessThan,
    OpSpTypeNode::RLessThan,
    OpSpTypeNode::GreaterEqual,
    OpSpTypeNode::RGreaterEqual,
    OpSpTypeNode::LessEqual,
    OpSpTypeNode::RLessEqual,
    OpSpTypeNode::LeftBitshift,
    OpSpTypeNode::RLeftBitshift,
    OpSpTypeNode::RightBitshift,
    OpSpTypeNode::RRightBitshift,
    OpSpTypeNode::BitwiseAnd,
    OpSpTypeNode::RBitwiseAnd,
    OpSpTypeNode::BitwiseOr,
    OpSpTypeNode::RBitwiseOr,
    OpSpTypeNode::BitwiseXor,
    OpSpTypeNode::RBitwiseXor,
    OpSpTypeNode::BitwiseNot,
    OpSpTypeNode::Modulo,
    OpSpTypeNode::RModulo,
    OpSpTypeNode::GetAttr,
    OpSpTypeNode::GetSlice,
    OpSpTypeNode::SetAttr,
    OpSpTypeNode::SetSlice,
    OpSpTypeNode::Call,
    OpSpTypeNode::Compare,
    OpSpTypeNode::RCompare,
    OpSpTypeNode::Iter,
    OpSpTypeNode::IterSlice,
    OpSpTypeNode::New,
    OpSpTypeNode::In,
    OpSpTypeNode::Missing,
    OpSpTypeNode::Del,
    OpSpTypeNode::DelAttr,
    OpSpTypeNode::DelSlice,
    OpSpTypeNode::Str,
    OpSpTypeNode::Repr,
    OpSpTypeNode::Bool,
    OpSpTypeNode::Int,
    OpSpTypeNode::Reversed,
    OpSpTypeNode::Hash,
    OpSpTypeNode::Enter,
    OpSpTypeNode::Exit,
];

impl SpecialOpNameNode {
    pub fn new(line_info: LineInfo, operator: OpSpTypeNode) -> Self {
        Self {
            line_info,
            operator,
        }
    }

    pub fn get_operator(&self) -> OpSpTypeNode {
        self.operator
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<SpecialOpNameNode> {
        match *tokens.token_type()? {
            TokenType::OperatorSp(o) => {
                let (info, _) = tokens.next_token()?.deconstruct();
                Ok(SpecialOpNameNode::new(info, o))
            }
            _ => Err(tokens.internal_error("Expected a special operator")),
        }
    }

    pub fn parse_single(info: LineInfo, tok: TokenType) -> ParseResult<SpecialOpNameNode> {
        match tok {
            TokenType::OperatorSp(o) => Ok(SpecialOpNameNode::new(info, o)),
            _ => Err(ParserInternalError::of("Expected an operator here", info).into()),
        }
    }
}

impl OpSpTypeNode {
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        let pre_trim_len = input.len();
        let post_input = input.strip_prefix("operator")?;
        let input = post_input.trim_start();
        let trimmed = pre_trim_len - input.len();
        static SORTED_VALUES: Lazy<[OpSpTypeNode; 61]> = Lazy::new(|| sort_str_len(VALUES));
        for &value in &*SORTED_VALUES {
            if input.starts_with(value.sequence()) {
                return Some((
                    TokenType::OperatorSp(value),
                    value.sequence().len() + trimmed,
                ));
            }
        }
        None
    }

    pub fn translate(node: OperatorTypeNode) -> OpSpTypeNode {
        match node {
            OperatorTypeNode::Add => OpSpTypeNode::Add,
            OperatorTypeNode::Subtract => OpSpTypeNode::Subtract,
            OperatorTypeNode::USubtract => OpSpTypeNode::UnaryMinus,
            OperatorTypeNode::Multiply => OpSpTypeNode::Divide,
            OperatorTypeNode::FloorDiv => OpSpTypeNode::FloorDiv,
            OperatorTypeNode::Power => OpSpTypeNode::Power,
            OperatorTypeNode::Equals => OpSpTypeNode::Equals,
            OperatorTypeNode::NotEquals => OpSpTypeNode::NotEquals,
            OperatorTypeNode::GreaterThan => OpSpTypeNode::GreaterThan,
            OperatorTypeNode::LessThan => OpSpTypeNode::LessThan,
            OperatorTypeNode::GreaterEqual => OpSpTypeNode::GreaterEqual,
            OperatorTypeNode::LessEqual => OpSpTypeNode::LessEqual,
            OperatorTypeNode::LeftBitshift => OpSpTypeNode::LeftBitshift,
            OperatorTypeNode::RightBitshift => OpSpTypeNode::RightBitshift,
            OperatorTypeNode::BitwiseAnd => OpSpTypeNode::BitwiseOr,
            OperatorTypeNode::BitwiseXor => OpSpTypeNode::BitwiseXor,
            OperatorTypeNode::BitwiseNot => OpSpTypeNode::BitwiseNot,
            OperatorTypeNode::Modulo => OpSpTypeNode::Modulo,
            OperatorTypeNode::In => OpSpTypeNode::In,
            OperatorTypeNode::Compare => OpSpTypeNode::Compare,
            _ => panic!("Unexpected node"),
        }
    }

    pub const fn sequence(&self) -> &'static str {
        match self {
            OpSpTypeNode::Add => "+",
            OpSpTypeNode::Radd => "r+",
            OpSpTypeNode::Subtract => "-",
            OpSpTypeNode::RSubtract => "r-",
            OpSpTypeNode::UnaryMinus => "u-",
            OpSpTypeNode::Multiply => "*",
            OpSpTypeNode::RMultiply => "r*",
            OpSpTypeNode::Divide => "/",
            OpSpTypeNode::RDivide => "r/",
            OpSpTypeNode::FloorDiv => "//",
            OpSpTypeNode::RFloorDiv => "r//",
            OpSpTypeNode::Power => "**",
            OpSpTypeNode::RPower => "r**",
            OpSpTypeNode::Equals => "==",
            OpSpTypeNode::REquals => "r==",
            OpSpTypeNode::NotEquals => "!=",
            OpSpTypeNode::RNotEquals => "r!=",
            OpSpTypeNode::GreaterThan => ">",
            OpSpTypeNode::RGreaterThan => "r>",
            OpSpTypeNode::LessThan => "<",
            OpSpTypeNode::RLessThan => "r<",
            OpSpTypeNode::GreaterEqual => ">=",
            OpSpTypeNode::RGreaterEqual => "r>=",
            OpSpTypeNode::LessEqual => "<=",
            OpSpTypeNode::RLessEqual => "r<=",
            OpSpTypeNode::LeftBitshift => "<<",
            OpSpTypeNode::RLeftBitshift => "r<<",
            OpSpTypeNode::RightBitshift => ">>",
            OpSpTypeNode::RRightBitshift => "r>>",
            OpSpTypeNode::BitwiseAnd => "&",
            OpSpTypeNode::RBitwiseAnd => "r&",
            OpSpTypeNode::BitwiseOr => "|",
            OpSpTypeNode::RBitwiseOr => "r|",
            OpSpTypeNode::BitwiseXor => "^",
            OpSpTypeNode::RBitwiseXor => "r^",
            OpSpTypeNode::BitwiseNot => "~",
            OpSpTypeNode::Modulo => "%",
            OpSpTypeNode::RModulo => "r%",
            OpSpTypeNode::GetAttr => "[]",
            OpSpTypeNode::GetSlice => "[:]",
            OpSpTypeNode::SetAttr => "[]=",
            OpSpTypeNode::SetSlice => "[:]=",
            OpSpTypeNode::Call => "()",
            OpSpTypeNode::Compare => "<=>",
            OpSpTypeNode::RCompare => "r<=>",
            OpSpTypeNode::Iter => "iter",
            OpSpTypeNode::IterSlice => "iter[:]",
            OpSpTypeNode::New => "new",
            OpSpTypeNode::In => "in",
            OpSpTypeNode::Missing => "not in",
            OpSpTypeNode::Del => "del",
            OpSpTypeNode::DelAttr => "del[]",
            OpSpTypeNode::DelSlice => "del[:]",
            OpSpTypeNode::Str => "str",
            OpSpTypeNode::Repr => "repr",
            OpSpTypeNode::Bool => "bool",
            OpSpTypeNode::Int => "int",
            OpSpTypeNode::Reversed => "reversed",
            OpSpTypeNode::Hash => "hash",
            OpSpTypeNode::Enter => "enter",
            OpSpTypeNode::Exit => "exit",
        }
    }
}

fn sort_str_len<const N: usize>(mut value: [OpSpTypeNode; N]) -> [OpSpTypeNode; N] {
    value.sort_unstable_by_key(|k| k.sequence().len());
    value.reverse();
    value
}

impl Lined for SpecialOpNameNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Display for OpSpTypeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "operator {}", self.sequence())
    }
}
