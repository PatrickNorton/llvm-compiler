use crate::parser::line_info::LineInfo;
use crate::parser::token::TokenType;
use once_cell::sync::Lazy;

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
