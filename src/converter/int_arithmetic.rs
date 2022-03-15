use std::borrow::Cow;

use itertools::Itertools;
use num::{BigInt, One, ToPrimitive, Zero};

use crate::parser::operator::OperatorTypeNode;

use super::constant::LangConstant;

pub fn convert_const(value: &LangConstant) -> Option<Cow<'_, BigInt>> {
    match value {
        LangConstant::Bigint(b) => Some(Cow::Borrowed(b.get_value())),
        LangConstant::Int(i) => Some(Cow::Owned(BigInt::from(i.get_value()))),
        LangConstant::Bool(b) => Some(Cow::Owned(if b.bool_value() {
            BigInt::one()
        } else {
            BigInt::zero()
        })),
        _ => None,
    }
}

pub fn convert_to_int(value: &LangConstant) -> Option<i32> {
    match value {
        LangConstant::Bigint(b) => b.get_value().to_i32(),
        LangConstant::Int(i) => Some(i.get_value()),
        LangConstant::Bool(b) => Some(if b.bool_value() { 1 } else { 0 }),
        _ => None,
    }
}

pub fn compute_const(op: OperatorTypeNode, values: Vec<BigInt>) -> Option<LangConstant> {
    let mut values = values.into_iter();
    match op {
        OperatorTypeNode::Add => Some(values.sum::<BigInt>().into()),
        OperatorTypeNode::Subtract => exactly_two(values).map(|(x, y)| (x - y).into()),
        OperatorTypeNode::USubtract => values.exactly_one().ok().map(|x| (-x).into()),
        OperatorTypeNode::Multiply => Some(values.product::<BigInt>().into()),
        OperatorTypeNode::FloorDiv => exactly_two(values).map(|(x, y)| (x / y).into()),
        OperatorTypeNode::LeftBitshift => {
            exactly_two(values).and_then(|(x, y)| y.to_u32().map(|y| (x << y).into()))
        }
        OperatorTypeNode::RightBitshift => {
            exactly_two(values).and_then(|(x, y)| y.to_u32().map(|y| (x >> y).into()))
        }
        OperatorTypeNode::Equals | OperatorTypeNode::Is => Some(values.all_equal().into()),
        OperatorTypeNode::NotEquals | OperatorTypeNode::IsNot => {
            exactly_two(values).map(|(x, y)| (x != y).into())
        }
        OperatorTypeNode::BitwiseAnd => values.reduce(|x, y| x & y).map(Into::into),
        OperatorTypeNode::BitwiseOr => values.reduce(|x, y| x | y).map(Into::into),
        OperatorTypeNode::BitwiseXor => values.reduce(|x, y| x ^ y).map(Into::into),
        OperatorTypeNode::BitwiseNot => values.exactly_one().ok().map(|x| (!x).into()),
        OperatorTypeNode::Modulo => exactly_two(values).map(|(x, y)| (x % y).into()),
        OperatorTypeNode::Power => pow_const(values).map(Into::into),
        // NOTE: This might be cleaner with feature(is_sorted), (#53485)
        OperatorTypeNode::GreaterThan => Some(values.tuple_windows().all(|(x, y)| x > y).into()),
        OperatorTypeNode::LessThan => Some(values.tuple_windows().all(|(x, y)| x < y).into()),
        OperatorTypeNode::GreaterEqual => Some(values.tuple_windows().all(|(x, y)| x >= y).into()),
        OperatorTypeNode::LessEqual => Some(values.tuple_windows().all(|(x, y)| x <= y).into()),
        _ => None,
    }
}

fn pow_const(mut values: impl Iterator<Item = BigInt>) -> Option<BigInt> {
    // TODO: Should be try_reduce (#87053) , feature(iterator_try_reduce)
    // values.try_reduce(|x, y| y.to_u32().map(|y| x.pow(y)))
    let next = values.next()?;
    values.try_fold(next, |x, y| y.to_u32().map(|y| x.pow(y)))
}

#[inline]
fn exactly_two<T>(mut values: impl Iterator<Item = T>) -> Option<(T, T)> {
    values
        .next()
        .and_then(|x| values.exactly_one().ok().map(|y| (x, y)))
}
