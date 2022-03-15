use itertools::{process_results, Itertools};
use num::{BigInt, One, ToPrimitive};

use crate::parser::operator::OperatorTypeNode;

use super::constant::{LangConstant, NumberConstant, StringConstant};

pub fn compute_const(op: OperatorTypeNode, consts: Vec<LangConstant>) -> Option<LangConstant> {
    match op {
        OperatorTypeNode::Add => add(consts).map(Into::into),
        OperatorTypeNode::Multiply => mul(consts).map(Into::into),
        OperatorTypeNode::Is | OperatorTypeNode::Equals => eq(consts).map(Into::into),
        OperatorTypeNode::IsNot | OperatorTypeNode::NotEquals => ne(consts).map(Into::into),
        _ => None,
    }
}

fn add(consts: Vec<LangConstant>) -> Option<String> {
    let mut result = String::new();
    for constant in consts {
        result.push_str(StringConstant::try_from(constant).ok()?.get_value())
    }
    Some(result)
}

fn mul(consts: Vec<LangConstant>) -> Option<String> {
    let mut consts = consts.into_iter();
    let value = StringConstant::try_from(consts.next()?).unwrap();
    let mut product = BigInt::one();
    for constant in consts {
        product *= &*NumberConstant::try_from(constant).ok()?.big_value();
    }
    let small_product = product.to_usize()?;
    small_product.checked_mul(value.get_value().len())?;
    Some(value.get_value().repeat(small_product))
}

fn eq(consts: Vec<LangConstant>) -> Option<bool> {
    process_results(consts.into_iter().map(StringConstant::try_from), |mut x| {
        x.all_equal()
    })
    .ok()
}

fn ne(consts: Vec<LangConstant>) -> Option<bool> {
    process_results(consts.into_iter().map(StringConstant::try_from), |mut x| {
        !x.all_equal()
    })
    .ok()
}
