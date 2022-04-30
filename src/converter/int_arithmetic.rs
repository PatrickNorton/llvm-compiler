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
        LangConstant::Bool(b) => Some(b.bool_value().into()),
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
        OperatorTypeNode::FloorDiv => floor_div_const(values).map(Into::into),
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
        OperatorTypeNode::Modulo => exactly_two(values)
            .and_then(|(x, y)| checked_mod(x, y))
            .map(Into::into),
        OperatorTypeNode::Power => pow_const(values).map(Into::into),
        // NOTE: This might be cleaner with feature(is_sorted), (#53485)
        OperatorTypeNode::GreaterThan => Some(values.tuple_windows().all(|(x, y)| x > y).into()),
        OperatorTypeNode::LessThan => Some(values.tuple_windows().all(|(x, y)| x < y).into()),
        OperatorTypeNode::GreaterEqual => Some(values.tuple_windows().all(|(x, y)| x >= y).into()),
        OperatorTypeNode::LessEqual => Some(values.tuple_windows().all(|(x, y)| x <= y).into()),
        _ => None,
    }
}

fn floor_div_const(values: impl Iterator<Item = BigInt>) -> Option<BigInt> {
    let (x, y) = exactly_two(values)?;
    if y.is_zero() {
        None
    } else {
        Some(x / y)
    }
}

fn checked_mod(x: BigInt, y: BigInt) -> Option<BigInt> {
    if y.is_zero() {
        None
    } else {
        Some(x % y)
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

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use num::{BigInt, One, Zero};

    use crate::converter::int_arithmetic::exactly_two;
    use crate::parser::operator::OperatorTypeNode;

    use super::compute_const;

    macro_rules! bigint_vec {
        () => {vec![]};
        ($($x:expr),+ $(,)?) => {
            vec![$(BigInt::from($x)),+]
        };
        ($x:expr; $n:expr) => {
            vec![BigInt::from($x); $n]
        }
    }

    #[test]
    fn precisely_two() {
        for i in 0..100 {
            let vals = 0..i;
            if i == 2 {
                assert_eq!(exactly_two(vals), Some((0, 1)));
            } else {
                assert_eq!(exactly_two(vals), None);
            }
        }
    }

    #[test]
    fn bigint_sum() {
        let values = vec![
            (bigint_vec![], BigInt::zero()),
            (bigint_vec![BigInt::zero()], BigInt::zero()),
            (bigint_vec![usize::MAX], usize::MAX.into()),
            (bigint_vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 55.into()),
            (bigint_vec![usize::MAX, 2], BigInt::from(usize::MAX) + 2),
            (
                bigint_vec!(1000000, 200000, 30000, 4000, 500, 60, 7),
                1234567.into(),
            ),
        ];
        for (values, sum) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Add, values.clone()),
                Some(sum.clone().into()),
                "sum({}) != {}",
                values.iter().format(", "),
                sum,
            );
        }
    }

    #[test]
    fn bigint_diff() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], None),
            (bigint_vec![1, 0], Some(BigInt::one())),
            (bigint_vec![0, 1], Some((-1).into())),
            (bigint_vec![0, 1, 2], None),
        ];
        for (values, diff) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Subtract, values),
                diff.map(Into::into),
            )
        }
    }

    #[test]
    fn bigint_negate() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], Some(BigInt::zero())),
            (bigint_vec![1], Some(-BigInt::one())),
            (bigint_vec![-1], Some(BigInt::one())),
            (bigint_vec![0, 1], None),
        ];
        for (values, neg) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::USubtract, values),
                neg.map(Into::into),
            )
        }
    }

    #[test]
    fn bigint_prod() {
        let values = vec![
            (bigint_vec![], BigInt::one()),
            (bigint_vec![BigInt::zero()], BigInt::zero()),
            (bigint_vec![usize::MAX], usize::MAX.into()),
            (bigint_vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 3628800.into()),
            (bigint_vec![usize::MAX, 2], BigInt::from(usize::MAX) * 2),
            (bigint_vec!(1001, 1002, 1003, 1004), 1010035050024u64.into()),
        ];
        for (values, prod) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Multiply, values),
                Some(prod.into()),
            )
        }
    }

    #[test]
    fn bigint_quot() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![BigInt::zero()], None),
            (bigint_vec![0, 1], Some(BigInt::zero())),
            (bigint_vec![1, 2], Some(BigInt::zero())),
            (bigint_vec![3, 4], Some(BigInt::zero())),
            (bigint_vec![7, 2], Some(3.into())),
            (bigint_vec![1, 0], None),
            (bigint_vec![1, 2, 3], None),
        ];
        for (values, quot) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::FloorDiv, values),
                quot.map(Into::into),
            )
        }
    }

    #[test]
    fn bigint_left_bs() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], None),
            (bigint_vec![0, 5], Some(BigInt::zero())),
            (bigint_vec![1, 5], Some(0b100000.into())),
            (bigint_vec![1, u64::MAX], None),
            (bigint_vec![1, 2, 3], None),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::LeftBitshift, values),
                result.map(Into::into),
            )
        }
    }

    #[test]
    fn bigint_right_bs() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], None),
            (bigint_vec![0, 5], Some(BigInt::zero())),
            (bigint_vec![0b100000, 5], Some(BigInt::one())),
            (bigint_vec![1, 3], Some(BigInt::zero())),
            (bigint_vec![1, u64::MAX], None),
            (bigint_vec![1, 2, 3], None),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::RightBitshift, values),
                result.map(Into::into),
            )
        }
    }

    #[test]
    fn bigint_equal() {
        let values = vec![
            (bigint_vec![], true),
            (bigint_vec![0], true),
            (bigint_vec![0, 0], true),
            (bigint_vec![0; 100], true),
            (bigint_vec![0, 1], false),
            (bigint_vec![0, 1, 1], false),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Equals, values.clone()),
                Some(result.into())
            );
            assert_eq!(
                compute_const(OperatorTypeNode::Is, values),
                Some(result.into())
            );
        }
    }

    #[test]
    fn bigint_not_equal() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], None),
            (bigint_vec![0, 0], Some(false)),
            (bigint_vec![0; 100], None),
            (bigint_vec![0, 1], Some(true)),
            (bigint_vec![0, 1, 1], None),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::NotEquals, values.clone()),
                result.map(Into::into)
            );
            assert_eq!(
                compute_const(OperatorTypeNode::IsNot, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_and() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![5], Some(5.into())),
            (bigint_vec![3, 0], Some(BigInt::zero())),
            (bigint_vec![4, 2], Some(BigInt::zero())),
            (bigint_vec![6, 2], Some(2.into())),
            (bigint_vec![7, 5, 4], Some(4.into())),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::BitwiseAnd, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_or() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], Some(BigInt::zero())),
            (bigint_vec![5], Some(5.into())),
            (bigint_vec![3, 0], Some(3.into())),
            (bigint_vec![4, 2], Some(6.into())),
            (bigint_vec![6, 3], Some(7.into())),
            (bigint_vec![7, 5, 4], Some(7.into())),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::BitwiseOr, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_xor() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], Some(BigInt::zero())),
            (bigint_vec![5], Some(5.into())),
            (bigint_vec![3, 0], Some(3.into())),
            (bigint_vec![4, 2], Some(6.into())),
            (bigint_vec![6, 3], Some(5.into())),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::BitwiseXor, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_not() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], Some(-BigInt::one())),
            (bigint_vec![-1], Some(BigInt::zero())),
            (bigint_vec![5], Some((-6).into())),
            (bigint_vec![3, 0], None),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::BitwiseNot, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_modulo() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![0], None),
            (bigint_vec![5], None),
            (bigint_vec![3, 0], None),
            (bigint_vec![4, 2], Some(BigInt::zero())),
            (bigint_vec![3, 5], Some(3.into())),
            (bigint_vec![6, 4], Some(2.into())),
            (bigint_vec![1, 5, 7], None),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Modulo, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_pow() {
        let values = vec![
            (bigint_vec![], None),
            (bigint_vec![1], Some(BigInt::one())),
            (bigint_vec![0, 1], Some(BigInt::zero())),
            (bigint_vec![1, 3], Some(BigInt::one())),
            (bigint_vec![0, 0], Some(BigInt::one())),
            (bigint_vec![3, 4], Some(81.into())),
            (bigint_vec![2, 5, 7], Some(34359738368u64.into())),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Power, values),
                result.map(Into::into)
            );
        }
    }

    #[test]
    fn bigint_gt() {
        let values = vec![
            (bigint_vec![], true),
            (bigint_vec![7], true),
            (bigint_vec![1, 3], false),
            (bigint_vec![3, 0], true),
            (bigint_vec![5, 3, 1], true),
            (bigint_vec![2, 6, 3], false),
            (bigint_vec![-1, 0, 1], false),
            (bigint_vec![5, 2, 2], false),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::GreaterThan, values),
                Some(result.into())
            );
        }
    }

    #[test]
    fn bigint_lt() {
        let values = vec![
            (bigint_vec![], true),
            (bigint_vec![7], true),
            (bigint_vec![1, 3], true),
            (bigint_vec![3, 0], false),
            (bigint_vec![1, 3, 5], true),
            (bigint_vec![1, 5, 2], false),
            (bigint_vec![1, 0, -1], false),
            (bigint_vec![1, 1, 3], false),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::LessThan, values),
                Some(result.into())
            );
        }
    }

    #[test]
    fn bigint_ge() {
        let values = vec![
            (bigint_vec![], true),
            (bigint_vec![7], true),
            (bigint_vec![1, 3], false),
            (bigint_vec![3, 0], true),
            (bigint_vec![2, 2], true),
            (bigint_vec![5, 3, 1], true),
            (bigint_vec![2, 6, 3], false),
            (bigint_vec![-1, 0, 1], false),
            (bigint_vec![5, 2, 2], true),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::GreaterEqual, values),
                Some(result.into())
            );
        }
    }

    #[test]
    fn bigint_le() {
        let values = vec![
            (bigint_vec![], true),
            (bigint_vec![7], true),
            (bigint_vec![3, 1], false),
            (bigint_vec![0, 5], true),
            (bigint_vec![1, 1], true),
            (bigint_vec![2, 8, 9], true),
            (bigint_vec![3, 7, 1], false),
            (bigint_vec![1, 0, -1], false),
            (bigint_vec![1, 2, 2], true),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::LessEqual, values),
                Some(result.into())
            );
        }
    }
}
