use itertools::{process_results, Itertools};
use num::{BigInt, One, ToPrimitive};

use crate::parser::operator::OperatorTypeNode;

use super::constant::{LangConstant, NumberConstant, StringConstant};

/// Computes constant-folding for a list of [`LangConstant`]s.
///
/// This does not do any type-checking, but instead returns `None` in any cases
/// where the operation is not well-defined. Type checks should be done at the
/// level of the function calling this code.
///
/// In addition to returning `None` in cases where the operation is undefined,
/// it also does so in cases in which the operation would error; for example in
/// the code
///
/// ```text
/// var foo = "abc" * ((1 << 64) + 1)
/// ```
///
/// which would not fit into any computer's memory. As such, note that returning
/// `None` does not necessarily signify that an error occurred, just that the
/// operation could not be completed statically.
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

#[cfg(test)]
mod tests {
    use crate::converter::constant::LangConstant;
    use crate::parser::operator::OperatorTypeNode;

    use super::compute_const;

    macro_rules! lang_const_vec {
        () => {vec![]};

        ($($x:expr),+ $(,)?) => {
            vec![$(LangConstant::from($x)),+]
        }
    }

    #[test]
    fn string_add() {
        let values = vec![
            (lang_const_vec![], ""),
            (lang_const_vec!["a"], "a"),
            (lang_const_vec!["foo", "bar"], "foobar"),
            (
                lang_const_vec!["string", " ", "summation"],
                "string summation",
            ),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Add, values),
                Some(result.into())
            );
        }
    }

    #[test]
    fn string_add_fail() {
        let values = vec![
            lang_const_vec![1],
            lang_const_vec!["a", 1],
            lang_const_vec![true, "false"],
            lang_const_vec!["12345", 67],
        ];
        for val in values {
            assert_eq!(compute_const(OperatorTypeNode::Add, val), None);
        }
    }

    #[test]
    fn string_mul() {
        let values = vec![
            (lang_const_vec!["foo"], "foo"),
            (lang_const_vec!["test", 5], "testtesttesttesttest"),
            (lang_const_vec!["bar ", 3, 2], "bar bar bar bar bar bar "),
        ];
        for (values, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Multiply, values),
                Some(result.into())
            );
        }
    }

    #[test]
    fn string_mul_fail() {
        let values = vec![
            lang_const_vec![],
            lang_const_vec!["foo", "bar"],
            lang_const_vec!["test", true],
            lang_const_vec!["temp", usize::MAX],
        ];
        for val in values {
            assert_eq!(compute_const(OperatorTypeNode::Multiply, val), None);
        }
    }

    #[test]
    fn string_eq() {
        let values = vec![
            (lang_const_vec![], true),
            (lang_const_vec!["a"], true),
            (lang_const_vec!["foo", "bar"], false),
            (lang_const_vec!["test", "test"], true),
            (lang_const_vec!["test", "test", "foo"], false),
        ];
        for (val, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::Equals, val.clone()),
                Some(result.into())
            );
            assert_eq!(
                compute_const(OperatorTypeNode::Is, val),
                Some(result.into())
            );
        }
    }

    #[test]
    fn string_eq_fail() {
        let values = vec![
            lang_const_vec!["a", 'a'],
            lang_const_vec!["foo", true, "bar"],
        ];
        for val in values {
            assert_eq!(compute_const(OperatorTypeNode::Equals, val.clone()), None);
            assert_eq!(compute_const(OperatorTypeNode::Is, val), None);
        }
    }

    #[test]
    fn string_not_eq() {
        let values = vec![
            (lang_const_vec![], false),
            (lang_const_vec!["a"], false),
            (lang_const_vec!["foo", "bar"], true),
            (lang_const_vec!["test", "test"], false),
            (lang_const_vec!["test", "test", "foo"], true),
            (lang_const_vec!["test", "foo", "test"], true),
        ];
        for (val, result) in values {
            assert_eq!(
                compute_const(OperatorTypeNode::NotEquals, val.clone()),
                Some(result.into())
            );
            assert_eq!(
                compute_const(OperatorTypeNode::IsNot, val),
                Some(result.into())
            );
        }
    }

    #[test]
    fn string_not_eq_fail() {
        let values = vec![
            lang_const_vec!["a", 'a'],
            lang_const_vec!["a", 'b'],
            lang_const_vec!["foo", true, "bar"],
        ];
        for val in values {
            assert_eq!(
                compute_const(OperatorTypeNode::NotEquals, val.clone()),
                None
            );
            assert_eq!(compute_const(OperatorTypeNode::IsNot, val), None);
        }
    }
}
