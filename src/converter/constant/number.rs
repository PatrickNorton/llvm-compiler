use std::borrow::Cow;
use std::fmt::Display;
use std::iter::repeat;
use std::sync::Arc;

use num::{BigInt, ToPrimitive};

use crate::converter::builtins::Builtins;
use crate::converter::type_obj::TypeObject;
use crate::util::decimal::BigDecimal;
use crate::util::{usize_to_bytes, U32_BYTES};

use super::{ConstantBytes, DecimalConstant, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumberConstant {
    Bigint(BigintConstant),
    Int(IntConstant),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BigintConstant {
    pub(super) value: Arc<BigInt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntConstant {
    pub(super) value: i32,
}

impl NumberConstant {
    pub fn big_value(&self) -> Cow<'_, BigInt> {
        match self {
            NumberConstant::Bigint(b) => Cow::Borrowed(b.get_value()),
            NumberConstant::Int(i) => Cow::Owned(i.get_value().into()),
        }
    }
}

impl BigintConstant {
    pub fn new(value: BigInt) -> Self {
        Self {
            value: Arc::new(value),
        }
    }

    pub fn get_value(&self) -> &BigInt {
        &self.value
    }

    pub fn str_value(&self) -> String {
        self.value.to_string()
    }

    pub fn repr_value(&self) -> String {
        self.value.to_string()
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.int_type()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let bi_bytes = Self::bigint_bytes(&self.value);
        let mut bytes = Vec::with_capacity(1 + U32_BYTES + bi_bytes.len());
        bytes.push(ConstantBytes::Bigint as u8);
        bytes.extend(usize_to_bytes(bi_bytes.len() / U32_BYTES));
        bytes.extend(bi_bytes);
        bytes
    }

    pub fn convert_bigint(value: &BigInt) -> Vec<u8> {
        let bi_bytes = Self::bigint_bytes(value);
        let mut bytes = Vec::with_capacity(bi_bytes.len() + U32_BYTES);
        bytes.extend(usize_to_bytes(bi_bytes.len() / U32_BYTES));
        bytes.extend(bi_bytes);
        bytes
    }

    fn bigint_bytes(value: &BigInt) -> Vec<u8> {
        // FIXME: Negative BigInts don't work
        let interior_bytes = value.to_bytes_be().1;
        let overflow = interior_bytes.len() % U32_BYTES;
        let added_bytes = if overflow == 0 {
            0
        } else {
            U32_BYTES - overflow
        };
        let mut bytes = Vec::with_capacity(interior_bytes.len() + added_bytes);
        bytes.extend(repeat(0).take(added_bytes));
        bytes.extend(interior_bytes);
        bytes
    }
}

impl IntConstant {
    pub fn new(value: i32) -> Self {
        Self { value }
    }

    pub fn get_value(&self) -> i32 {
        self.value
    }

    pub fn str_value(&self) -> String {
        self.value.to_string()
    }

    pub fn repr_value(&self) -> String {
        self.value.to_string()
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.int_type()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Int as u8);
        bytes.extend(self.value.to_be_bytes());
        bytes
    }
}

impl Display for BigintConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl Display for IntConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl TryFrom<LangConstant> for NumberConstant {
    type Error = LangConstant;

    fn try_from(value: LangConstant) -> Result<Self, Self::Error> {
        match value {
            LangConstant::Bigint(b) => Ok(NumberConstant::Bigint(b)),
            LangConstant::Int(i) => Ok(NumberConstant::Int(i)),
            x => Err(x),
        }
    }
}

impl From<NumberConstant> for LangConstant {
    fn from(x: NumberConstant) -> Self {
        match x {
            NumberConstant::Bigint(b) => LangConstant::Bigint(b),
            NumberConstant::Int(i) => LangConstant::Int(i),
        }
    }
}

impl From<BigInt> for LangConstant {
    fn from(value: BigInt) -> Self {
        match value.to_i32() {
            Option::Some(x) => LangConstant::Int(IntConstant::new(x)),
            Option::None => LangConstant::Bigint(BigintConstant::new(value)),
        }
    }
}

impl From<BigInt> for NumberConstant {
    fn from(value: BigInt) -> Self {
        match value.to_i32() {
            Option::Some(x) => NumberConstant::Int(IntConstant::new(x)),
            Option::None => NumberConstant::Bigint(BigintConstant::new(value)),
        }
    }
}

impl From<BigDecimal> for LangConstant {
    fn from(x: BigDecimal) -> Self {
        Self::Decimal(DecimalConstant::new(x))
    }
}

macro_rules! from_ints {
    ($($int:ty),+ $(,)?) => {
        $(from_int!($int);)+
    };
}

macro_rules! from_int {
    ($int:ty) => {
        impl From<$int> for LangConstant {
            fn from(value: $int) -> Self {
                match value.try_into() {
                    Result::Ok(x) => Self::Int(IntConstant::new(x)),
                    Result::Err(_) => Self::Bigint(BigintConstant::new(value.into())),
                }
            }
        }

        impl From<$int> for NumberConstant {
            fn from(value: $int) -> Self {
                match value.try_into() {
                    Result::Ok(x) => Self::Int(IntConstant::new(x)),
                    Result::Err(_) => Self::Bigint(BigintConstant::new(value.into())),
                }
            }
        }
    };
}

from_ints!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, usize, isize);

#[cfg(test)]
mod tests {
    use num::BigInt;

    use crate::converter::constant::{BigintConstant, ConstantBytes, IntConstant, NumberConstant};

    const BIGINT_BYTE: u8 = ConstantBytes::Bigint as u8;
    const INT_BYTE: u8 = ConstantBytes::Int as u8;

    #[test]
    fn big_value() {
        assert_eq!(
            *NumberConstant::Int(IntConstant::new(5)).big_value(),
            BigInt::from(5)
        );
        assert_eq!(
            *NumberConstant::Bigint(BigintConstant::new(BigInt::from(1_000_000))).big_value(),
            BigInt::from(1_000_000)
        );
    }

    #[test]
    fn big_bytes() {
        assert_eq!(
            BigintConstant::new(0.into()).to_bytes(),
            vec![BIGINT_BYTE, 0, 0, 0, 1, 0, 0, 0, 0]
        );
        assert_eq!(
            BigintConstant::new(1.into()).to_bytes(),
            vec![BIGINT_BYTE, 0, 0, 0, 1, 0, 0, 0, 1]
        );
        #[rustfmt::skip]
        let result_u64 = vec![
            BIGINT_BYTE,
            0, 0, 0, 2,
            0x12, 0x34, 0x56, 0x78,
            0x90, 0xAB, 0xCD, 0xEF,
        ];
        assert_eq!(
            BigintConstant::new(0x1234_5678_90AB_CDEFu64.into()).to_bytes(),
            result_u64
        );
        #[rustfmt::skip]
        let result_u64_padded = vec![
            BIGINT_BYTE,
            0, 0, 0, 2,
            0x00, 0x00, 0x12, 0x34,
            0x56, 0x78, 0x90, 0xAB,
        ];
        assert_eq!(
            BigintConstant::new(0x1234_5678_90ABu64.into()).to_bytes(),
            result_u64_padded
        );
    }

    #[test]
    fn convert_bigint() {
        assert_eq!(
            BigintConstant::convert_bigint(&0.into()),
            vec![0, 0, 0, 1, 0, 0, 0, 0]
        );
        assert_eq!(
            BigintConstant::convert_bigint(&1.into()),
            vec![0, 0, 0, 1, 0, 0, 0, 1]
        );
        #[rustfmt::skip]
        let result_u64 = vec![
            0, 0, 0, 2,
            0x12, 0x34, 0x56, 0x78,
            0x90, 0xAB, 0xCD, 0xEF,
        ];
        assert_eq!(
            BigintConstant::convert_bigint(&0x1234_5678_90AB_CDEFu64.into()),
            result_u64
        );
        #[rustfmt::skip]
        let result_u64_padded = vec![
            0, 0, 0, 2,
            0x00, 0x00, 0x12, 0x34,
            0x56, 0x78, 0x90, 0xAB,
        ];
        assert_eq!(
            BigintConstant::convert_bigint(&0x1234_5678_90ABu64.into()),
            result_u64_padded
        );
    }

    #[test]
    fn small_bytes() {
        assert_eq!(IntConstant::new(0).to_bytes(), vec![INT_BYTE, 0, 0, 0, 0]);
        assert_eq!(
            IntConstant::new(0x0ABADE66).to_bytes(),
            vec![INT_BYTE, 0x0A, 0xBA, 0xDE, 0x66]
        );
        assert_eq!(
            IntConstant::new(-1).to_bytes(),
            vec![INT_BYTE, 0xFF, 0xFF, 0xFF, 0xFF]
        );
    }

    #[test]
    fn big_str() {
        assert_eq!(BigintConstant::new(0.into()).str_value(), "0");
        assert_eq!(BigintConstant::new(1000.into()).str_value(), "1000");
        assert_eq!(
            BigintConstant::new(BigInt::from(10).pow(10)).str_value(),
            "10000000000"
        );
    }

    #[test]
    fn big_repr() {
        assert_eq!(BigintConstant::new(0.into()).repr_value(), "0");
        assert_eq!(BigintConstant::new(1000.into()).repr_value(), "1000");
        assert_eq!(
            BigintConstant::new(BigInt::from(10).pow(10)).repr_value(),
            "10000000000"
        );
    }

    #[test]
    fn small_str() {
        assert_eq!(IntConstant::new(0).str_value(), "0");
        assert_eq!(IntConstant::new(1000).str_value(), "1000");
        assert_eq!(IntConstant::new(1_000_000_000).str_value(), "1000000000");
    }

    #[test]
    fn small_repr() {
        assert_eq!(IntConstant::new(0).repr_value(), "0");
        assert_eq!(IntConstant::new(1000).repr_value(), "1000");
        assert_eq!(IntConstant::new(1_000_000_000).repr_value(), "1000000000");
    }
}
