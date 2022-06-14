use std::fmt::Display;
use std::sync::Arc;

use num::Zero;

use crate::converter::builtins::BuiltinRef;
use crate::converter::constant::ConstantBytes;
use crate::converter::type_obj::TypeObject;
use crate::util::decimal::BigDecimal;
use crate::util::{isize_to_bytes, usize_to_bytes};

use super::LangConstant;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DecimalConstant {
    pub(super) value: Arc<BigDecimal>,
}

impl DecimalConstant {
    pub fn new(value: BigDecimal) -> Self {
        Self {
            value: Arc::new(value),
        }
    }

    pub fn get_value(&self) -> &BigDecimal {
        &self.value
    }

    pub fn get_type<'a>(&self, builtins: BuiltinRef<'a>) -> &'a TypeObject {
        builtins.dec_type()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![ConstantBytes::Decimal as u8];
        let bigint_val = self.value.data();
        let byte_array = if bigint_val.is_zero() {
            Vec::new()
        } else {
            bigint_val.to_bytes_be().1
        };
        bytes.extend(usize_to_bytes(byte_array.len()));
        bytes.extend(isize_to_bytes(self.value.scale()));
        bytes.extend(byte_array);
        bytes
    }
}

impl Display for DecimalConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl From<BigDecimal> for LangConstant {
    fn from(x: BigDecimal) -> Self {
        LangConstant::Decimal(DecimalConstant::new(x))
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;

    use crate::converter::constant::{ConstantBytes, DecimalConstant};
    use crate::util::decimal::BigDecimal;

    const DECIMAL_BYTE: u8 = ConstantBytes::Decimal as u8;

    #[test]
    fn dec_zero_bytes() {
        let dec = DecimalConstant::new(BigDecimal::zero());
        let bytes = &[DECIMAL_BYTE, 0, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(dec.to_bytes(), bytes);
    }

    fn dec_pairs() -> Vec<(BigDecimal, Vec<u8>)> {
        vec![
            (
                BigDecimal::new(1u32.into(), 0),
                vec![DECIMAL_BYTE, 0, 0, 0, 1, 0, 0, 0, 0, 1],
            ),
            (
                BigDecimal::new(255u32.into(), 0),
                vec![DECIMAL_BYTE, 0, 0, 0, 1, 0, 0, 0, 0, 255],
            ),
            (
                BigDecimal::new(256u32.into(), 0),
                vec![DECIMAL_BYTE, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0],
            ),
            (
                BigDecimal::new(u32::MAX.into(), 0),
                vec![DECIMAL_BYTE, 0, 0, 0, 4, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff],
            ),
        ]
    }

    #[test]
    fn dec_whole_bytes() {
        for (dec, bytes) in dec_pairs() {
            assert_eq!(DecimalConstant::new(dec).to_bytes(), bytes);
        }
    }
}
