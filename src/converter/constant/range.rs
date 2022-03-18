use std::fmt::Display;
use std::sync::Arc;

use num::{BigInt, One, ToPrimitive};

use crate::converter::builtins::Builtins;
use crate::converter::constant::BigintConstant;
use crate::converter::type_obj::TypeObject;

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RangeConstant {
    value: Arc<Range>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Range {
    start: Option<BigInt>,
    stop: Option<BigInt>,
    step: Option<BigInt>,
}

impl RangeConstant {
    pub fn new(start: Option<BigInt>, stop: Option<BigInt>, step: Option<BigInt>) -> Self {
        Self {
            value: Arc::new(Range { start, stop, step }),
        }
    }

    pub fn get_start(&self) -> &Option<BigInt> {
        &self.value.start
    }

    pub fn get_stop(&self) -> &Option<BigInt> {
        &self.value.stop
    }

    pub fn get_step(&self) -> &Option<BigInt> {
        &self.value.step
    }

    pub fn str_value(&self) -> String {
        self.value.to_string()
    }

    pub fn repr_value(&self) -> String {
        self.value.to_string()
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.range_type()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![ConstantBytes::Range as u8];
        add_to_bytes(&mut bytes, &self.value.start);
        add_to_bytes(&mut bytes, &self.value.stop);
        add_to_bytes(&mut bytes, &self.value.step);
        bytes
    }
}

impl Range {
    pub const fn new(start: Option<BigInt>, stop: Option<BigInt>, step: Option<BigInt>) -> Self {
        Self { start, stop, step }
    }
}

fn add_to_bytes(bytes: &mut Vec<u8>, value: &Option<BigInt>) {
    match value {
        Option::None => bytes.push(0),
        Option::Some(val) => match val.to_u32() {
            Option::Some(val) => {
                bytes.push(1);
                bytes.extend(val.to_be_bytes());
            }
            Option::None => {
                bytes.push(2);
                bytes.extend(BigintConstant::convert_bigint(val));
            }
        },
    }
}

impl Display for RangeConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.step {
            Option::None => write!(f, "[{}:{}]", RangeFmt(&self.start), RangeFmt(&self.stop)),
            Option::Some(x) if x.is_one() => {
                write!(f, "[{}:{}]", RangeFmt(&self.start), RangeFmt(&self.stop))
            }
            Option::Some(step) => write!(
                f,
                "[{}:{}:{}]",
                RangeFmt(&self.start),
                RangeFmt(&self.stop),
                step
            ),
        }
    }
}

impl From<RangeConstant> for LangConstant {
    fn from(x: RangeConstant) -> Self {
        Self::Range(x)
    }
}

impl From<Range> for RangeConstant {
    fn from(range: Range) -> Self {
        Self {
            value: Arc::new(range),
        }
    }
}

struct RangeFmt<'a>(&'a Option<BigInt>);

impl Display for RangeFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Option::Some(val) = self.0 {
            Display::fmt(val, f)
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use num::{BigInt, One, Zero};
    use once_cell::sync::Lazy;

    use crate::converter::constant::{ConstantBytes, Range, RangeConstant};

    const RANGE_BYTE: u8 = ConstantBytes::Range as u8;

    static RANGE_STRS: Lazy<Vec<(Range, &str)>> = Lazy::new(|| {
        vec![
            (Range::new(None, None, None), "[:]"),
            (Range::new(Some(BigInt::zero()), None, None), "[0:]"),
            (Range::new(None, Some(BigInt::zero()), None), "[:0]"),
            (Range::new(None, None, Some(BigInt::zero())), "[::0]"),
            (
                Range::new(Some(BigInt::zero()), Some(BigInt::zero()), None),
                "[0:0]",
            ),
            (
                Range::new(Some(BigInt::zero()), None, Some(BigInt::zero())),
                "[0::0]",
            ),
            (
                Range::new(None, Some(BigInt::zero()), Some(BigInt::zero())),
                "[:0:0]",
            ),
            (
                Range::new(
                    Some(BigInt::zero()),
                    Some(BigInt::zero()),
                    Some(BigInt::zero()),
                ),
                "[0:0:0]",
            ),
            (
                Range::new(
                    Some(BigInt::one()),
                    Some((-123_456).into()),
                    Some(100_000_000_000_000u64.into()),
                ),
                "[1:-123456:100000000000000]",
            ),
        ]
    });

    #[test]
    fn range_display_zero() {
        for (range, text) in &*RANGE_STRS {
            assert_eq!(format!("{}", range), *text);
        }
    }

    #[test]
    fn range_bytes() {
        assert_eq!(
            RangeConstant::new(None, None, None).to_bytes(),
            vec![RANGE_BYTE, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(Some(BigInt::zero()), None, None).to_bytes(),
            vec![RANGE_BYTE, 1, 0, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(None, Some(BigInt::zero()), None).to_bytes(),
            vec![RANGE_BYTE, 0, 1, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(None, None, Some(BigInt::zero())).to_bytes(),
            vec![RANGE_BYTE, 0, 0, 1, 0, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(Some(BigInt::zero()), Some(BigInt::zero()), None).to_bytes(),
            vec![RANGE_BYTE, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(Some(BigInt::zero()), None, Some(BigInt::zero())).to_bytes(),
            vec![RANGE_BYTE, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(None, Some(BigInt::zero()), Some(BigInt::zero())).to_bytes(),
            vec![RANGE_BYTE, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
        );
        assert_eq!(
            RangeConstant::new(
                Some(BigInt::zero()),
                Some(BigInt::zero()),
                Some(BigInt::zero())
            )
            .to_bytes(),
            vec![RANGE_BYTE, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0]
        );
    }

    #[test]
    fn complex_range_bytes() {
        #[rustfmt::skip]
        let complex_bytes = vec![
            RANGE_BYTE,
            1, 0, 0, 0, 1,
            1, 0x12, 0x34, 0x56, 0x78,
            2, 0, 0, 0, 2,
            0x12, 0x34, 0x56, 0x78, 0x90, 0xAB, 0xCD, 0xEF,
        ];
        assert_eq!(
            RangeConstant::new(
                Some(BigInt::one()),
                Some(0x1234_5678.into()),
                Some(0x1234_5678_90AB_CDEFu64.into())
            )
            .to_bytes(),
            complex_bytes
        );
    }

    #[test]
    fn range_str() {
        for (range, text) in &*RANGE_STRS {
            assert_eq!(RangeConstant::from(range.clone()).str_value(), *text);
        }
    }

    #[test]
    fn range_repr() {
        for (range, text) in &*RANGE_STRS {
            assert_eq!(RangeConstant::from(range.clone()).repr_value(), *text);
        }
    }
}
