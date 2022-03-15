use std::fmt::Display;
use std::sync::Arc;

use num::{BigInt, One, ToPrimitive};

use crate::converter::constant::BigintConstant;

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RangeConstant {
    value: Arc<Range>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
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

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![ConstantBytes::Range as u8];
        add_to_bytes(&mut bytes, &self.value.start);
        add_to_bytes(&mut bytes, &self.value.stop);
        add_to_bytes(&mut bytes, &self.value.step);
        bytes
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
        match &self.value.step {
            Option::None => write!(
                f,
                "[{}:{}]",
                RangeFmt(&self.value.start),
                RangeFmt(&self.value.stop)
            ),
            Option::Some(x) if x.is_one() => write!(
                f,
                "[{}:{}]",
                RangeFmt(&self.value.start),
                RangeFmt(&self.value.stop)
            ),
            Option::Some(step) => write!(
                f,
                "[{}:{}:{}]",
                RangeFmt(&self.value.start),
                RangeFmt(&self.value.stop),
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
