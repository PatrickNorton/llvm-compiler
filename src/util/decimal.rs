use std::fmt::Display;
use std::ops::Add;

use num::{BigInt, Zero};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BigDecimal {
    data: BigInt,
    scale: isize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DecimalRef<'a> {
    data: &'a BigInt,
    scale: isize,
}

impl BigDecimal {
    pub fn new(data: BigInt, scale: isize) -> BigDecimal {
        BigDecimal { data, scale }
    }

    pub fn as_ref(&self) -> DecimalRef<'_> {
        DecimalRef {
            data: &self.data,
            scale: self.scale,
        }
    }

    pub fn data(&self) -> &BigInt {
        &self.data
    }
}

impl<'a> DecimalRef<'a> {
    pub fn from_int(value: &'a BigInt) -> Self {
        Self {
            data: value,
            scale: 0,
        }
    }

    pub fn precision(&self) -> usize {
        todo!()
    }

    pub fn move_point_left(self, shift: usize) -> DecimalRef<'a> {
        todo!()
    }

    pub fn scale(&self) -> isize {
        self.scale
    }
}

impl Zero for BigDecimal {
    fn zero() -> Self {
        Self::new(BigInt::zero(), 0)
    }

    fn is_zero(&self) -> bool {
        self.data.is_zero()
    }
}

impl Add<BigDecimal> for BigDecimal {
    type Output = BigDecimal;

    fn add(self, rhs: BigDecimal) -> Self::Output {
        todo!()
    }
}

impl Display for BigDecimal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for DecimalRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
