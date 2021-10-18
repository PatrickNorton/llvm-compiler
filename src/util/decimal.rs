use num::BigInt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BigDecimal {
    data: BigInt,
    scale: isize,
}

impl BigDecimal {
    pub fn new(data: BigInt, scale: isize) -> BigDecimal {
        BigDecimal { data, scale }
    }
}
