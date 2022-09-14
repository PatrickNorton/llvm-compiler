use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Add;
use std::str::FromStr;

use num::bigint::ParseBigIntError;
use num::traits::Pow;
use num::{BigInt, Num, Signed, ToPrimitive, Zero};

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

#[derive(Debug, PartialEq, Eq)]
pub enum ParseBigDecimalError {
    ParseDecimal(ParseFloatError),
    ParseInt(ParseIntError),
    ParseBigInt(ParseBigIntError),
    Empty,
    Other(String),
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

    pub fn scale(&self) -> isize {
        self.scale
    }

    #[inline]
    fn from_str_radix(s: &str, radix: u32) -> Result<BigDecimal, ParseBigDecimalError> {
        if radix != 10 {
            return Err(ParseBigDecimalError::Other(String::from(
                "The radix for decimal MUST be 10",
            )));
        }
        let exp_separator = &['e', 'E'];
        // split slice into base and exponent parts
        let (base_part, exponent_value) = match s.find(exp_separator) {
            // exponent defaults to 0 if (e|E) not found
            None => (s, 0),
            // split and parse exponent field
            Some(loc) => {
                // slice up to `loc` and 1 after to skip the 'e' char
                let (base, exp) = (&s[..loc], &s[loc + 1..]);

                // special consideration for rust 1.0.0 which would not
                // parse a leading '+'
                let exp = match exp.chars().next() {
                    Some('+') => &exp[1..],
                    _ => exp,
                };

                (base, isize::from_str(exp)?)
            }
        };
        // TEMPORARY: Test for emptiness - remove once BigInt supports similar error
        if base_part.is_empty() {
            return Err(ParseBigDecimalError::Empty);
        }
        // split decimal into a digit string and decimal-point offset
        let (digits, decimal_offset): (String, _) = match base_part.find('.') {
            // No dot! pass directly to BigInt
            None => (base_part.to_string(), 0),
            // decimal point found - necessary copy into new string buffer
            Some(loc) => {
                // split into leading and trailing digits
                let (lead, trail) = (&base_part[..loc], &base_part[loc + 1..]);
                // copy all leading characters into 'digits' string
                let mut digits = String::from(lead);
                // copy all trailing characters after '.' into the digits string
                digits.push_str(trail);
                (digits, trail.len() as isize)
            }
        };
        let scale = decimal_offset - exponent_value;
        let big_int = BigInt::from_str_radix(&digits, radix)?;
        Ok(BigDecimal::new(big_int, scale))
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
        // As described in "Bit Twiddling Hacks" by Sean Anderson,
        // (http://graphics.stanford.edu/~seander/bithacks.html)
        // integer log 10 of x is within 1 of (2776511644261678566/2^63) *
        // (1 + integer log 2 of x). The fraction 2776511644261678566/2^63
        // approximates log10(2). So we first do a version of log2 and
        // then scale and check against powers table. This is a little
        // simpler in present context than the version in Hacker's
        // Delight sec 11-4.
        if self.data.is_zero() {
            1
        } else {
            // TODO: When feature(int_log) (#70887) stabilizes, try converting
            // to a u64 and returning `log10(x) + 1`
            let r = (((self.data.bits() + 1) as u128 * 2776511644261678566u128) >> 63) as usize;
            if self.data < &BigInt::from(10).pow(r) {
                r
            } else {
                r + 1
            }
        }
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

const LONG_TEN_POWERS_TABLE: &[u64] = &[
    1,                    // 0 / 10^0
    10,                   // 1 / 10^1
    100,                  // 2 / 10^2
    1000,                 // 3 / 10^3
    10000,                // 4 / 10^4
    100000,               // 5 / 10^5
    1000000,              // 6 / 10^6
    10000000,             // 7 / 10^7
    100000000,            // 8 / 10^8
    1000000000,           // 9 / 10^9
    10000000000,          // 10 / 10^10
    100000000000,         // 11 / 10^11
    1000000000000,        // 12 / 10^12
    10000000000000,       // 13 / 10^13
    100000000000000,      // 14 / 10^14
    1000000000000000,     // 15 / 10^15
    10000000000000000,    // 16 / 10^16
    100000000000000000,   // 17 / 10^17
    1000000000000000000,  // 18 / 10^18
    10000000000000000000, // 19 / 10^19
];

impl Add<BigDecimal> for BigDecimal {
    type Output = BigDecimal;

    fn add(self, rhs: BigDecimal) -> Self::Output {
        if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else {
            add_bigs(&self.data, self.scale, &rhs.data, rhs.scale)
        }
    }
}

fn add_bigs(first: &BigInt, scale1: isize, second: &BigInt, scale2: isize) -> BigDecimal {
    let (mut first, mut second) = (first, second);
    let mut r_scale = scale1;
    let s_diff = r_scale as i128 - scale2 as i128;
    let temp;
    if s_diff != 0 {
        if s_diff < 0 {
            let raise = check_scale(first, -s_diff);
            r_scale = scale2;
            temp = big_multiply_power_ten(first, raise);
            first = &temp;
        } else {
            let raise = check_scale(second, s_diff);
            temp = big_multiply_power_ten(second, raise);
            second = &temp;
        }
    }
    let sum = first + second;
    BigDecimal::new(sum, r_scale)
}

fn check_scale(int_val: &BigInt, scale: i128) -> isize {
    scale.to_isize().unwrap_or_else(|| {
        if !int_val.is_zero() {
            panic!("{}", if scale > 0 { "Overflow" } else { "Underflow" });
        }
        if scale > isize::MAX as i128 {
            isize::MAX
        } else {
            isize::MIN
        }
    })
}

fn big_multiply_power_ten(value: &BigInt, n: isize) -> BigInt {
    if n <= 0 {
        value.clone()
    } else if let Option::Some(x) = LONG_TEN_POWERS_TABLE.get(n as usize) {
        value * x
    } else {
        value * BigInt::from(10).pow(n as usize)
    }
}

impl Display for BigDecimal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_dec(&self.data, self.scale, f)
    }
}

impl Display for DecimalRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_dec(self.data, self.scale, f)
    }
}

impl FromStr for BigDecimal {
    type Err = ParseBigDecimalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_str_radix(s, 10)
    }
}

impl From<ParseBigIntError> for ParseBigDecimalError {
    fn from(x: ParseBigIntError) -> Self {
        ParseBigDecimalError::ParseBigInt(x)
    }
}

impl From<ParseIntError> for ParseBigDecimalError {
    fn from(x: ParseIntError) -> Self {
        ParseBigDecimalError::ParseInt(x)
    }
}

fn display_dec(
    int_val: &BigInt,
    scale: isize,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    // TODO: Reduce amount of string-allocation/copying required
    // Aquire the absolute integer as a decimal string
    let mut abs_int = int_val.abs().to_str_radix(10);
    // Split the representation at the decimal point
    let (before, after) = if scale >= abs_int.len() as isize {
        // First case: the integer representation falls
        // completely behind the decimal point
        let scale = scale as usize;
        let after = "0".repeat(scale - abs_int.len()) + abs_int.as_str();
        ("0".to_string(), after)
    } else {
        // Second case: the integer representation falls
        // around, or before the decimal point
        let location = abs_int.len() as isize - scale;
        if location > abs_int.len() as isize {
            // Case 2.1, entirely before the decimal point
            // We should prepend zeros
            let zeros = location as usize - abs_int.len();
            let abs_int = abs_int + "0".repeat(zeros as usize).as_str();
            (abs_int, "".to_string())
        } else {
            // Case 2.2, somewhere around the decimal point
            // Just split it in two
            let after = abs_int.split_off(location as usize);
            (abs_int, after)
        }
    };
    // Alter precision after the decimal point
    let after = if let Some(precision) = f.precision() {
        let len = after.len();
        if len < precision {
            let mut tmp = after;
            tmp.push_str(&"0".repeat(precision - len));
            tmp
        } else {
            // TODO: Should we round?
            let mut tmp = after;
            tmp.truncate(precision);
            tmp
        }
    } else {
        after
    };
    // Concatenate everything
    let complete_without_sign = if !after.is_empty() {
        before + "." + after.as_str()
    } else {
        before
    };
    // pad_integral does the right thing although we have a decimal
    f.pad_integral(!int_val.is_negative(), "", &complete_without_sign)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use num::Zero;

    use crate::util::decimal::BigDecimal;

    #[test]
    fn add_zero() {
        assert_eq!(BigDecimal::zero() + BigDecimal::zero(), BigDecimal::zero());
        for x in 1..100 {
            assert_eq!(
                BigDecimal::zero() + BigDecimal::new(x.into(), 0),
                BigDecimal::new(x.into(), 0)
            );
            assert_eq!(
                BigDecimal::new(x.into(), 0) + BigDecimal::zero(),
                BigDecimal::new(x.into(), 0)
            );
        }
    }

    #[test]
    fn add_integers() {
        let vals = &[
            ("12.34", "1.234", "13.574"),
            ("12.34", "-1.234", "11.106"),
            ("1234e6", "1234e-6", "1234000000.001234"),
            ("1234e-6", "1234e6", "1234000000.001234"),
            ("18446744073709551616.0", "1", "18446744073709551617.0"),
            ("184467440737e3380", "0", "184467440737e3380"),
        ];

        for &(x, y, z) in vals {
            let a = BigDecimal::from_str(x).unwrap();
            let b = BigDecimal::from_str(y).unwrap();
            let c = BigDecimal::from_str(z).unwrap();

            assert_eq!(a.clone() + b.clone(), c);
            assert_eq!(b + a, c);

            // TODO: Other Add impls
            // assert_eq!(a.clone() + &b, c);
            // assert_eq!(&a + b.clone(), c);
            // assert_eq!(&a + &b, c);

            // a += b;
            // assert_eq!(a, c);
        }
    }

    #[test]
    fn precision() {
        let vals = &[
            ("0", 1),
            ("10", 2),
            ("1", 1),
            ("9", 1),
            ("999", 3),
            ("1000", 4),
            ("9900", 4),
            ("9999", 4),
            ("10000", 5),
            ("99999", 5),
            ("100000", 6),
            ("999999", 6),
            ("1000000", 7),
            ("9999999", 7),
            ("999999999999", 12),
            ("999999999999999999999999", 24),
            ("999999999999999999999999999999999999999999999999", 48),
            (
                "999999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999999",
                96,
            ),
            (
                "199999911199999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999000",
                96,
            ),
            (
                "999999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999991",
                192,
            ),
            (
                "199999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999999\
                 999999999999999999999999999999999999999999999999",
                192,
            ),
        ];
        for &(x, y) in vals {
            let a = BigDecimal::from_str(x).unwrap();
            let b = a.as_ref().precision();
            assert_eq!(b, y);
        }
    }
}
