use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::util::decimal::BigDecimal;
use num::bigint::Sign;
use num::{BigInt, BigUint, ToPrimitive, Zero};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Number {
    Integer(BigInt),
    Decimal(BigDecimal),
}

#[derive(Debug)]
pub struct NumberNode {
    line_info: LineInfo,
    value: Number,
}

impl Number {
    pub fn parse(input: &str) -> Option<(TokenType, usize)> {
        if let Option::Some(x) = input.strip_prefix("0x") {
            Self::number_of(x, 16).map(|(t, x)| (t, x + 2))
        } else if let Option::Some(x) = input.strip_prefix("0o") {
            Self::number_of(x, 8).map(|(t, x)| (t, x + 2))
        } else if let Option::Some(x) = input.strip_prefix("0b") {
            Self::number_of(x, 2).map(|(t, x)| (t, x + 2))
        } else {
            Self::number_of(input, 10)
        }
    }

    fn number_of(input: &str, radix: u32) -> Option<(TokenType, usize)> {
        if input.is_empty() {
            return Option::None;
        }
        let s = input;
        if s.starts_with('_') {
            // Must lead with a real digit!
            return Option::None;
        }

        // First normalize all characters to plain digit values
        let mut v = Vec::with_capacity(s.len());
        let mut dot_pos = Option::None;
        let mut digit_count = None;
        for (i, b) in s.bytes().enumerate() {
            let d = match b {
                b'0'..=b'9' => b - b'0',
                b'a'..=b'z' => b - b'a' + 10,
                b'A'..=b'Z' => b - b'A' + 10,
                b'_' => continue,
                b'.' => match dot_pos {
                    Option::Some(_) => {
                        digit_count = Some(i);
                        break;
                    }
                    Option::None => {
                        dot_pos = Option::Some(v.len());
                        continue;
                    }
                },
                _ => {
                    digit_count = Some(i);
                    if i == 0 {
                        return None;
                    } else {
                        break;
                    }
                }
            };
            if d < radix as u8 {
                v.push(d);
            } else if i > 0 && dot_pos == Some(i - 1) {
                // Deal with trailing dot that isn't a number,
                // for example in `tuple.0.method()`
                // FIXME: Deal with trailing letters in hex numbers:
                // `0x1.foo()` will fail b/c `f` is a valid hex digit
                dot_pos = None;
                break;
            } else {
                return Option::None;
            }
        }

        let v_len = v.len();
        let res = if radix.is_power_of_two() {
            // Powers of two can use bitwise masks and shifting instead of multiplication
            let bits = ilog2(radix);
            let mut result = BigUint::zero();
            for digit in v {
                result <<= bits;
                result += digit;
            }
            result
        } else {
            let mut result = BigUint::zero();
            for digit in v {
                result *= radix;
                result += digit;
            }
            result
        };
        if let Option::Some(dot) = dot_pos {
            let scale = v_len - dot;
            let dec = BigDecimal::new(BigInt::from_biguint(Sign::Plus, res), scale as isize);
            let num = Number::Decimal(dec);
            Some((TokenType::Number(num), digit_count.unwrap_or(input.len())))
        } else {
            let num = Number::Integer(BigInt::from_biguint(Sign::Plus, res));
            Some((TokenType::Number(num), digit_count.unwrap_or(input.len())))
        }
    }
}

impl NumberNode {
    pub fn new(line_info: LineInfo, value: Number) -> Self {
        Self { line_info, value }
    }

    pub fn deconstruct(self) -> (LineInfo, Number) {
        (self.line_info, self.value)
    }

    pub fn get_value(&self) -> &Number {
        &self.value
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<NumberNode> {
        let (line_info, token) = tokens.next_token()?.deconstruct();
        match token {
            TokenType::Number(n) => Ok(NumberNode::new(line_info, n)),
            _ => panic!("Expected a number"),
        }
    }
}

macro_rules! to_primitive_fn {
    ($name:ident, $ty:ty) => {
        fn $name(&self) -> Option<$ty> {
            match self {
                Number::Integer(i) => i.$name(),
                Number::Decimal(_) => None,
            }
        }
    };
}

impl ToPrimitive for Number {
    to_primitive_fn!(to_u8, u8);
    to_primitive_fn!(to_i8, i8);
    to_primitive_fn!(to_u16, u16);
    to_primitive_fn!(to_i16, i16);
    to_primitive_fn!(to_u32, u32);
    to_primitive_fn!(to_i32, i32);
    to_primitive_fn!(to_u64, u64);
    to_primitive_fn!(to_i64, i64);
    to_primitive_fn!(to_u128, u128);
    to_primitive_fn!(to_i128, i128);
    to_primitive_fn!(to_usize, usize);
    to_primitive_fn!(to_isize, isize);
}

impl Lined for NumberNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

#[inline]
fn ilog2(radix: u32) -> u32 {
    u32::BITS - radix.leading_zeros() - 1
}
