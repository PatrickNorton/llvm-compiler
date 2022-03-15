use std::borrow::Cow;
use std::fmt::Display;

use crate::util::{string_escape, U32_BYTES};

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharConstant {
    pub(super) value: char,
}

impl CharConstant {
    pub const fn new(value: char) -> Self {
        Self { value }
    }

    pub fn get_value(&self) -> char {
        self.value
    }

    pub fn name(c: char) -> Cow<'static, str> {
        match c {
            '\'' => r#"c"'""#.into(),
            '"' => r#"c'"'"#.into(),
            _ => format!("c'{}'", string_escape::escaped(c)).into(),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Char as u8);
        bytes.extend((self.value as u32).to_be_bytes());
        bytes
    }
}

impl From<char> for CharConstant {
    fn from(x: char) -> Self {
        Self::new(x)
    }
}

impl From<char> for LangConstant {
    fn from(x: char) -> Self {
        LangConstant::Char(x.into())
    }
}

impl From<CharConstant> for LangConstant {
    fn from(x: CharConstant) -> Self {
        LangConstant::Char(x)
    }
}

impl Display for CharConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            '\'' => r#"c"'""#.fmt(f),
            '"' => r#"c'"'"#.fmt(f),
            c => write!(f, "c'{}'", string_escape::escaped(c)),
        }
    }
}
