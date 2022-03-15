use std::fmt::Display;
use std::sync::Arc;

use crate::util::{string_escape, usize_to_bytes, U32_BYTES};

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringConstant {
    pub(super) value: Arc<str>,
}

impl StringConstant {
    pub fn new(value: String) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn get_value(&self) -> &str {
        &self.value
    }

    pub fn str_bytes(text: &str) -> Vec<u8> {
        let mut result = Vec::with_capacity(text.len() + 4);
        result.extend(&usize_to_bytes(result.len()));
        result.extend(text.as_bytes());
        result
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + 2 * U32_BYTES + self.value.len());
        bytes.push(if self.value.is_ascii() {
            ConstantBytes::Ascii
        } else {
            ConstantBytes::Str
        } as u8);
        bytes.extend(Self::str_bytes(&self.value));
        bytes
    }
}

impl Display for StringConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, r#""{}""#, string_escape::escape(&self.value))
    }
}

impl TryFrom<LangConstant> for StringConstant {
    type Error = LangConstant;

    fn try_from(value: LangConstant) -> Result<Self, Self::Error> {
        match value {
            LangConstant::String(x) => Ok(x),
            val => Err(val),
        }
    }
}

impl From<String> for StringConstant {
    fn from(x: String) -> Self {
        Self::new(x)
    }
}

impl From<String> for LangConstant {
    fn from(x: String) -> Self {
        LangConstant::String(x.into())
    }
}

impl From<&str> for StringConstant {
    fn from(value: &str) -> Self {
        StringConstant {
            value: value.into(),
        }
    }
}

impl From<&str> for LangConstant {
    fn from(value: &str) -> Self {
        LangConstant::String(value.into())
    }
}

impl From<StringConstant> for LangConstant {
    fn from(x: StringConstant) -> Self {
        Self::String(x)
    }
}
