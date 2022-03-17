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

    pub fn str_value(&self) -> String {
        self.value.to_string()
    }

    pub fn repr_value(&self) -> String {
        self.to_string()
    }

    pub fn str_bytes(text: &str) -> Vec<u8> {
        let mut result = Vec::with_capacity(text.len() + 4);
        result.extend(&usize_to_bytes(text.len()));
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

#[cfg(test)]
mod tests {
    use crate::converter::constant::{ConstantBytes, StringConstant};

    const ASCII_BYTE: u8 = ConstantBytes::Ascii as u8;
    const STRING_BYTE: u8 = ConstantBytes::Str as u8;

    #[test]
    fn str_bytes() {
        assert_eq!(StringConstant::str_bytes(""), vec![0, 0, 0, 0]);
        assert_eq!(
            StringConstant::str_bytes("abcdefg"),
            vec![0, 0, 0, 7, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67]
        );
        assert_eq!(
            StringConstant::str_bytes("abc\u{1000}"),
            vec![0, 0, 0, 6, 0x61, 0x62, 0x63, 0xe1, 0x80, 0x80]
        );
    }

    #[test]
    fn to_bytes() {
        assert_eq!(
            StringConstant::new(String::new()).to_bytes(),
            vec![ASCII_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            StringConstant::new("abcdefg".to_string()).to_bytes(),
            vec![ASCII_BYTE, 0, 0, 0, 7, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67]
        );
        assert_eq!(
            StringConstant::new("abc\u{1000}".to_string()).to_bytes(),
            vec![STRING_BYTE, 0, 0, 0, 6, 0x61, 0x62, 0x63, 0xe1, 0x80, 0x80]
        );
    }

    #[test]
    fn str_str() {
        let strings = &["", "a", "abcdefg", "abc\n\\\u{80}\u{e000}"];
        for string in strings {
            assert_eq!(StringConstant::new(string.to_string()).str_value(), *string);
        }
    }

    #[test]
    fn str_repr() {
        assert_eq!(StringConstant::new(String::new()).repr_value(), r#""""#);
        assert_eq!(
            StringConstant::new("abcdefg".to_string()).repr_value(),
            r#""abcdefg""#
        );
        assert_eq!(
            StringConstant::new("abc\n\\\u{80}\u{e000}".to_string()).repr_value(),
            r#""abc\n\\\x80\uE000""#
        );
    }
}
