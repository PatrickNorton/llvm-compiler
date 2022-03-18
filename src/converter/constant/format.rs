use std::fmt::Display;

use crate::converter::constant::ConstantBytes;
use crate::converter::type_obj::TypeObject;
use crate::parser::formatted_string::FormatInfo;
use crate::util::usize_to_bytes;

use super::LangConstant;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FormatConstant {
    value: FormatInfo,
}

impl FormatConstant {
    pub fn new(value: FormatInfo) -> Self {
        Self { value }
    }

    pub fn get_type(&self) -> &TypeObject {
        panic!("Cannot get type of FormatConstant")
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![ConstantBytes::Format as u8];
        bytes.extend((self.value.fill as u32).to_be_bytes());
        bytes.push(map_null(self.value.align, '<').try_into().unwrap());
        bytes.push(map_null(self.value.sign, '-').try_into().unwrap());
        bytes.push(self.hash_zero_byte());
        bytes.extend(usize_to_bytes(self.value.min_width));
        bytes.extend(usize_to_bytes(self.value.precision));
        bytes.push(map_null(self.value.fmt_type, 's').try_into().unwrap());
        bytes
    }

    fn hash_zero_byte(&self) -> u8 {
        u8::from(self.value.hash) | (u8::from(self.value.zero) << 1)
    }
}

fn map_null(x: char, replace: char) -> char {
    if x == '\0' {
        replace
    } else {
        x
    }
}

impl Display for FormatConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl From<FormatConstant> for LangConstant {
    fn from(x: FormatConstant) -> Self {
        LangConstant::Fmt(x)
    }
}

impl From<FormatInfo> for FormatConstant {
    fn from(x: FormatInfo) -> Self {
        FormatConstant::new(x)
    }
}

impl From<FormatInfo> for LangConstant {
    fn from(x: FormatInfo) -> Self {
        Self::Fmt(x.into())
    }
}
