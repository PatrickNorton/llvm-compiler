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
        bytes.push(self.value.align.to_byte());
        bytes.push(self.value.sign.to_byte());
        bytes.push(self.hash_zero_byte());
        bytes.extend(usize_to_bytes(self.value.min_width));
        bytes.extend(usize_to_bytes(self.value.precision));
        bytes.push(self.value.fmt_type.to_byte());
        bytes
    }

    fn hash_zero_byte(&self) -> u8 {
        u8::from(self.value.hash) | (u8::from(self.value.zero) << 1)
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

#[cfg(test)]
mod tests {
    use crate::converter::constant::{ConstantBytes, FormatConstant};
    use crate::parser::formatted_string::FormatInfo;

    const FMT_BYTE: u8 = ConstantBytes::Format as u8;

    #[test]
    fn empty_fmt_bytes() {
        #[rustfmt::skip]
        let fmt_bytes = &[
            FMT_BYTE,
            0, 0, 0, 0,
            b'<',
            b'-',
            0,
            0, 0, 0, 0,
            0, 0, 0, 0,
            b's',
        ];
        assert_eq!(
            FormatConstant::new(FormatInfo::empty()).to_bytes(),
            fmt_bytes
        );
    }
}
