use std::fmt::Display;

use crate::converter::builtins::Builtins;
use crate::util::U32_BYTES;

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuiltinConstant {
    value: u16,
}

impl BuiltinConstant {
    pub const fn new(value: u16) -> Self {
        Self { value }
    }

    pub fn fmt_name(
        &self,
        builtins: &Builtins,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        builtins
            .builtin_name(self.value)
            .expect("Must be a valid builtin")
            .fmt(f)
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Builtin as u8);
        bytes.extend((self.value as u32).to_be_bytes());
        bytes
    }
}

impl From<BuiltinConstant> for LangConstant {
    fn from(x: BuiltinConstant) -> Self {
        LangConstant::Builtin(x)
    }
}
