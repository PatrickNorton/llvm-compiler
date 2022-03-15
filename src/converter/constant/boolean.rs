use std::fmt::Display;

use crate::converter::constant::ConstantBytes;

use super::LangConstant;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolConstant {
    value: bool,
}

impl BoolConstant {
    pub const fn new(value: bool) -> Self {
        Self { value }
    }

    pub const fn bool_value(&self) -> bool {
        self.value
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        vec![ConstantBytes::Bool as u8, self.value.into()]
    }
}

impl Display for BoolConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(if self.value { "true" } else { "false" })
    }
}

impl From<BoolConstant> for LangConstant {
    fn from(x: BoolConstant) -> Self {
        LangConstant::Bool(x)
    }
}

impl From<bool> for LangConstant {
    fn from(x: bool) -> Self {
        LangConstant::Bool(x.into())
    }
}

impl From<bool> for BoolConstant {
    fn from(x: bool) -> Self {
        Self::new(x)
    }
}
