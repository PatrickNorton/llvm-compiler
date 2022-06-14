use std::borrow::Cow;
use std::fmt::Display;

use crate::converter::builtins::{BuiltinRef, Builtins};
use crate::converter::type_obj::TypeObject;
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

    pub fn get_type<'a>(&self, builtins: BuiltinRef<'a>) -> Cow<'a, TypeObject> {
        builtins.constant_no(self.value).unwrap().get_type(builtins)
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

#[cfg(test)]
mod tests {
    use crate::converter::constant::{BuiltinConstant, ConstantBytes};

    #[test]
    fn builtin_bytes() {
        const BUILTIN_BYTE: u8 = ConstantBytes::Builtin as u8;
        assert_eq!(
            BuiltinConstant::new(3).to_bytes(),
            vec![BUILTIN_BYTE, 0, 0, 0, 3]
        );
    }
}
