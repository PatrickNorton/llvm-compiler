use std::fmt::Display;
use std::sync::Arc;

use crate::converter::builtins::Builtins;
use crate::converter::type_obj::TypeObject;
use crate::util::U32_BYTES;

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionConstant {
    name: Arc<str>,
    index: u16,
}

impl FunctionConstant {
    pub fn new(name: Arc<str>, index: u16) -> Self {
        Self { name, index }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.callable()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Function as u8);
        bytes.extend((self.index as u32).to_be_bytes());
        bytes
    }
}

impl Display for FunctionConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl From<FunctionConstant> for LangConstant {
    fn from(x: FunctionConstant) -> Self {
        LangConstant::Func(x)
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::constant::{ConstantBytes, FunctionConstant};

    #[test]
    fn function_bytes() {
        const FUNCTION_BYTE: u8 = ConstantBytes::Function as u8;
        assert_eq!(
            FunctionConstant::new("".into(), 0).to_bytes(),
            vec![FUNCTION_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            FunctionConstant::new("".into(), 1000).to_bytes(),
            vec![FUNCTION_BYTE, 0, 0, 0x3, 0xe8]
        );
    }
}
