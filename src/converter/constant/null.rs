use std::fmt::Display;

use derive_new::new;

use crate::converter::builtins::Builtins;
use crate::converter::constant::ConstantBytes;
use crate::converter::type_obj::TypeObject;

use super::LangConstant;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, new)]
pub struct NullConstant {}

impl NullConstant {
    pub fn to_bytes(self) -> Vec<u8> {
        vec![ConstantBytes::Null as u8]
    }

    pub fn str_value(&self) -> String {
        "null".to_string()
    }

    pub fn repr_value(&self) -> String {
        "null".to_string()
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.null_type()
    }
}

impl Display for NullConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "null".fmt(f)
    }
}

impl From<NullConstant> for LangConstant {
    fn from(x: NullConstant) -> Self {
        LangConstant::Null(x)
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::constant::{ConstantBytes, NullConstant};

    #[test]
    fn null_bytes() {
        assert_eq!(
            NullConstant::new().to_bytes(),
            vec![ConstantBytes::Null as u8]
        )
    }

    #[test]
    fn null_str() {
        assert_eq!(NullConstant::new().str_value(), "null");
    }

    #[test]
    fn null_repr() {
        assert_eq!(NullConstant::new().repr_value(), "null");
    }
}
