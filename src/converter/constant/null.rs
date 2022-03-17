use std::fmt::Display;

use derive_new::new;

use crate::converter::constant::ConstantBytes;

use super::LangConstant;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, new)]
pub struct NullConstant {}

impl NullConstant {
    pub fn to_bytes(self) -> Vec<u8> {
        vec![ConstantBytes::Null as u8]
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
}
