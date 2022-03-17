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

    pub fn str_value(&self) -> String {
        self.value.to_string()
    }

    pub fn repr_value(&self) -> String {
        self.value.to_string()
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

#[cfg(test)]
mod tests {
    use crate::converter::constant::{BoolConstant, ConstantBytes};

    #[test]
    fn bool_bytes() {
        assert_eq!(
            BoolConstant::new(false).to_bytes(),
            vec![ConstantBytes::Bool as u8, 0]
        );
        assert_eq!(
            BoolConstant::new(true).to_bytes(),
            vec![ConstantBytes::Bool as u8, 1]
        );
    }

    #[test]
    fn bool_str() {
        assert_eq!(BoolConstant::new(false).str_value(), "false");
        assert_eq!(BoolConstant::new(true).str_value(), "true");
    }

    #[test]
    fn bool_repr() {
        assert_eq!(BoolConstant::new(false).repr_value(), "false");
        assert_eq!(BoolConstant::new(true).repr_value(), "true");
    }

    #[test]
    fn display() {
        assert_eq!(format!("{}", BoolConstant::new(false)), "false");
        assert_eq!(format!("{}", BoolConstant::new(true)), "true");
    }

    #[test]
    fn from_bool() {
        assert_eq!(BoolConstant::new(false), false.into());
        assert_eq!(BoolConstant::new(true), true.into());
    }
}
