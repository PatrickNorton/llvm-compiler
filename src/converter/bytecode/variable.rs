use std::fmt::Display;

use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;

use super::BytecodeType;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableBytecode {
    variable: u16,
}

impl VariableBytecode {
    pub const fn new(variable: u16) -> Self {
        Self { variable }
    }
}

impl BytecodeType for VariableBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _functions: &[&Function],
    ) -> std::fmt::Result {
        Display::fmt(&self.variable, f)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &IndexSet<LangConstant>) {
        buffer.extend(&self.variable.to_be_bytes())
    }
}

impl From<u16> for VariableBytecode {
    fn from(x: u16) -> Self {
        VariableBytecode::new(x)
    }
}
