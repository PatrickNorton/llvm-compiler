use std::fmt::Display;

use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;

use super::BytecodeType;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackPosBytecode {
    position: u16,
}

impl StackPosBytecode {
    pub const fn new(position: u16) -> Self {
        Self { position }
    }
}

impl BytecodeType for StackPosBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _functions: &[&Function],
    ) -> std::fmt::Result {
        Display::fmt(&self.position, f)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &IndexSet<LangConstant>) {
        buffer.extend(&self.position.to_be_bytes())
    }
}

impl From<u16> for StackPosBytecode {
    fn from(x: u16) -> Self {
        Self::new(x)
    }
}
