use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;

use super::BytecodeType;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionNoBytecode {
    value: u16,
}

impl FunctionNoBytecode {
    pub const fn new(value: u16) -> Self {
        Self { value }
    }
}

impl BytecodeType for FunctionNoBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        functions: &[&Function],
    ) -> std::fmt::Result {
        write!(
            f,
            "{} ({})",
            self.value,
            functions[self.value as usize].get_name()
        )
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &IndexSet<LangConstant>) {
        buffer.extend(&self.value.to_be_bytes())
    }
}

impl From<u16> for FunctionNoBytecode {
    fn from(x: u16) -> Self {
        Self::new(x)
    }
}
