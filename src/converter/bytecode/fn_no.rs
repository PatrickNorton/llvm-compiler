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

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;

    use crate::converter::bytecode::{BytecodeType, FunctionNoBytecode};

    #[test]
    fn assemble_fn_no() {
        let values = &[
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000, 9999, 10000, 12345, 65535,
        ];
        for &value in values {
            let mut buf = Vec::new();
            FunctionNoBytecode::new(value).assemble(&mut buf, &IndexSet::new());
            assert_eq!(buf.len(), FunctionNoBytecode::SIZE);
            assert_eq!(buf, &value.to_be_bytes());
        }
    }
}