use std::fmt::Display;

use crate::converter::file_writer::ConstantSet;

use super::{BytecodeFmt, BytecodeType};

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
        _info: BytecodeFmt<'_>,
    ) -> std::fmt::Result {
        Display::fmt(&self.position, f)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &ConstantSet) {
        buffer.extend(&self.position.to_be_bytes())
    }
}

impl From<u16> for StackPosBytecode {
    fn from(x: u16) -> Self {
        Self::new(x)
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;

    use crate::converter::bytecode::{BytecodeType, StackPosBytecode};

    #[test]
    fn assemble_stack_pos() {
        let values = &[
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000, 9999, 10000, 12345, 65535,
        ];
        for &value in values {
            let mut buf = Vec::new();
            StackPosBytecode::new(value).assemble(&mut buf, &IndexSet::new().into());
            assert_eq!(buf.len(), StackPosBytecode::SIZE);
            assert_eq!(buf, &value.to_be_bytes());
        }
    }
}
