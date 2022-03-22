use std::fmt::Display;

use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;

use super::BytecodeType;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TableNoBytecode {
    table: u16,
}

impl TableNoBytecode {
    pub const fn new(table: u16) -> Self {
        Self { table }
    }
}

impl BytecodeType for TableNoBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _functions: &[&Function],
    ) -> std::fmt::Result {
        Display::fmt(&self.table, f)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &IndexSet<LangConstant>) {
        buffer.extend(&self.table.to_be_bytes())
    }
}

impl From<u16> for TableNoBytecode {
    fn from(x: u16) -> Self {
        Self::new(x)
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;

    use crate::converter::bytecode::{BytecodeType, TableNoBytecode};

    #[test]
    fn assemble_variant() {
        let values = &[
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000, 9999, 10000, 12345, 65535,
        ];
        for &value in values {
            let mut buf = Vec::new();
            TableNoBytecode::new(value).assemble(&mut buf, &IndexSet::new());
            assert_eq!(buf.len(), TableNoBytecode::SIZE);
            assert_eq!(buf, &value.to_be_bytes());
        }
    }
}
