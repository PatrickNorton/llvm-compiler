use crate::converter::file_writer::ConstantSet;
use crate::converter::syscalls::syscall_name;

use super::{BytecodeFmt, BytecodeType};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyscallBytecode {
    syscall: u16,
}

impl SyscallBytecode {
    pub const fn new(syscall: u16) -> Self {
        Self { syscall }
    }
}

impl BytecodeType for SyscallBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _info: BytecodeFmt<'_>,
    ) -> std::fmt::Result {
        write!(f, "{} ({})", self.syscall, syscall_name(self.syscall))
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &ConstantSet) {
        buffer.extend(self.syscall.to_be_bytes())
    }
}

impl From<u16> for SyscallBytecode {
    fn from(x: u16) -> Self {
        Self::new(x)
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;

    use crate::converter::bytecode::{BytecodeType, SyscallBytecode};

    #[test]
    fn assemble_syscall() {
        let values = &[
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000, 9999, 10000, 12345, 65535,
        ];
        for &value in values {
            let mut buf = Vec::new();
            SyscallBytecode::new(value).assemble(&mut buf, &IndexSet::new().into());
            assert_eq!(buf.len(), SyscallBytecode::SIZE);
            assert_eq!(buf, &value.to_be_bytes());
        }
    }
}
