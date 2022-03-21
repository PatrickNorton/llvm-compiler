use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;
use crate::converter::syscalls::syscall_name;

use super::BytecodeType;

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
        _functions: &[&Function],
    ) -> std::fmt::Result {
        write!(f, "{} ({})", self.syscall, syscall_name(self.syscall))
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &IndexSet<LangConstant>) {
        buffer.extend(&self.syscall.to_be_bytes())
    }
}

impl From<u16> for SyscallBytecode {
    fn from(x: u16) -> Self {
        Self::new(x)
    }
}
