use crate::converter::constant::LangConstant;
use crate::converter::file_writer::ConstantSet;
use crate::util::usize_to_short_bytes;

use super::{BytecodeFmt, BytecodeType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantBytecode {
    value: LangConstant,
}

impl ConstantBytecode {
    pub const fn new(value: LangConstant) -> Self {
        Self { value }
    }

    pub const fn get_value(&self) -> &LangConstant {
        &self.value
    }
}

impl BytecodeType for ConstantBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        info: BytecodeFmt<'_>,
    ) -> std::fmt::Result {
        let index = info.constants.get_index_of(&self.value).unwrap();
        write!(f, "{} ({})", index, self.value.display(info.builtins))
    }

    fn assemble(&self, buffer: &mut Vec<u8>, constants: &ConstantSet) {
        let index = constants.get_index_of(&self.value).unwrap();
        buffer.extend(usize_to_short_bytes(index))
    }
}

impl<T> From<T> for ConstantBytecode
where
    T: Into<LangConstant>,
{
    fn from(x: T) -> Self {
        Self::new(x.into())
    }
}

#[cfg(test)]
mod tests {
    use indexmap::{indexset, IndexSet};

    use crate::converter::bytecode::{BytecodeType, ConstantBytecode};
    use crate::converter::constant::LangConstant;

    #[test]
    fn assemble_constant() {
        let constants: IndexSet<LangConstant> =
            indexset! {true.into(), 3.into(), 4.into(), 1.into(), false.into()};
        let constant_set = constants.clone().into();
        for (i, constant) in constants.iter().enumerate() {
            let mut buf = Vec::new();
            ConstantBytecode::new(constant.clone()).assemble(&mut buf, &constant_set);
            assert_eq!(buf.len(), ConstantBytecode::SIZE);
            assert_eq!(buf, &(i as u16).to_be_bytes());
        }
    }
}
