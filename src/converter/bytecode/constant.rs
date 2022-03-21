use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;
use crate::util::usize_to_bytes;

use super::BytecodeType;

#[derive(Debug, Clone)]
pub struct ConstantBytecode {
    value: LangConstant,
}

impl ConstantBytecode {
    pub const fn new(value: LangConstant) -> Self {
        Self { value }
    }
}

impl BytecodeType for ConstantBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        _f: &mut std::fmt::Formatter<'_>,
        _functions: &[&Function],
    ) -> std::fmt::Result {
        todo!("Needs set of constants")
    }

    fn assemble(&self, buffer: &mut Vec<u8>, constants: &IndexSet<LangConstant>) {
        let index = constants.get_index_of(&self.value).unwrap();
        buffer.extend(&usize_to_bytes(index))
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
