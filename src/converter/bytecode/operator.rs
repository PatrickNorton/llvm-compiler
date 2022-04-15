use crate::converter::file_writer::ConstantSet;
use crate::converter::function::Function;
use crate::parser::operator_sp::OpSpTypeNode;

use super::BytecodeType;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct OperatorBytecode {
    value: OpSpTypeNode,
}

impl OperatorBytecode {
    pub const fn new(value: OpSpTypeNode) -> Self {
        Self { value }
    }
}

impl BytecodeType for OperatorBytecode {
    const SIZE: usize = 2;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _functions: &[&Function],
    ) -> std::fmt::Result {
        write!(f, "{} ({})", self.value as u16, self.value)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &ConstantSet) {
        buffer.extend(&(self.value as u16).to_be_bytes())
    }
}

impl From<OpSpTypeNode> for OperatorBytecode {
    fn from(x: OpSpTypeNode) -> Self {
        OperatorBytecode::new(x)
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;

    use crate::converter::bytecode::BytecodeType;
    use crate::parser::operator_sp;

    use super::OperatorBytecode;

    #[test]
    fn assemble_op() {
        for (i, op) in operator_sp::VALUES.into_iter().enumerate() {
            let mut buf = Vec::new();
            OperatorBytecode::new(op).assemble(&mut buf, &IndexSet::new().into());
            assert_eq!(buf.len(), OperatorBytecode::SIZE);
            assert_eq!(buf, &(i as u16).to_be_bytes());
        }
    }
}
