use super::{
    bytecode_match, ArgcBytecode, Bytecode, ConstantBytecode, FunctionNoBytecode, LocationBytecode,
    OperatorBytecode, StackPosBytecode, SyscallBytecode, TableNoBytecode, VariableBytecode,
    VariantBytecode,
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BytecodeRef<'a> {
    Argc(&'a ArgcBytecode),
    Constant(&'a ConstantBytecode),
    FnNo(&'a FunctionNoBytecode),
    Location(&'a LocationBytecode),
    Operator(&'a OperatorBytecode),
    StackPos(&'a StackPosBytecode),
    Syscall(&'a SyscallBytecode),
    TableNo(&'a TableNoBytecode),
    Variable(&'a VariableBytecode),
    Variant(&'a VariantBytecode),
}

macro_rules! vec_into {
    () => {vec![]};

    ($($x:expr),+ $(,)?) => {
        vec![$($x.into()),+]
    };
}

impl Bytecode {
    /// The [operands](BytecodeRef) of the bytecode, as a [`Vec`].
    ///
    /// # Examples
    /// ```
    /// assert_eq!(Bytecode::LoadNull().get_operands(), Vec::new());
    /// assert_eq!(
    ///     Bytecode::LoadValue(0.into()).get_operands(),
    ///     vec![BytecodeRef::Variable(&0.into())]
    /// );
    /// assert_eq!(
    ///     Bytecode::SwapStack(0.into(), 1.into()),
    ///     vec![BytecodeRef::StackPos(&0.into()), BytecodeRef::StackPos(&1.into())]
    /// );
    /// ```
    pub fn get_operands(&self) -> Vec<BytecodeRef<'_>> {
        bytecode_match!(self => macro vec_into)
    }
}

macro_rules! from_bytecode {
    ($bytecode:ident -> $variant:ident) => {
        impl<'a> From<&'a $bytecode> for BytecodeRef<'a> {
            fn from(x: &'a $bytecode) -> Self {
                Self::$variant(x)
            }
        }
    };
}

from_bytecode!(ArgcBytecode -> Argc);
from_bytecode!(ConstantBytecode -> Constant);
from_bytecode!(FunctionNoBytecode -> FnNo);
from_bytecode!(LocationBytecode -> Location);
from_bytecode!(OperatorBytecode -> Operator);
from_bytecode!(StackPosBytecode -> StackPos);
from_bytecode!(SyscallBytecode -> Syscall);
from_bytecode!(TableNoBytecode -> TableNo);
from_bytecode!(VariableBytecode -> Variable);
from_bytecode!(VariantBytecode -> Variant);

#[cfg(test)]
mod tests {
    use crate::converter::bytecode::{Bytecode, BytecodeRef};

    #[test]
    fn bytecode_operands() {
        assert_eq!(Bytecode::LoadNull().get_operands(), Vec::new());
        assert_eq!(
            Bytecode::LoadValue(0.into()).get_operands(),
            vec![BytecodeRef::Variable(&0.into())]
        );
        assert_eq!(
            Bytecode::SwapStack(0.into(), 1.into()).get_operands(),
            vec![
                BytecodeRef::StackPos(&0.into()),
                BytecodeRef::StackPos(&1.into())
            ]
        );
    }
}
