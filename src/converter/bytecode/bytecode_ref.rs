use super::{
    ArgcBytecode, Bytecode, ConstantBytecode, FunctionNoBytecode, LocationBytecode,
    OperatorBytecode, StackPosBytecode, SyscallBytecode, TableNoBytecode, VariableBytecode,
    VariantBytecode,
};

#[derive(Debug)]
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

impl Bytecode {
    pub fn get_operands(&self) -> Vec<BytecodeRef<'_>> {
        match self {
            Bytecode::Nop() => Vec::new(),
            Bytecode::LoadNull() => Vec::new(),
            Bytecode::LoadConst(x) => vec![x.into()],
            Bytecode::LoadValue(x) => vec![x.into()],
            Bytecode::LoadDot(x) => vec![x.into()],
            Bytecode::LoadSubscript(x) => vec![x.into()],
            Bytecode::LoadOp(x) => vec![x.into()],
            Bytecode::PopTop() => Vec::new(),
            Bytecode::DupTop() => Vec::new(),
            Bytecode::Swap2() => Vec::new(),
            Bytecode::Swap3() => Vec::new(),
            Bytecode::SwapN(x) => vec![x.into()],
            Bytecode::Store(x) => vec![x.into()],
            Bytecode::StoreSubscript(x) => vec![x.into()],
            Bytecode::StoreAttr(x) => vec![x.into()],
            Bytecode::SwapStack(x, y) => vec![x.into(), y.into()],
            Bytecode::Plus() => Vec::new(),
            Bytecode::Minus() => Vec::new(),
            Bytecode::Times() => Vec::new(),
            Bytecode::Divide() => Vec::new(),
            Bytecode::FloorDiv() => Vec::new(),
            Bytecode::Mod() => Vec::new(),
            Bytecode::Subscript() => Vec::new(),
            Bytecode::Power() => Vec::new(),
            Bytecode::LBitshift() => Vec::new(),
            Bytecode::RBitshift() => Vec::new(),
            Bytecode::BitwiseAnd() => Vec::new(),
            Bytecode::BitwiseOr() => Vec::new(),
            Bytecode::BitwiseXor() => Vec::new(),
            Bytecode::Compare() => Vec::new(),
            Bytecode::DelSubscript() => Vec::new(),
            Bytecode::UMinus() => Vec::new(),
            Bytecode::BitwiseNot() => Vec::new(),
            Bytecode::BoolAnd() => Vec::new(),
            Bytecode::BoolOr() => Vec::new(),
            Bytecode::BoolNot() => Vec::new(),
            Bytecode::BoolXor() => Vec::new(),
            Bytecode::Identical() => Vec::new(),
            Bytecode::Instanceof() => Vec::new(),
            Bytecode::CallOp(x, y) => vec![x.into(), y.into()],
            Bytecode::PackTuple(x) => vec![x.into()],
            Bytecode::UnpackTuple() => Vec::new(),
            Bytecode::Equal() => Vec::new(),
            Bytecode::LessThan() => Vec::new(),
            Bytecode::GreaterThan() => Vec::new(),
            Bytecode::LessEqual() => Vec::new(),
            Bytecode::GreaterEqual() => Vec::new(),
            Bytecode::Contains() => Vec::new(),
            Bytecode::Jump(x) => vec![x.into()],
            Bytecode::JumpFalse(x) => vec![x.into()],
            Bytecode::JumpTrue(x) => vec![x.into()],
            Bytecode::JumpNN(x) => vec![x.into()],
            Bytecode::JumpNull(x) => vec![x.into()],
            Bytecode::CallMethod(x, y) => vec![x.into(), y.into()],
            Bytecode::CallTos(x) => vec![x.into()],
            Bytecode::CallFn(x, y) => vec![x.into(), y.into()],
            Bytecode::TailMethod(x) => vec![x.into()],
            Bytecode::TailTos(x) => vec![x.into()],
            Bytecode::TailFn(x, y) => vec![x.into(), y.into()],
            Bytecode::Return(x) => vec![x.into()],
            Bytecode::Yield(x) => vec![x.into()],
            Bytecode::SwitchTable(x) => vec![x.into()],
            Bytecode::Throw() => Vec::new(),
            Bytecode::ThrowQuick(x) => vec![x.into()],
            Bytecode::EnterTry(x) => vec![x.into()],
            Bytecode::ExceptN(x) => vec![x.into()],
            Bytecode::Finally() => Vec::new(),
            Bytecode::EndTry(x) => vec![x.into()],
            Bytecode::FuncDef() => Vec::new(),
            Bytecode::ClassDef() => Vec::new(),
            Bytecode::EndClass() => Vec::new(),
            Bytecode::ForIter(x, y) => vec![x.into(), y.into()],
            Bytecode::ListCreate(x) => vec![x.into()],
            Bytecode::SetCreate(x) => vec![x.into()],
            Bytecode::DictCreate(x) => vec![x.into()],
            Bytecode::ListAdd() => Vec::new(),
            Bytecode::SetAdd() => Vec::new(),
            Bytecode::DictAdd() => Vec::new(),
            Bytecode::Dotimes(x) => vec![x.into()],
            Bytecode::ForParallel(x, y) => vec![x.into(), y.into()],
            Bytecode::MakeSlice() => Vec::new(),
            Bytecode::ListDyn() => Vec::new(),
            Bytecode::SetDyn() => Vec::new(),
            Bytecode::DictDyn() => Vec::new(),
            Bytecode::ListCap() => Vec::new(),
            Bytecode::SetCap() => Vec::new(),
            Bytecode::DictCap() => Vec::new(),
            Bytecode::DoStatic(x) => vec![x.into()],
            Bytecode::StoreStatic(x) => vec![x.into()],
            Bytecode::LoadStatic(x) => vec![x.into()],
            Bytecode::GetVariant(x) => vec![x.into()],
            Bytecode::MakeVariant(x) => vec![x.into()],
            Bytecode::VariantNo() => Vec::new(),
            Bytecode::MakeOption() => Vec::new(),
            Bytecode::IsSome() => Vec::new(),
            Bytecode::UnwrapOption() => Vec::new(),
            Bytecode::MakeFunction(x) => vec![x.into()],
            Bytecode::GetType() => Vec::new(),
            Bytecode::GetSys(x) => vec![x.into()],
            Bytecode::Syscall(x, y) => vec![x.into(), y.into()],
            Bytecode::DupTop2() => Vec::new(),
            Bytecode::DupTopN(x) => vec![x.into()],
            Bytecode::UnpackIterable() => Vec::new(),
            Bytecode::PackIterable() => Vec::new(),
            Bytecode::SwapDyn() => Vec::new(),
        }
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
