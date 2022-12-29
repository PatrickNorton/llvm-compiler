mod argc;
mod bytecode_ref;
mod constant;
mod fn_no;
mod location;
mod operator;
mod stack_pos;
mod syscall;
mod table_no;
mod variable;
mod variant;

use std::fmt::Display;

use derive_new::new;

pub use self::argc::ArgcBytecode;
pub use self::bytecode_ref::BytecodeRef;
pub use self::constant::ConstantBytecode;
pub use self::fn_no::FunctionNoBytecode;
pub use self::location::{Label, LocationBytecode};
pub use self::operator::OperatorBytecode;
pub use self::stack_pos::StackPosBytecode;
pub use self::syscall::SyscallBytecode;
pub use self::table_no::TableNoBytecode;
pub use self::variable::VariableBytecode;
pub use self::variant::VariantBytecode;

use super::builtins::Builtins;
use super::constant::LangConstant;
use super::file_writer::ConstantSet;
use super::function::Function;

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Bytecode {
    Nop() = 0x0,
    LoadNull() = 0x1,
    LoadConst(ConstantBytecode) = 0x2,
    LoadValue(VariableBytecode) = 0x3,
    LoadDot(ConstantBytecode) = 0x4,
    LoadSubscript(ArgcBytecode) = 0x5,
    LoadOp(OperatorBytecode) = 0x6,
    PopTop() = 0x7,
    DupTop() = 0x8,
    Swap2() = 0x9,
    Swap3() = 0xA,
    SwapN(StackPosBytecode) = 0xB,
    Store(VariableBytecode) = 0xC,
    StoreSubscript(ArgcBytecode) = 0xD,
    StoreAttr(ConstantBytecode) = 0xE,
    SwapStack(StackPosBytecode, StackPosBytecode) = 0xF,
    // Binary operators
    Plus() = 0x10,
    Minus() = 0x11,
    Times() = 0x12,
    Divide() = 0x13,
    FloorDiv() = 0x14,
    Mod() = 0x15,
    Subscript() = 0x16,
    Power() = 0x17,
    LBitshift() = 0x18,
    RBitshift() = 0x19,
    BitwiseAnd() = 0x1A,
    BitwiseOr() = 0x1B,
    BitwiseXor() = 0x1C,
    Compare() = 0x1D,
    DelSubscript() = 0x1E,
    UMinus() = 0x1F,
    BitwiseNot() = 0x20,
    BoolAnd() = 0x21,
    BoolOr() = 0x22,
    BoolNot() = 0x23,
    BoolXor() = 0x24,
    Identical() = 0x25,
    Instanceof() = 0x26,
    CallOp(OperatorBytecode, ArgcBytecode) = 0x27,
    PackTuple(ArgcBytecode) = 0x28,
    UnpackTuple() = 0x29,
    Equal() = 0x2A,
    LessThan() = 0x2B,
    GreaterThan() = 0x2C,
    LessEqual() = 0x2D,
    GreaterEqual() = 0x2E,
    Contains() = 0x2F,
    // Jumps, etc.
    Jump(LocationBytecode) = 0x30,
    JumpFalse(LocationBytecode) = 0x31,
    JumpTrue(LocationBytecode) = 0x32,
    JumpNN(LocationBytecode) = 0x33,
    JumpNull(LocationBytecode) = 0x34,
    CallMethod(ConstantBytecode, ArgcBytecode) = 0x35,
    CallTos(ArgcBytecode) = 0x36,
    CallFn(FunctionNoBytecode, ArgcBytecode) = 0x37,
    TailMethod(ArgcBytecode) = 0x38,
    TailTos(ArgcBytecode) = 0x39,
    TailFn(FunctionNoBytecode, ArgcBytecode) = 0x3A,
    Return(ArgcBytecode) = 0x3B,
    Yield(ArgcBytecode) = 0x3C,
    SwitchTable(TableNoBytecode) = 0x3D,
    // Exception stuff
    Throw() = 0x40,
    ThrowQuick(ArgcBytecode) = 0x41,
    EnterTry(LocationBytecode) = 0x42,
    ExceptN(ArgcBytecode) = 0x43,
    Finally() = 0x44,
    EndTry(ArgcBytecode) = 0x45,
    // Markers
    FuncDef() = 0x48,
    ClassDef() = 0x49,
    EndClass() = 0x4A,
    // Loop stuff
    ForIter(LocationBytecode, ArgcBytecode) = 0x50,
    ListCreate(ArgcBytecode) = 0x51,
    SetCreate(ArgcBytecode) = 0x52,
    DictCreate(ArgcBytecode) = 0x53,
    ListAdd() = 0x54,
    SetAdd() = 0x55,
    DictAdd() = 0x56,
    Dotimes(LocationBytecode) = 0x57,
    ForParallel(LocationBytecode, ArgcBytecode) = 0x58,
    MakeSlice() = 0x59,
    ListDyn() = 0x5A,
    SetDyn() = 0x5B,
    DictDyn() = 0x5C,
    ListCap() = 0x5D,
    SetCap() = 0x5E,
    DictCap() = 0x5F,
    // Statics
    DoStatic(LocationBytecode) = 0x60,
    StoreStatic(VariableBytecode) = 0x61,
    LoadStatic(VariableBytecode) = 0x62,
    // Union/Option stuff
    GetVariant(VariantBytecode) = 0x68,
    MakeVariant(VariantBytecode) = 0x69,
    VariantNo() = 0x6A,
    MakeOption() = 0x6B,
    IsSome() = 0x6C,
    UnwrapOption() = 0x6D,
    // Misc.
    MakeFunction(FunctionNoBytecode) = 0x70,
    GetType() = 0x71,
    GetSys(SyscallBytecode) = 0x72,
    Syscall(SyscallBytecode, ArgcBytecode) = 0x73,
    // Dups, part 2 (maybe realign?)
    DupTop2() = 0x78,
    DupTopN(ArgcBytecode) = 0x79,
    UnpackIterable() = 0x7A,
    PackIterable() = 0x7B,
    SwapDyn() = 0x7C,
}

trait BytecodeType {
    const SIZE: usize;

    fn write_str(&self, f: &mut std::fmt::Formatter<'_>, info: BytecodeFmt<'_>)
        -> std::fmt::Result;

    fn assemble(&self, buffer: &mut Vec<u8>, constants: &ConstantSet);

    fn size(&self) -> usize
    where
        Self: Sized,
    {
        <Self as BytecodeType>::SIZE
    }
}

macro_rules! sum_sizes {
    () => {0};
    ($val:ident $(,)?) => {$val.size()};
    ($val:ident, $($vals:ident),* $(,)?) => {
        $val.size() + sum_sizes!($($vals),*)
    };
}

macro_rules! bytecode_size {
    ($($val:ident),* $(,)?) => {
        sum_sizes!($($val),*) + 1
    };
}

// NOTE: feature(macro_metavar_expressions) (#83527) would improve this
macro_rules! count {
    () => {
        0
    };
    ($a:ident $(,)?) => {{
        let _ = $a;
        1
    }};
    ($a:ident, $b:ident $(,)?) => {{
        let _ = ($a, $b);
        2
    }};
}

macro_rules! dispatch_assemble {
    ($self:ident, $constants:ident) => {
        $self.assemble_0()
    };
    ($a:ident, $self:ident, $constants:ident) => {
        $self.assemble_1($a, $constants)
    };
    ($a:ident, $b:ident, $self:ident, $constants:ident) => {
        $self.assemble_2($a, $b, $constants)
    };
}

/// Implements the given macro on each [`Bytecode`] variant.
///
/// The given macro should be callable with 0, 1, or 2 comma-separated
/// arguments with the simple variant. If additional arguments are given,
/// those will be appended to the macro call.
///
/// # Examples
/// ```ignore
/// bytecode_match!(foo => macro bar)
/// // expands to
/// match foo {
///     Bytecode::Nop() => bar!(),
///     Bytecode::LoadNull() => bar!(),
///     Bytecode::LoadConst(x) => bar!(x),
///     // etc.
/// }
///
/// bytecode_match!(foo => macro bar(baz))
/// // expands to
/// match foo {
///     Bytecode::Nop() => bar!(baz),
///     Bytecode::LoadNull() => bar!(baz),
///     Bytecode::LoadConst(x) => bar!(x, baz),
///     // etc.
/// }
/// ```
macro_rules! bytecode_match {
    ($val:expr => macro $name:ident) => {
        bytecode_match!($val => macro $name())
    };

    ($val:expr => macro $name:ident ($($vals:ident),*)) => {
        match $val {
            Bytecode::Nop() => $name!($($vals),*),
            Bytecode::LoadNull() => $name!($($vals),*),
            Bytecode::LoadConst(x) => $name!(x, $($vals),*),
            Bytecode::LoadValue(x) => $name!(x, $($vals),*),
            Bytecode::LoadDot(x) => $name!(x, $($vals),*),
            Bytecode::LoadSubscript(x) => $name!(x, $($vals),*),
            Bytecode::LoadOp(x) => $name!(x, $($vals),*),
            Bytecode::PopTop() => $name!($($vals),*),
            Bytecode::DupTop() => $name!($($vals),*),
            Bytecode::Swap2() => $name!($($vals),*),
            Bytecode::Swap3() => $name!($($vals),*),
            Bytecode::SwapN(x) => $name!(x, $($vals),*),
            Bytecode::Store(x) => $name!(x, $($vals),*),
            Bytecode::StoreSubscript(x) => $name!(x, $($vals),*),
            Bytecode::StoreAttr(x) => $name!(x, $($vals),*),
            Bytecode::SwapStack(x, y) => $name!(x, y, $($vals),*),
            Bytecode::Plus() => $name!($($vals),*),
            Bytecode::Minus() => $name!($($vals),*),
            Bytecode::Times() => $name!($($vals),*),
            Bytecode::Divide() => $name!($($vals),*),
            Bytecode::FloorDiv() => $name!($($vals),*),
            Bytecode::Mod() => $name!($($vals),*),
            Bytecode::Subscript() => $name!($($vals),*),
            Bytecode::Power() => $name!($($vals),*),
            Bytecode::LBitshift() => $name!($($vals),*),
            Bytecode::RBitshift() => $name!($($vals),*),
            Bytecode::BitwiseAnd() => $name!($($vals),*),
            Bytecode::BitwiseOr() => $name!($($vals),*),
            Bytecode::BitwiseXor() => $name!($($vals),*),
            Bytecode::Compare() => $name!($($vals),*),
            Bytecode::DelSubscript() => $name!($($vals),*),
            Bytecode::UMinus() => $name!($($vals),*),
            Bytecode::BitwiseNot() => $name!($($vals),*),
            Bytecode::BoolAnd() => $name!($($vals),*),
            Bytecode::BoolOr() => $name!($($vals),*),
            Bytecode::BoolNot() => $name!($($vals),*),
            Bytecode::BoolXor() => $name!($($vals),*),
            Bytecode::Identical() => $name!($($vals),*),
            Bytecode::Instanceof() => $name!($($vals),*),
            Bytecode::CallOp(x, y) => $name!(x, y, $($vals),*),
            Bytecode::PackTuple(x) => $name!(x, $($vals),*),
            Bytecode::UnpackTuple() => $name!($($vals),*),
            Bytecode::Equal() => $name!($($vals),*),
            Bytecode::LessThan() => $name!($($vals),*),
            Bytecode::GreaterThan() => $name!($($vals),*),
            Bytecode::LessEqual() => $name!($($vals),*),
            Bytecode::GreaterEqual() => $name!($($vals),*),
            Bytecode::Contains() => $name!($($vals),*),
            Bytecode::Jump(x) => $name!(x, $($vals),*),
            Bytecode::JumpFalse(x) => $name!(x, $($vals),*),
            Bytecode::JumpTrue(x) => $name!(x, $($vals),*),
            Bytecode::JumpNN(x) => $name!(x, $($vals),*),
            Bytecode::JumpNull(x) => $name!(x, $($vals),*),
            Bytecode::CallMethod(x, y) => $name!(x, y, $($vals),*),
            Bytecode::CallTos(x) => $name!(x, $($vals),*),
            Bytecode::CallFn(x, y) => $name!(x, y, $($vals),*),
            Bytecode::TailMethod(x) => $name!(x, $($vals),*),
            Bytecode::TailTos(x) => $name!(x, $($vals),*),
            Bytecode::TailFn(x, y) => $name!(x, y, $($vals),*),
            Bytecode::Return(x) => $name!(x, $($vals),*),
            Bytecode::Yield(x) => $name!(x, $($vals),*),
            Bytecode::SwitchTable(x) => $name!(x, $($vals),*),
            Bytecode::Throw() => $name!($($vals),*),
            Bytecode::ThrowQuick(x) => $name!(x, $($vals),*),
            Bytecode::EnterTry(x) => $name!(x, $($vals),*),
            Bytecode::ExceptN(x) => $name!(x, $($vals),*),
            Bytecode::Finally() => $name!($($vals),*),
            Bytecode::EndTry(x) => $name!(x, $($vals),*),
            Bytecode::FuncDef() => $name!($($vals),*),
            Bytecode::ClassDef() => $name!($($vals),*),
            Bytecode::EndClass() => $name!($($vals),*),
            Bytecode::ForIter(x, y) => $name!(x, y, $($vals),*),
            Bytecode::ListCreate(x) => $name!(x, $($vals),*),
            Bytecode::SetCreate(x) => $name!(x, $($vals),*),
            Bytecode::DictCreate(x) => $name!(x, $($vals),*),
            Bytecode::ListAdd() => $name!($($vals),*),
            Bytecode::SetAdd() => $name!($($vals),*),
            Bytecode::DictAdd() => $name!($($vals),*),
            Bytecode::Dotimes(x) => $name!(x, $($vals),*),
            Bytecode::ForParallel(x, y) => $name!(x, y, $($vals),*),
            Bytecode::MakeSlice() => $name!($($vals),*),
            Bytecode::ListDyn() => $name!($($vals),*),
            Bytecode::SetDyn() => $name!($($vals),*),
            Bytecode::DictDyn() => $name!($($vals),*),
            Bytecode::ListCap() => $name!($($vals),*),
            Bytecode::SetCap() => $name!($($vals),*),
            Bytecode::DictCap() => $name!($($vals),*),
            Bytecode::DoStatic(x) => $name!(x, $($vals),*),
            Bytecode::StoreStatic(x) => $name!(x, $($vals),*),
            Bytecode::LoadStatic(x) => $name!(x, $($vals),*),
            Bytecode::GetVariant(x) => $name!(x, $($vals),*),
            Bytecode::MakeVariant(x) => $name!(x, $($vals),*),
            Bytecode::VariantNo() => $name!($($vals),*),
            Bytecode::MakeOption() => $name!($($vals),*),
            Bytecode::IsSome() => $name!($($vals),*),
            Bytecode::UnwrapOption() => $name!($($vals),*),
            Bytecode::MakeFunction(x) => $name!(x, $($vals),*),
            Bytecode::GetType() => $name!($($vals),*),
            Bytecode::GetSys(x) => $name!(x, $($vals),*),
            Bytecode::Syscall(x, y) => $name!(x, y, $($vals),*),
            Bytecode::DupTop2() => $name!($($vals),*),
            Bytecode::DupTopN(x) => $name!(x, $($vals),*),
            Bytecode::UnpackIterable() => $name!($($vals),*),
            Bytecode::PackIterable() => $name!($($vals),*),
            Bytecode::SwapDyn() => $name!($($vals),*),
        }
    };
}

pub(self) use bytecode_match;

impl Bytecode {
    pub fn jump_if(value: bool, label: Label) -> Bytecode {
        if value {
            Bytecode::JumpTrue(label.into())
        } else {
            Bytecode::JumpFalse(label.into())
        }
    }

    pub fn size(&self) -> usize {
        bytecode_match!(self => macro bytecode_size)
    }

    pub fn assemble(&self, constants: &ConstantSet) -> Vec<u8> {
        bytecode_match!(self => macro dispatch_assemble (self, constants))
    }

    pub const fn get_constant(&self) -> Option<&LangConstant> {
        match self {
            Bytecode::LoadConst(x) => Some(x.get_value()),
            Bytecode::LoadDot(x) => Some(x.get_value()),
            Bytecode::StoreAttr(x) => Some(x.get_value()),
            Bytecode::CallMethod(x, _) => Some(x.get_value()),
            _ => None,
        }
    }

    pub const fn byte_value(&self) -> u8 {
        // SAFETY: repr(u8) guarantees layout, so this is sound.
        // TODO: When a safe variant of this lands, switch to it instead
        unsafe { *(self as *const _ as *const u8) }
    }

    const fn bytecode_count(&self) -> usize {
        bytecode_match!(self => macro count)
    }

    fn assemble_0(&self) -> Vec<u8> {
        debug_assert_eq!(self.bytecode_count(), 0);
        vec![self.byte_value()]
    }

    fn assemble_1(&self, a: &impl BytecodeType, constants: &ConstantSet) -> Vec<u8> {
        debug_assert_eq!(self.bytecode_count(), 1);
        let mut bytes = Vec::with_capacity(self.size());
        bytes.push(self.byte_value());
        a.assemble(&mut bytes, constants);
        bytes
    }

    fn assemble_2(
        &self,
        a: &impl BytecodeType,
        b: &impl BytecodeType,
        constants: &ConstantSet,
    ) -> Vec<u8> {
        debug_assert_eq!(self.bytecode_count(), 2);
        let mut bytes = Vec::with_capacity(self.size());
        bytes.push(self.byte_value());
        a.assemble(&mut bytes, constants);
        b.assemble(&mut bytes, constants);
        bytes
    }
}

// Bytecode formatting impls

#[derive(Debug, Copy, Clone, new)]
pub(self) struct BytecodeFmt<'a> {
    pub functions: &'a [&'a Function],
    pub constants: &'a ConstantSet,
    pub builtins: &'a Builtins,
}

#[derive(Debug, new)]
struct BytecodeFormat<'a> {
    bytecode: &'a Bytecode,
    info: BytecodeFmt<'a>,
}

#[derive(Debug, new)]
struct BytecodeValFormat<'a, T> {
    bytecode: &'a T,
    info: BytecodeFmt<'a>,
}

impl<'a> Display for BytecodeFormat<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.bytecode.write_str(f, self.info)
    }
}

impl<'a, T: BytecodeType> Display for BytecodeValFormat<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.bytecode.write_str(f, self.info)
    }
}

fn write_single(
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
    a: &impl BytecodeType,
    info: BytecodeFmt<'_>,
) -> std::fmt::Result {
    write!(f, "{:<16}{}", name, BytecodeValFormat::new(a, info))
}

fn write_double(
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
    a: &impl BytecodeType,
    b: &impl BytecodeType,
    info: BytecodeFmt<'_>,
) -> std::fmt::Result {
    write!(
        f,
        "{:<16}{}, {}",
        name,
        BytecodeValFormat::new(a, info),
        BytecodeValFormat::new(b, info)
    )
}

impl Bytecode {
    pub fn display<'a>(
        &'a self,
        functions: &'a [&'a Function],
        constants: &'a ConstantSet,
        builtins: &'a Builtins,
    ) -> impl Display + 'a {
        let fmt = BytecodeFmt::new(functions, constants, builtins);
        BytecodeFormat::new(self, fmt)
    }

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        info: BytecodeFmt<'_>,
    ) -> std::fmt::Result {
        match self {
            Bytecode::Nop() => write!(f, "NOP"),
            Bytecode::LoadNull() => write!(f, "LOAD_NULL"),
            Bytecode::LoadConst(a) => write_single(f, "LOAD_CONST", a, info),
            Bytecode::LoadValue(a) => write_single(f, "LOAD_VALUE", a, info),
            Bytecode::LoadDot(a) => write_single(f, "LOAD_DOT", a, info),
            Bytecode::LoadSubscript(a) => write_single(f, "LOAD_SUBSCRIPT", a, info),
            Bytecode::LoadOp(a) => write_single(f, "LOAD_OP", a, info),
            Bytecode::PopTop() => write!(f, "POP_TOP"),
            Bytecode::DupTop() => write!(f, "DUP_TOP"),
            Bytecode::Swap2() => write!(f, "SWAP_2"),
            Bytecode::Swap3() => write!(f, "SWAP_3"),
            Bytecode::SwapN(a) => write_single(f, "SWAP_N", a, info),
            Bytecode::Store(a) => write_single(f, "STORE", a, info),
            Bytecode::StoreSubscript(a) => write_single(f, "STORE_SUBSCRIPT", a, info),
            Bytecode::StoreAttr(a) => write_single(f, "STORE_ATTR", a, info),
            Bytecode::SwapStack(a, b) => write_double(f, "SWAP_STACK", a, b, info),
            Bytecode::Plus() => write!(f, "PLUS"),
            Bytecode::Minus() => write!(f, "MINUS"),
            Bytecode::Times() => write!(f, "TIMES"),
            Bytecode::Divide() => write!(f, "DIVIDE"),
            Bytecode::FloorDiv() => write!(f, "FLOOR_DIV"),
            Bytecode::Mod() => write!(f, "MOD"),
            Bytecode::Subscript() => write!(f, "SUBSCRIPT"),
            Bytecode::Power() => write!(f, "POWER"),
            Bytecode::LBitshift() => write!(f, "L_BITSHIFT"),
            Bytecode::RBitshift() => write!(f, "R_BITSHIFT"),
            Bytecode::BitwiseAnd() => write!(f, "BITWISE_AND"),
            Bytecode::BitwiseOr() => write!(f, "BITWISE_OR"),
            Bytecode::BitwiseXor() => write!(f, "BITWISE_XOR"),
            Bytecode::Compare() => write!(f, "COMPARE"),
            Bytecode::DelSubscript() => write!(f, "DEL_SUBSCRIPT"),
            Bytecode::UMinus() => write!(f, "U_MINUS"),
            Bytecode::BitwiseNot() => write!(f, "BITWISE_NOT"),
            Bytecode::BoolAnd() => write!(f, "BOOL_AND"),
            Bytecode::BoolOr() => write!(f, "BOOL_OR"),
            Bytecode::BoolNot() => write!(f, "BOOL_NOT"),
            Bytecode::BoolXor() => write!(f, "BOOL_XOR"),
            Bytecode::Identical() => write!(f, "IDENTICAL"),
            Bytecode::Instanceof() => write!(f, "INSTANCEOF"),
            Bytecode::CallOp(a, b) => write_double(f, "CALL_OP", a, b, info),
            Bytecode::PackTuple(a) => write_single(f, "PACK_TUPLE", a, info),
            Bytecode::UnpackTuple() => write!(f, "UNPACK_TUPLE"),
            Bytecode::Equal() => write!(f, "EQUAL"),
            Bytecode::LessThan() => write!(f, "LESS_THAN"),
            Bytecode::GreaterThan() => write!(f, "GREATER_THAN"),
            Bytecode::LessEqual() => write!(f, "LESS_EQUAL"),
            Bytecode::GreaterEqual() => write!(f, "GREATER_EQUAL"),
            Bytecode::Contains() => write!(f, "CONTAINS"),
            Bytecode::Jump(a) => write_single(f, "JUMP", a, info),
            Bytecode::JumpFalse(a) => write_single(f, "JUMP_FALSE", a, info),
            Bytecode::JumpTrue(a) => write_single(f, "JUMP_TRUE", a, info),
            Bytecode::JumpNN(a) => write_single(f, "JUMP_NN", a, info),
            Bytecode::JumpNull(a) => write_single(f, "JUMP_NULL", a, info),
            Bytecode::CallMethod(a, b) => write_double(f, "CALL_METHOD", a, b, info),
            Bytecode::CallTos(a) => write_single(f, "CALL_TOS", a, info),
            Bytecode::CallFn(a, b) => write_double(f, "CALL_FN", a, b, info),
            Bytecode::TailMethod(a) => write_single(f, "TAIL_METHOD", a, info),
            Bytecode::TailTos(a) => write_single(f, "TAIL_TOS", a, info),
            Bytecode::TailFn(a, b) => write_double(f, "TAIL_FN", a, b, info),
            Bytecode::Return(a) => write_single(f, "RETURN", a, info),
            Bytecode::Yield(a) => write_single(f, "YIELD", a, info),
            Bytecode::SwitchTable(a) => write_single(f, "SWITCH_TABLE", a, info),
            Bytecode::Throw() => write!(f, "THROW"),
            Bytecode::ThrowQuick(a) => write_single(f, "THROW_QUICK", a, info),
            Bytecode::EnterTry(a) => write_single(f, "ENTER_TRY", a, info),
            Bytecode::ExceptN(a) => write_single(f, "EXCEPT_N", a, info),
            Bytecode::Finally() => write!(f, "FINALLY"),
            Bytecode::EndTry(a) => write_single(f, "END_TRY", a, info),
            Bytecode::FuncDef() => write!(f, "FUNC_DEF"),
            Bytecode::ClassDef() => write!(f, "CLASS_DEF"),
            Bytecode::EndClass() => write!(f, "END_CLASS"),
            Bytecode::ForIter(a, b) => write_double(f, "FOR_ITER", a, b, info),
            Bytecode::ListCreate(a) => write_single(f, "LIST_CREATE", a, info),
            Bytecode::SetCreate(a) => write_single(f, "SET_CREATE", a, info),
            Bytecode::DictCreate(a) => write_single(f, "DICT_CREATE", a, info),
            Bytecode::ListAdd() => write!(f, "LIST_ADD"),
            Bytecode::SetAdd() => write!(f, "SET_ADD"),
            Bytecode::DictAdd() => write!(f, "DICT_ADD"),
            Bytecode::Dotimes(a) => write_single(f, "DOTIMES", a, info),
            Bytecode::ForParallel(a, b) => write_double(f, "FOR_PARALLEL", a, b, info),
            Bytecode::MakeSlice() => write!(f, "MAKE_SLICE"),
            Bytecode::ListDyn() => write!(f, "LIST_DYN"),
            Bytecode::SetDyn() => write!(f, "SET_DYN"),
            Bytecode::DictDyn() => write!(f, "LIST_DYN"),
            Bytecode::ListCap() => write!(f, "LIST_CAP"),
            Bytecode::SetCap() => write!(f, "SET_CAP"),
            Bytecode::DictCap() => write!(f, "DICT_CAP"),
            Bytecode::DoStatic(a) => write_single(f, "DO_STATIC", a, info),
            Bytecode::StoreStatic(a) => write_single(f, "STORE_STATIC", a, info),
            Bytecode::LoadStatic(a) => write_single(f, "LOAD_STATIC", a, info),
            Bytecode::GetVariant(a) => write_single(f, "GET_VARIANT", a, info),
            Bytecode::MakeVariant(a) => write_single(f, "MAKE_VARIANT", a, info),
            Bytecode::VariantNo() => write!(f, "VARIANT_NO"),
            Bytecode::MakeOption() => write!(f, "MAKE_OPTION"),
            Bytecode::IsSome() => write!(f, "IS_SOME"),
            Bytecode::UnwrapOption() => write!(f, "UNWRAP_OPTION"),
            Bytecode::MakeFunction(a) => write_single(f, "MAKE_FUNCTION", a, info),
            Bytecode::GetType() => write!(f, "GET_TYPE"),
            Bytecode::GetSys(a) => write_single(f, "GET_SYS", a, info),
            Bytecode::Syscall(a, b) => write_double(f, "SYSCALL", a, b, info),
            Bytecode::DupTop2() => write!(f, "DUP_TOP_2"),
            Bytecode::DupTopN(a) => write_single(f, "DUP_TOP_N", a, info),
            Bytecode::UnpackIterable() => write!(f, "UNPACK_ITERABLE"),
            Bytecode::PackIterable() => write!(f, "PACK_ITERABLE"),
            Bytecode::SwapDyn() => write!(f, "SWAP_DYN"),
        }
    }
}
