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
pub enum Bytecode {
    Nop(),
    LoadNull(),
    LoadConst(ConstantBytecode),
    LoadValue(VariableBytecode),
    LoadDot(ConstantBytecode),
    LoadSubscript(ArgcBytecode),
    LoadOp(OperatorBytecode),
    PopTop(),
    DupTop(),
    Swap2(),
    Swap3(),
    SwapN(StackPosBytecode),
    Store(VariableBytecode),
    StoreSubscript(ArgcBytecode),
    StoreAttr(ConstantBytecode),
    SwapStack(StackPosBytecode, StackPosBytecode),
    // Binary operators
    Plus(),
    Minus(),
    Times(),
    Divide(),
    FloorDiv(),
    Mod(),
    Subscript(),
    Power(),
    LBitshift(),
    RBitshift(),
    BitwiseAnd(),
    BitwiseOr(),
    BitwiseXor(),
    Compare(),
    DelSubscript(),
    UMinus(),
    BitwiseNot(),
    BoolAnd(),
    BoolOr(),
    BoolNot(),
    BoolXor(),
    Identical(),
    Instanceof(),
    CallOp(OperatorBytecode, ArgcBytecode),
    PackTuple(ArgcBytecode),
    UnpackTuple(),
    Equal(),
    LessThan(),
    GreaterThan(),
    LessEqual(),
    GreaterEqual(),
    Contains(),
    // Jumps, etc.
    Jump(LocationBytecode),
    JumpFalse(LocationBytecode),
    JumpTrue(LocationBytecode),
    JumpNN(LocationBytecode),
    JumpNull(LocationBytecode),
    CallMethod(ConstantBytecode, ArgcBytecode),
    CallTos(ArgcBytecode),
    CallFn(FunctionNoBytecode, ArgcBytecode),
    TailMethod(ArgcBytecode),
    TailTos(ArgcBytecode),
    TailFn(FunctionNoBytecode, ArgcBytecode),
    Return(ArgcBytecode),
    Yield(ArgcBytecode),
    SwitchTable(TableNoBytecode),
    // Exception stuff
    Throw(),
    ThrowQuick(ArgcBytecode),
    EnterTry(LocationBytecode),
    ExceptN(ArgcBytecode),
    Finally(),
    EndTry(ArgcBytecode),
    // Markers
    FuncDef(),
    ClassDef(),
    EndClass(),
    // Loop stuff
    ForIter(LocationBytecode, ArgcBytecode),
    ListCreate(ArgcBytecode),
    SetCreate(ArgcBytecode),
    DictCreate(ArgcBytecode),
    ListAdd(),
    SetAdd(),
    DictAdd(),
    Dotimes(LocationBytecode),
    ForParallel(LocationBytecode, ArgcBytecode),
    MakeSlice(),
    ListDyn(),
    SetDyn(),
    DictDyn(),
    ListCap(),
    SetCap(),
    DictCap(),
    // Statics
    DoStatic(LocationBytecode),
    StoreStatic(VariableBytecode),
    LoadStatic(VariableBytecode),
    // Union/Option stuff
    GetVariant(VariantBytecode),
    MakeVariant(VariantBytecode),
    VariantNo(),
    MakeOption(),
    IsSome(),
    UnwrapOption(),
    // Misc.
    MakeFunction(FunctionNoBytecode),
    GetType(),
    GetSys(SyscallBytecode),
    Syscall(SyscallBytecode, ArgcBytecode),
    // Dups, part 2 (maybe realign?)
    DupTop2(),
    DupTopN(ArgcBytecode),
    UnpackIterable(),
    PackIterable(),
    SwapDyn(),
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
// NOTE: ${ignore} is being stabilized in 1.62.0
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

    pub fn get_constant(&self) -> Option<&LangConstant> {
        match self {
            Bytecode::LoadConst(x) => Some(x.get_value()),
            Bytecode::LoadDot(x) => Some(x.get_value()),
            Bytecode::StoreAttr(x) => Some(x.get_value()),
            Bytecode::CallMethod(x, _) => Some(x.get_value()),
            _ => None,
        }
    }

    pub fn byte_value(&self) -> u8 {
        // TODO: If/when feature(arbitrary_enum_discriminant) (#60553) lands, we
        // can make these custom discriminants and do a cast of some sort
        match self {
            Bytecode::Nop() => 0x0,
            Bytecode::LoadNull() => 0x1,
            Bytecode::LoadConst(_) => 0x2,
            Bytecode::LoadValue(_) => 0x3,
            Bytecode::LoadDot(_) => 0x4,
            Bytecode::LoadSubscript(_) => 0x5,
            Bytecode::LoadOp(_) => 0x6,
            Bytecode::PopTop() => 0x7,
            Bytecode::DupTop() => 0x8,
            Bytecode::Swap2() => 0x9,
            Bytecode::Swap3() => 0xA,
            Bytecode::SwapN(_) => 0xB,
            Bytecode::Store(_) => 0xC,
            Bytecode::StoreSubscript(_) => 0xD,
            Bytecode::StoreAttr(_) => 0xE,
            Bytecode::SwapStack(_, _) => 0xF,
            // Binary operators
            Bytecode::Plus() => 0x10,
            Bytecode::Minus() => 0x11,
            Bytecode::Times() => 0x12,
            Bytecode::Divide() => 0x13,
            Bytecode::FloorDiv() => 0x14,
            Bytecode::Mod() => 0x15,
            Bytecode::Subscript() => 0x16,
            Bytecode::Power() => 0x17,
            Bytecode::LBitshift() => 0x18,
            Bytecode::RBitshift() => 0x19,
            Bytecode::BitwiseAnd() => 0x1A,
            Bytecode::BitwiseOr() => 0x1B,
            Bytecode::BitwiseXor() => 0x1C,
            Bytecode::Compare() => 0x1D,
            Bytecode::DelSubscript() => 0x1E,
            Bytecode::UMinus() => 0x1F,
            Bytecode::BitwiseNot() => 0x20,
            Bytecode::BoolAnd() => 0x21,
            Bytecode::BoolOr() => 0x22,
            Bytecode::BoolNot() => 0x23,
            Bytecode::BoolXor() => 0x24,
            Bytecode::Identical() => 0x25,
            Bytecode::Instanceof() => 0x26,
            Bytecode::CallOp(_, _) => 0x27,
            Bytecode::PackTuple(_) => 0x28,
            Bytecode::UnpackTuple() => 0x29,
            Bytecode::Equal() => 0x2A,
            Bytecode::LessThan() => 0x2B,
            Bytecode::GreaterThan() => 0x2C,
            Bytecode::LessEqual() => 0x2D,
            Bytecode::GreaterEqual() => 0x2E,
            Bytecode::Contains() => 0x2F,
            // Jumps, etc.
            Bytecode::Jump(_) => 0x30,
            Bytecode::JumpFalse(_) => 0x31,
            Bytecode::JumpTrue(_) => 0x32,
            Bytecode::JumpNN(_) => 0x33,
            Bytecode::JumpNull(_) => 0x34,
            Bytecode::CallMethod(_, _) => 0x35,
            Bytecode::CallTos(_) => 0x36,
            Bytecode::CallFn(_, _) => 0x37,
            Bytecode::TailMethod(_) => 0x38,
            Bytecode::TailTos(_) => 0x39,
            Bytecode::TailFn(_, _) => 0x3A,
            Bytecode::Return(_) => 0x3B,
            Bytecode::Yield(_) => 0x3C,
            Bytecode::SwitchTable(_) => 0x3D,
            // Exception stuff
            Bytecode::Throw() => 0x40,
            Bytecode::ThrowQuick(_) => 0x41,
            Bytecode::EnterTry(_) => 0x42,
            Bytecode::ExceptN(_) => 0x43,
            Bytecode::Finally() => 0x44,
            Bytecode::EndTry(_) => 0x45,
            // Markers
            Bytecode::FuncDef() => 0x48,
            Bytecode::ClassDef() => 0x49,
            Bytecode::EndClass() => 0x4A,
            // Loop stuff
            Bytecode::ForIter(_, _) => 0x50,
            Bytecode::ListCreate(_) => 0x51,
            Bytecode::SetCreate(_) => 0x52,
            Bytecode::DictCreate(_) => 0x53,
            Bytecode::ListAdd() => 0x54,
            Bytecode::SetAdd() => 0x55,
            Bytecode::DictAdd() => 0x56,
            Bytecode::Dotimes(_) => 0x57,
            Bytecode::ForParallel(_, _) => 0x58,
            Bytecode::MakeSlice() => 0x59,
            Bytecode::ListDyn() => 0x5A,
            Bytecode::SetDyn() => 0x5B,
            Bytecode::DictDyn() => 0x5C,
            Bytecode::ListCap() => 0x5D,
            Bytecode::SetCap() => 0x5E,
            Bytecode::DictCap() => 0x5F,
            // Statics
            Bytecode::DoStatic(_) => 0x60,
            Bytecode::StoreStatic(_) => 0x61,
            Bytecode::LoadStatic(_) => 0x62,
            // Union/Option stuff
            Bytecode::GetVariant(_) => 0x68,
            Bytecode::MakeVariant(_) => 0x69,
            Bytecode::VariantNo() => 0x6A,
            Bytecode::MakeOption() => 0x6B,
            Bytecode::IsSome() => 0x6C,
            Bytecode::UnwrapOption() => 0x6D,
            // Misc.
            Bytecode::MakeFunction(_) => 0x70,
            Bytecode::GetType() => 0x71,
            Bytecode::GetSys(_) => 0x72,
            Bytecode::Syscall(_, _) => 0x73,
            // Dups, part 2 (maybe realign?)
            Bytecode::DupTop2() => 0x78,
            Bytecode::DupTopN(_) => 0x79,
            Bytecode::UnpackIterable() => 0x7A,
            Bytecode::PackIterable() => 0x7B,
            Bytecode::SwapDyn() => 0x7C,
        }
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
