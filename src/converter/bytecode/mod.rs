mod argc;
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
use indexmap::IndexSet;

pub use self::argc::ArgcBytecode;
pub use self::constant::ConstantBytecode;
pub use self::fn_no::FunctionNoBytecode;
pub use self::location::{Label, LocationBytecode};
pub use self::operator::OperatorBytecode;
pub use self::stack_pos::StackPosBytecode;
pub use self::syscall::SyscallBytecode;
pub use self::table_no::TableNoBytecode;
pub use self::variable::VariableBytecode;
pub use self::variant::VariantBytecode;

use super::constant::LangConstant;
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

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        functions: &[&Function],
    ) -> std::fmt::Result;

    fn assemble(&self, buffer: &mut Vec<u8>, constants: &IndexSet<LangConstant>);
}

impl Bytecode {
    pub fn jump_if(value: bool, label: Label) -> Bytecode {
        if value {
            Bytecode::JumpTrue(label.into())
        } else {
            Bytecode::JumpFalse(label.into())
        }
    }

    pub fn size(&self) -> usize {
        todo!()
    }

    pub fn assemble(&self) -> Vec<u8> {
        todo!()
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
}

// Bytecode formatting impls

#[derive(Debug, new)]
struct BytecodeFormat<'a> {
    bytecode: &'a Bytecode,
    info: &'a [&'a Function],
}

#[derive(Debug, new)]
struct BytecodeValFormat<'a, T> {
    bytecode: &'a T,
    functions: &'a [&'a Function],
}

impl<'a> Display for BytecodeFormat<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.bytecode.write_str(f, self.info)
    }
}

impl<'a, T: BytecodeType> Display for BytecodeValFormat<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.bytecode.write_str(f, self.functions)
    }
}

fn write_single(
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
    a: &impl BytecodeType,
    functions: &[&Function],
) -> std::fmt::Result {
    write!(f, "{:<16}{}", name, BytecodeValFormat::new(a, functions))
}

fn write_double(
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
    a: &impl BytecodeType,
    b: &impl BytecodeType,
    functions: &[&Function],
) -> std::fmt::Result {
    write!(
        f,
        "{:<16}{}, {}",
        name,
        BytecodeValFormat::new(a, functions),
        BytecodeValFormat::new(b, functions)
    )
}

impl Bytecode {
    pub fn display<'a>(&'a self, functions: &'a [&'a Function]) -> impl Display + 'a {
        BytecodeFormat::new(self, functions)
    }

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        functions: &[&Function],
    ) -> std::fmt::Result {
        match self {
            Bytecode::Nop() => write!(f, "NOP"),
            Bytecode::LoadNull() => write!(f, "LOAD_NULL"),
            Bytecode::LoadConst(a) => write_single(f, "LOAD_CONST", a, functions),
            Bytecode::LoadValue(a) => write_single(f, "LOAD_VALUE", a, functions),
            Bytecode::LoadDot(a) => write_single(f, "LOAD_DOT", a, functions),
            Bytecode::LoadSubscript(a) => write_single(f, "LOAD_SUBSCRIPT", a, functions),
            Bytecode::LoadOp(a) => write_single(f, "LOAD_OP", a, functions),
            Bytecode::PopTop() => write!(f, "POP_TOP"),
            Bytecode::DupTop() => write!(f, "DUP_TOP"),
            Bytecode::Swap2() => write!(f, "SWAP_2"),
            Bytecode::Swap3() => write!(f, "SWAP_3"),
            Bytecode::SwapN(a) => write_single(f, "SWAP_N", a, functions),
            Bytecode::Store(a) => write_single(f, "STORE", a, functions),
            Bytecode::StoreSubscript(a) => write_single(f, "STORE_SUBSCRIPT", a, functions),
            Bytecode::StoreAttr(a) => write_single(f, "STORE_ATTR", a, functions),
            Bytecode::SwapStack(a, b) => write_double(f, "SWAP_STACK", a, b, functions),
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
            Bytecode::CallOp(a, b) => write_double(f, "CALL_OP", a, b, functions),
            Bytecode::PackTuple(a) => write_single(f, "PACK_TUPLE", a, functions),
            Bytecode::UnpackTuple() => write!(f, "UNPACK_TUPLE"),
            Bytecode::Equal() => write!(f, "EQUAL"),
            Bytecode::LessThan() => write!(f, "LESS_THAN"),
            Bytecode::GreaterThan() => write!(f, "GREATER_THAN"),
            Bytecode::LessEqual() => write!(f, "LESS_EQUAL"),
            Bytecode::GreaterEqual() => write!(f, "GREATER_EQUAL"),
            Bytecode::Contains() => write!(f, "CONTAINS"),
            Bytecode::Jump(a) => write_single(f, "JUMP", a, functions),
            Bytecode::JumpFalse(a) => write_single(f, "JUMP_FALSE", a, functions),
            Bytecode::JumpTrue(a) => write_single(f, "JUMP_TRUE", a, functions),
            Bytecode::JumpNN(a) => write_single(f, "JUMP_NN", a, functions),
            Bytecode::JumpNull(a) => write_single(f, "JUMP_NULL", a, functions),
            Bytecode::CallMethod(a, b) => write_double(f, "CALL_METHOD", a, b, functions),
            Bytecode::CallTos(a) => write_single(f, "CALL_TOS", a, functions),
            Bytecode::CallFn(a, b) => write_double(f, "CALL_FN", a, b, functions),
            Bytecode::TailMethod(a) => write_single(f, "TAIL_METHOD", a, functions),
            Bytecode::TailTos(a) => write_single(f, "TAIL_TOS", a, functions),
            Bytecode::TailFn(a, b) => write_double(f, "TAIL_FN", a, b, functions),
            Bytecode::Return(a) => write_single(f, "RETURN", a, functions),
            Bytecode::Yield(a) => write_single(f, "YIELD", a, functions),
            Bytecode::SwitchTable(a) => write_single(f, "SWITCH_TABLE", a, functions),
            Bytecode::Throw() => write!(f, "THROW"),
            Bytecode::ThrowQuick(a) => write_single(f, "THROW_QUICK", a, functions),
            Bytecode::EnterTry(a) => write_single(f, "ENTER_TRY", a, functions),
            Bytecode::ExceptN(a) => write_single(f, "EXCEPT_N", a, functions),
            Bytecode::Finally() => write!(f, "FINALLY"),
            Bytecode::EndTry(a) => write_single(f, "END_TRY", a, functions),
            Bytecode::FuncDef() => write!(f, "FUNC_DEF"),
            Bytecode::ClassDef() => write!(f, "CLASS_DEF"),
            Bytecode::EndClass() => write!(f, "END_CLASS"),
            Bytecode::ForIter(a, b) => write_double(f, "FOR_ITER", a, b, functions),
            Bytecode::ListCreate(a) => write_single(f, "LIST_CREATE", a, functions),
            Bytecode::SetCreate(a) => write_single(f, "SET_CREATE", a, functions),
            Bytecode::DictCreate(a) => write_single(f, "DICT_CREATE", a, functions),
            Bytecode::ListAdd() => write!(f, "LIST_ADD"),
            Bytecode::SetAdd() => write!(f, "SET_ADD"),
            Bytecode::DictAdd() => write!(f, "DICT_ADD"),
            Bytecode::Dotimes(a) => write_single(f, "DOTIMES", a, functions),
            Bytecode::ForParallel(a, b) => write_double(f, "FOR_PARALLEL", a, b, functions),
            Bytecode::MakeSlice() => write!(f, "MAKE_SLICE"),
            Bytecode::ListDyn() => write!(f, "LIST_DYN"),
            Bytecode::SetDyn() => write!(f, "SET_DYN"),
            Bytecode::DictDyn() => write!(f, "LIST_DYN"),
            Bytecode::ListCap() => write!(f, "LIST_CAP"),
            Bytecode::SetCap() => write!(f, "SET_CAP"),
            Bytecode::DictCap() => write!(f, "DICT_CAP"),
            Bytecode::DoStatic(a) => write_single(f, "DO_STATIC", a, functions),
            Bytecode::StoreStatic(a) => write_single(f, "STORE_STATIC", a, functions),
            Bytecode::LoadStatic(a) => write_single(f, "LOAD_STATIC", a, functions),
            Bytecode::GetVariant(a) => write_single(f, "GET_VARIANT", a, functions),
            Bytecode::MakeVariant(a) => write_single(f, "MAKE_VARIANT", a, functions),
            Bytecode::VariantNo() => write!(f, "VARIANT_NO"),
            Bytecode::MakeOption() => write!(f, "MAKE_OPTION"),
            Bytecode::IsSome() => write!(f, "IS_SOME"),
            Bytecode::UnwrapOption() => write!(f, "UNWRAP_OPTION"),
            Bytecode::MakeFunction(a) => write_single(f, "MAKE_FUNCTION", a, functions),
            Bytecode::GetType() => write!(f, "GET_TYPE"),
            Bytecode::GetSys(a) => write_single(f, "GET_SYS", a, functions),
            Bytecode::Syscall(a, b) => write_double(f, "SYSCALL", a, b, functions),
            Bytecode::DupTop2() => write!(f, "DUP_TOP_2"),
            Bytecode::DupTopN(a) => write_single(f, "DUP_TOP_N", a, functions),
            Bytecode::UnpackIterable() => write!(f, "UNPACK_ITERABLE"),
            Bytecode::PackIterable() => write!(f, "PACK_ITERABLE"),
            Bytecode::SwapDyn() => write!(f, "SWAP_DYN"),
        }
    }
}
