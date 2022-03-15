use crate::parser::base::IndependentNode;

use super::assert::AssertConverter;
use super::assign::AssignConverter;
use super::aug_assign::AugAssignConverter;
use super::break_converter::BreakConverter;
use super::bytecode_list::BytecodeList;
use super::class::{ClassConverter, EnumConverter, InterfaceConverter, UnionConverter};
use super::compiler_info::CompilerInfo;
use super::continue_conv::ContinueConverter;
use super::convertible::{BaseConvertible, ConverterBase};
use super::declare::DeclarationConverter;
use super::declared_assign::DeclaredAssignConverter;
use super::delete::DeleteConverter;
use super::diverge::DivergingInfo;
use super::do_while::DoWhileConverter;
use super::dotimes::DotimesConverter;
use super::fn_def::FunctionDefConverter;
use super::for_converter::ForConverter;
use super::if_converter::IfConverter;
use super::import::ImportExportConverter;
use super::inc_dec::IncDecConverter;
use super::ret::ReturnConverter;
use super::test_converter::TestConverter;
use super::try_stmt::TryConverter;
use super::typedef::TypedefConverter;
use super::while_converter::WhileConverter;
use super::with::WithConverter;
use super::yield_stmt::YieldConverter;
use super::{CompileBytes, CompileResult};

#[derive(Debug)]
pub enum BaseConverter<'a> {
    Assert(AssertConverter<'a>),
    Assign(AssignConverter<'a>),
    AugAssign(AugAssignConverter<'a>),
    Break(BreakConverter<'a>),
    ClassDef(ClassConverter<'a>),
    Continue(ContinueConverter<'a>),
    Declaration(DeclarationConverter<'a>),
    DeclaredAssign(DeclaredAssignConverter<'a>),
    Delete(DeleteConverter<'a>),
    Do(DoWhileConverter<'a>),
    Dotimes(DotimesConverter<'a>),
    Enum(EnumConverter<'a>),
    For(ForConverter<'a>),
    FunctionDef(FunctionDefConverter<'a>),
    If(IfConverter<'a>),
    Import(ImportExportConverter<'a>),
    IncDec(IncDecConverter<'a>),
    Interface(InterfaceConverter<'a>),
    Return(ReturnConverter<'a>),
    Test(TestConverter<'a>),
    Try(TryConverter<'a>),
    Typedef(TypedefConverter<'a>),
    Union(UnionConverter<'a>),
    While(WhileConverter<'a>),
    With(WithConverter<'a>),
    Yield(YieldConverter<'a>),
}

macro_rules! base_converter_each {
    ($self:ident: $var:ident => $result:expr) => {
        match $self {
            BaseConverter::Assert($var) => $result,
            BaseConverter::Assign($var) => $result,
            BaseConverter::AugAssign($var) => $result,
            BaseConverter::Break($var) => $result,
            BaseConverter::ClassDef($var) => $result,
            BaseConverter::Continue($var) => $result,
            BaseConverter::Declaration($var) => $result,
            BaseConverter::DeclaredAssign($var) => $result,
            BaseConverter::Delete($var) => $result,
            BaseConverter::Do($var) => $result,
            BaseConverter::Dotimes($var) => $result,
            BaseConverter::Enum($var) => $result,
            BaseConverter::For($var) => $result,
            BaseConverter::FunctionDef($var) => $result,
            BaseConverter::If($var) => $result,
            BaseConverter::Import($var) => $result,
            BaseConverter::IncDec($var) => $result,
            BaseConverter::Interface($var) => $result,
            BaseConverter::Return($var) => $result,
            BaseConverter::Test($var) => $result,
            BaseConverter::Try($var) => $result,
            BaseConverter::Typedef($var) => $result,
            BaseConverter::Union($var) => $result,
            BaseConverter::While($var) => $result,
            BaseConverter::With($var) => $result,
            BaseConverter::Yield($var) => $result,
        }
    };
}

impl<'a> BaseConverter<'a> {
    pub fn bytes(stmt: impl BaseConvertible<'a>, info: &'a mut CompilerInfo) -> CompileBytes {
        stmt.base_converter().convert(info)
    }

    pub fn bytes_with_return(
        stmt: impl BaseConvertible<'a>,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        stmt.base_converter().convert_with_return(info)
    }
}

impl<'a> ConverterBase for BaseConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        base_converter_each!(self: x => x.convert(info))
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        base_converter_each!(self: x => x.convert_with_return(info))
    }
}

impl<'a> BaseConvertible<'a> for &'a IndependentNode {
    type Converter = BaseConverter<'a>;

    fn base_converter(self) -> Self::Converter {
        match self {
            IndependentNode::Assert(a) => BaseConverter::Assert(a.base_converter()),
            IndependentNode::Assign(a) => BaseConverter::Assign(a.base_converter()),
            IndependentNode::AugAssign(a) => BaseConverter::AugAssign(a.base_converter()),
            IndependentNode::Break(b) => BaseConverter::Break(b.base_converter()),
            IndependentNode::ClassDef(c) => BaseConverter::ClassDef(c.base_converter()),
            IndependentNode::Context(_) => todo!(),
            IndependentNode::Continue(c) => BaseConverter::Continue(c.base_converter()),
            IndependentNode::Declaration(d) => BaseConverter::Declaration(d.base_converter()),
            IndependentNode::DeclaredAssign(d) => BaseConverter::DeclaredAssign(d.base_converter()),
            IndependentNode::Defer(_) => todo!(),
            IndependentNode::Delete(d) => BaseConverter::Delete(d.base_converter()),
            IndependentNode::Derived(_) => todo!(),
            IndependentNode::Do(d) => BaseConverter::Do(d.base_converter()),
            IndependentNode::Dotimes(d) => BaseConverter::Dotimes(d.base_converter()),
            IndependentNode::Enum(e) => BaseConverter::Enum(e.base_converter()),
            IndependentNode::For(f) => BaseConverter::For(f.base_converter()),
            IndependentNode::FunctionDef(f) => BaseConverter::FunctionDef(f.base_converter()),
            IndependentNode::If(i) => BaseConverter::If(i.base_converter()),
            IndependentNode::Import(i) => BaseConverter::Import(i.base_converter()),
            IndependentNode::IncDec(i) => BaseConverter::IncDec(i.base_converter()),
            IndependentNode::Interface(i) => BaseConverter::Interface(i.base_converter()),
            IndependentNode::Method(_) => panic!("Methods should only occur in classes"),
            IndependentNode::OpDef(_) => panic!("Operator defs should only occur in classes"),
            IndependentNode::OpAssign(_) => todo!(),
            IndependentNode::Property(_) => panic!("Property definitions should occur in classes"),
            IndependentNode::Return(r) => BaseConverter::Return(r.base_converter()),
            IndependentNode::Sync(_) => todo!(),
            IndependentNode::Test(t) => BaseConverter::Test(t.base_converter()),
            IndependentNode::Try(t) => BaseConverter::Try(t.base_converter()),
            IndependentNode::Typedef(t) => BaseConverter::Typedef(t.base_converter()),
            IndependentNode::Union(u) => BaseConverter::Union(u.base_converter()),
            IndependentNode::While(w) => BaseConverter::While(w.base_converter()),
            IndependentNode::With(w) => BaseConverter::With(w.base_converter()),
            IndependentNode::Yield(y) => BaseConverter::Yield(y.base_converter()),
        }
    }
}
