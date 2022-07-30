use core::slice;

use crate::parser::name::NameNode;
use crate::parser::test_node::TestNode;
use crate::util::first;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::class::VariantConverter;
use super::compiler_info::CompilerInfo;
use super::comprehension::ComprehensionConverter;
use super::convertible::{ConverterBase, ConverterTest, TestConvertible};
use super::dict_comp::DictCompConverter;
use super::dict_literal::DictLiteralConverter;
use super::diverge::DivergingInfo;
use super::dotted_var::DotConverter;
use super::f_string::FormattedStringConverter;
use super::fn_call::FunctionCallConverter;
use super::index::IndexConverter;
use super::lambda::LambdaConverter;
use super::literal::LiteralConverter;
use super::number::NumberConverter;
use super::operator::OperatorConverter;
use super::raise::RaiseConverter;
use super::range::RangeConverter;
use super::slice::SliceConverter;
use super::string::StringConverter;
use super::switch::SwitchConverter;
use super::ternary::TernaryConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::variable::VariableConverter;
use super::{CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug)]
pub enum TestConverter<'a> {
    Comprehension(ComprehensionConverter<'a>),
    Dotted(DotConverter<'a>),
    DictComp(DictCompConverter<'a>),
    DictLiteral(DictLiteralConverter<'a>),
    Formatted(FormattedStringConverter<'a>),
    Function(FunctionCallConverter<'a>),
    Index(IndexConverter<'a>),
    Lambda(LambdaConverter<'a>),
    Literal(LiteralConverter<'a>),
    Number(NumberConverter<'a>),
    Operator(OperatorConverter<'a>),
    Raise(RaiseConverter<'a>),
    Range(RangeConverter<'a>),
    Slice(SliceConverter<'a>),
    String(StringConverter<'a>),
    Switch(SwitchConverter<'a>),
    Ternary(TernaryConverter<'a>),
    Variable(VariableConverter<'a>),
    Variant(VariantConverter<'a>),
}

macro_rules! test_converter_each {
    ($self:ident: $val:ident => $result:expr) => {
        match $self {
            TestConverter::Comprehension($val) => $result,
            TestConverter::Dotted($val) => $result,
            TestConverter::DictComp($val) => $result,
            TestConverter::DictLiteral($val) => $result,
            TestConverter::Formatted($val) => $result,
            TestConverter::Function($val) => $result,
            TestConverter::Index($val) => $result,
            TestConverter::Lambda($val) => $result,
            TestConverter::Literal($val) => $result,
            TestConverter::Number($val) => $result,
            TestConverter::Operator($val) => $result,
            TestConverter::Raise($val) => $result,
            TestConverter::Range($val) => $result,
            TestConverter::Slice($val) => $result,
            TestConverter::String($val) => $result,
            TestConverter::Switch($val) => $result,
            TestConverter::Ternary($val) => $result,
            TestConverter::Variable($val) => $result,
            TestConverter::Variant($val) => $result,
        }
    };
}

impl<'a> TestConverter<'a> {
    pub fn constant_return(
        node: impl TestConvertible<'a>,
        info: &mut CompilerInfo,
        ret_count: u16,
    ) -> CompileConstant {
        node.test_converter(ret_count).constant_return(info)
    }

    pub fn return_type(
        info: &mut CompilerInfo,
        node: impl TestConvertible<'a>,
        ret_count: u16,
    ) -> CompileTypes {
        node.test_converter(ret_count).return_type(info)
    }

    pub fn bytes(
        node: impl TestConvertible<'a>,
        info: &mut CompilerInfo,
        ret_count: u16,
    ) -> CompileBytes {
        node.test_converter(ret_count).convert(info)
    }

    pub fn bytes_maybe_option(
        node: impl TestConvertible<'a>,
        info: &mut CompilerInfo,
        ret_count: u16,
        end_type: &'a TypeObject,
    ) -> CompileBytes {
        if let TypeObject::Option(_) = end_type {
            bytes_maybe_option(node.test_converter(ret_count), info, end_type)
        } else {
            bytes_maybe_option(
                node.test_conv_expected(ret_count, slice::from_ref(end_type)),
                info,
                end_type,
            )
        }
    }
}

fn bytes_maybe_option(
    mut converter: impl ConverterTest,
    info: &mut CompilerInfo,
    end_type: &TypeObject,
) -> CompileBytes {
    let ret_type = first(converter.return_type(info)?);
    if OptionTypeObject::needs_make_option(end_type, &ret_type) {
        let mut bytes = converter.convert(info)?;
        bytes.add(Bytecode::MakeOption());
        Ok(bytes)
    } else {
        converter.convert(info)
    }
}

impl<'a> ConverterBase for TestConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        test_converter_each!(self: x => x.convert(info))
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        test_converter_each!(self: x => x.convert_with_return(info))
    }
}

impl<'a> ConverterTest for TestConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        test_converter_each!(self: x => x.return_type(info))
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        test_converter_each!(self: x => x.constant_return(info))
    }
}

impl<'a> TestConvertible<'a> for &'a TestNode {
    type Converter = TestConverter<'a>;

    fn test_converter(self, ret_count: u16) -> Self::Converter {
        match self {
            TestNode::Comprehension(c) => TestConverter::Comprehension(c.test_converter(ret_count)),
            TestNode::DictComp(d) => TestConverter::DictComp(d.test_converter(ret_count)),
            TestNode::DictLiteral(d) => TestConverter::DictLiteral(d.test_converter(ret_count)),
            TestNode::Empty(_) => panic!("Should not be converting empty test nodes"),
            TestNode::Formatted(f) => TestConverter::Formatted(f.test_converter(ret_count)),
            TestNode::Lambda(l) => TestConverter::Lambda(l.test_converter(ret_count)),
            TestNode::Literal(l) => TestConverter::Literal(l.test_converter(ret_count)),
            TestNode::Name(NameNode::Dotted(d)) => {
                TestConverter::Dotted(d.test_converter(ret_count))
            }
            TestNode::Name(NameNode::EscapedOp(_)) => todo!(),
            TestNode::Name(NameNode::Function(f)) => {
                TestConverter::Function(f.test_converter(ret_count))
            }
            TestNode::Name(NameNode::Index(i)) => TestConverter::Index(i.test_converter(ret_count)),
            TestNode::Name(NameNode::SpecialOp(_)) => todo!(),
            TestNode::Name(NameNode::Variable(v)) => {
                TestConverter::Variable(v.test_converter(ret_count))
            }
            TestNode::Number(n) => TestConverter::Number(n.test_converter(ret_count)),
            TestNode::Operator(o) => TestConverter::Operator(o.test_converter(ret_count)),
            TestNode::Raise(r) => TestConverter::Raise(r.test_converter(ret_count)),
            TestNode::Range(r) => TestConverter::Range(r.test_converter(ret_count)),
            TestNode::Slice(s) => TestConverter::Slice(s.test_converter(ret_count)),
            TestNode::Some(_) => todo!(),
            TestNode::String(s) => TestConverter::String(s.test_converter(ret_count)),
            TestNode::Switch(s) => TestConverter::Switch(s.test_converter(ret_count)),
            TestNode::Ternary(t) => TestConverter::Ternary(t.test_converter(ret_count)),
            TestNode::Variant(v) => TestConverter::Variant(v.test_converter(ret_count)),
        }
    }

    fn test_conv_expected(self, ret_count: u16, expected: &'a [TypeObject]) -> Self::Converter {
        match self {
            TestNode::Comprehension(c) => {
                TestConverter::Comprehension(c.test_conv_expected(ret_count, expected))
            }
            TestNode::DictComp(d) => {
                TestConverter::DictComp(d.test_conv_expected(ret_count, expected))
            }
            TestNode::DictLiteral(d) => {
                TestConverter::DictLiteral(d.test_conv_expected(ret_count, expected))
            }
            TestNode::Empty(_) => panic!("Should not be converting empty test nodes"),
            TestNode::Formatted(f) => {
                TestConverter::Formatted(f.test_conv_expected(ret_count, expected))
            }
            TestNode::Lambda(l) => TestConverter::Lambda(l.test_conv_expected(ret_count, expected)),
            TestNode::Literal(l) => {
                TestConverter::Literal(l.test_conv_expected(ret_count, expected))
            }
            TestNode::Name(NameNode::Dotted(d)) => {
                TestConverter::Dotted(d.test_conv_expected(ret_count, expected))
            }
            TestNode::Name(NameNode::EscapedOp(_)) => todo!("Escaped operators"),
            TestNode::Name(NameNode::Function(f)) => {
                TestConverter::Function(f.test_conv_expected(ret_count, expected))
            }
            TestNode::Name(NameNode::Index(i)) => {
                TestConverter::Index(i.test_conv_expected(ret_count, expected))
            }
            TestNode::Name(NameNode::SpecialOp(_)) => todo!(),
            TestNode::Name(NameNode::Variable(v)) => {
                TestConverter::Variable(v.test_conv_expected(ret_count, expected))
            }
            TestNode::Number(n) => TestConverter::Number(n.test_conv_expected(ret_count, expected)),
            TestNode::Operator(o) => {
                TestConverter::Operator(o.test_conv_expected(ret_count, expected))
            }
            TestNode::Raise(r) => TestConverter::Raise(r.test_conv_expected(ret_count, expected)),
            TestNode::Range(r) => TestConverter::Range(r.test_conv_expected(ret_count, expected)),
            TestNode::Slice(s) => TestConverter::Slice(s.test_conv_expected(ret_count, expected)),
            TestNode::Some(_) => todo!(),
            TestNode::String(s) => TestConverter::String(s.test_conv_expected(ret_count, expected)),
            TestNode::Switch(s) => TestConverter::Switch(s.test_conv_expected(ret_count, expected)),
            TestNode::Ternary(t) => {
                TestConverter::Ternary(t.test_conv_expected(ret_count, expected))
            }
            TestNode::Variant(v) => {
                TestConverter::Variant(v.test_conv_expected(ret_count, expected))
            }
        }
    }
}

impl<'a> From<DotConverter<'a>> for TestConverter<'a> {
    fn from(x: DotConverter<'a>) -> Self {
        TestConverter::Dotted(x)
    }
}
