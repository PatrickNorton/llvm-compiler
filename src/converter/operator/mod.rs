mod boolean;
mod casted;
mod contains;
mod equals;
mod identity;
mod instance;
mod normal;
mod null_op;

use std::borrow::Cow;

use num::BigInt;

use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::type_obj::TypeObject;
use crate::converter::CompileResult;
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator::{OperatorNode, OperatorTypeNode};

use self::boolean::BoolOpConverter;
use self::casted::CastedConverter;
use self::contains::InConverter;
use self::equals::EqualsConverter;
use self::identity::IsConverter;
use self::instance::InstanceConverter;
use self::normal::NormalOperatorConverter;
use self::null_op::{unwrap_option, NullOpConverter};

use super::constant::LangConstant;
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::CompilerException;
use super::test_converter::TestConverter;
use super::{int_arithmetic, str_arithmetic, CompileBytes, CompileConstant, CompileTypes};

#[derive(Debug)]
pub struct OperatorConverter<'a> {
    value: InnerOpConverter<'a>,
}

#[derive(Debug)]
enum InnerOpConverter<'a> {
    Boolean(BoolOpConverter<'a>),
    Casted(CastedConverter<'a>),
    Equals(EqualsConverter<'a>),
    In(InConverter<'a>),
    Instance(InstanceConverter<'a>),
    Is(IsConverter<'a>),
    Normal(NormalOperatorConverter<'a>),
    Null(NullOpConverter<'a>),
}

impl<'a> ConverterBase for OperatorConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        match &mut self.value {
            InnerOpConverter::Boolean(b) => b.convert(info),
            InnerOpConverter::Casted(c) => c.convert(info),
            InnerOpConverter::Equals(e) => e.convert(info),
            InnerOpConverter::In(i) => i.convert(info),
            InnerOpConverter::Instance(i) => i.convert(info),
            InnerOpConverter::Is(i) => i.convert(info),
            InnerOpConverter::Normal(n) => n.convert(info),
            InnerOpConverter::Null(n) => n.convert(info),
        }
    }
}

impl<'a> ConverterTest for OperatorConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        match &mut self.value {
            InnerOpConverter::Boolean(b) => b.return_type(info),
            InnerOpConverter::Casted(c) => c.return_type(info),
            InnerOpConverter::Equals(e) => e.return_type(info),
            InnerOpConverter::In(i) => i.return_type(info),
            InnerOpConverter::Instance(i) => i.return_type(info),
            InnerOpConverter::Is(i) => i.return_type(info),
            InnerOpConverter::Normal(n) => n.return_type(info),
            InnerOpConverter::Null(n) => n.return_type(info),
        }
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        match &mut self.value {
            InnerOpConverter::Boolean(b) => b.constant_return(info),
            InnerOpConverter::Casted(_) => Ok(None),
            InnerOpConverter::Equals(e) => e.constant_return(info),
            InnerOpConverter::In(i) => i.constant_return(info),
            InnerOpConverter::Instance(_) => Ok(None),
            InnerOpConverter::Is(i) => i.constant_return(info),
            InnerOpConverter::Normal(n) => n.constant_return(info),
            InnerOpConverter::Null(_) => Ok(None),
        }
    }
}

impl<'a> OperatorConverter<'a> {
    pub fn new(node: &'a OperatorNode, ret_count: u16) -> Self {
        Self::of_components(
            node.get_operator(),
            node.get_operands(),
            node.line_info().clone(),
            ret_count,
        )
    }

    pub fn of_components(
        op: OperatorTypeNode,
        args: &'a [ArgumentNode],
        line_info: LineInfo,
        ret_count: u16,
    ) -> Self {
        Self {
            value: match op {
                OperatorTypeNode::NullCoerce
                | OperatorTypeNode::NotNull
                | OperatorTypeNode::Optional => {
                    InnerOpConverter::Null(NullOpConverter::new(op, args, line_info, ret_count))
                }
                OperatorTypeNode::BoolAnd
                | OperatorTypeNode::BoolOr
                | OperatorTypeNode::BoolNot
                | OperatorTypeNode::BoolXor => {
                    InnerOpConverter::Boolean(BoolOpConverter::new(op, args, line_info, ret_count))
                }
                OperatorTypeNode::Is => {
                    InnerOpConverter::Is(IsConverter::new(true, args, line_info, ret_count))
                }
                OperatorTypeNode::IsNot => {
                    InnerOpConverter::Is(IsConverter::new(false, args, line_info, ret_count))
                }
                OperatorTypeNode::In => {
                    InnerOpConverter::In(InConverter::new(true, args, line_info, ret_count))
                }
                OperatorTypeNode::NotIn => {
                    InnerOpConverter::In(InConverter::new(false, args, line_info, ret_count))
                }
                OperatorTypeNode::Instanceof => InnerOpConverter::Instance(InstanceConverter::new(
                    true, args, line_info, ret_count,
                )),
                OperatorTypeNode::NotInstanceof => InnerOpConverter::Instance(
                    InstanceConverter::new(false, args, line_info, ret_count),
                ),
                OperatorTypeNode::Equals => {
                    InnerOpConverter::Equals(EqualsConverter::new(true, args, line_info, ret_count))
                }
                OperatorTypeNode::NotEquals => InnerOpConverter::Equals(EqualsConverter::new(
                    false, args, line_info, ret_count,
                )),
                OperatorTypeNode::Casted => {
                    InnerOpConverter::Casted(CastedConverter::new(args, line_info, ret_count))
                }
                op => InnerOpConverter::Normal(NormalOperatorConverter::new(
                    op, args, line_info, ret_count,
                )),
            },
        }
    }

    pub fn convert_with_as(
        node: &'a OperatorNode,
        info: &'a mut CompilerInfo,
        ret_count: u16,
    ) -> CompileResult<(BytecodeList, TypeObject)> {
        Self::new(node, ret_count).convert_as(info)
    }

    pub fn unwrap_option(info: &mut CompilerInfo, bytes: &mut BytecodeList, value: &str) {
        unwrap_option(info, bytes, value)
    }

    fn convert_as(&self, info: &mut CompilerInfo) -> CompileResult<(BytecodeList, TypeObject)> {
        match &self.value {
            InnerOpConverter::Boolean(b) => b.convert_with_as(),
            InnerOpConverter::Casted(c) => c.convert_with_as(),
            InnerOpConverter::Equals(e) => e.convert_with_as(),
            InnerOpConverter::In(i) => i.convert_with_as(),
            InnerOpConverter::Instance(i) => i.convert_with_as(info),
            InnerOpConverter::Is(i) => i.convert_with_as(info),
            InnerOpConverter::Normal(n) => n.convert_with_as(),
            InnerOpConverter::Null(n) => n.convert_with_as(info),
        }
    }
}

pub fn operator_bytecode(op: OperatorTypeNode) -> Bytecode {
    match op {
        OperatorTypeNode::Add => Bytecode::Plus(),
        OperatorTypeNode::Subtract => Bytecode::Minus(),
        OperatorTypeNode::Multiply => Bytecode::Times(),
        OperatorTypeNode::Divide => Bytecode::Divide(),
        OperatorTypeNode::FloorDiv => Bytecode::FloorDiv(),
        OperatorTypeNode::Modulo => Bytecode::Mod(),
        OperatorTypeNode::Power => Bytecode::Power(),
        OperatorTypeNode::LeftBitshift => Bytecode::LBitshift(),
        OperatorTypeNode::RightBitshift => Bytecode::RBitshift(),
        OperatorTypeNode::BitwiseAnd => Bytecode::BitwiseAnd(),
        OperatorTypeNode::BitwiseOr => Bytecode::BitwiseOr(),
        OperatorTypeNode::BitwiseXor => Bytecode::BitwiseXor(),
        OperatorTypeNode::Compare => Bytecode::Compare(),
        OperatorTypeNode::USubtract => Bytecode::UMinus(),
        OperatorTypeNode::BitwiseNot => Bytecode::BitwiseNot(),
        OperatorTypeNode::BoolAnd => Bytecode::BoolAnd(),
        OperatorTypeNode::BoolOr => Bytecode::BoolOr(),
        OperatorTypeNode::BoolNot => Bytecode::BoolNot(),
        OperatorTypeNode::Is => Bytecode::Identical(),
        OperatorTypeNode::Instanceof => Bytecode::Instanceof(),
        OperatorTypeNode::Equals => Bytecode::Equal(),
        OperatorTypeNode::LessThan => Bytecode::LessThan(),
        OperatorTypeNode::GreaterThan => Bytecode::GreaterThan(),
        OperatorTypeNode::LessEqual => Bytecode::LessEqual(),
        OperatorTypeNode::GreaterEqual => Bytecode::GreaterEqual(),
        _ => panic!("Unexpected bytecode {}", op.sequence()),
    }
}

pub fn is_mandatory(op: OperatorTypeNode) -> bool {
    matches!(
        op,
        OperatorTypeNode::BoolNot
            | OperatorTypeNode::Is
            | OperatorTypeNode::IsNot
            | OperatorTypeNode::In
            | OperatorTypeNode::NotIn
            | OperatorTypeNode::Casted
            | OperatorTypeNode::USubtract
    )
}

pub(self) fn as_exception(line_info: &LineInfo) -> CompilerException {
    CompilerException::of(
        "Cannot use 'as' here, condition must be an 'instanceof', '?', or 'is not null' statement",
        line_info,
    )
}

pub(self) fn default_constant(
    op: OperatorTypeNode,
    info: &mut CompilerInfo,
    args: &[ArgumentNode],
) -> CompileConstant {
    let constants = match all_consts(info, args)? {
        Option::Some(x) => x,
        Option::None => return Ok(None),
    };
    if constants[0].is_string() {
        Ok(str_arithmetic::compute_const(op, constants))
    } else {
        Ok(all_ints(constants).and_then(|x| int_arithmetic::compute_const(op, x)))
    }
}

fn all_consts(
    info: &mut CompilerInfo,
    args: &[ArgumentNode],
) -> CompileResult<Option<Vec<LangConstant>>> {
    args.iter()
        .map(|x| TestConverter::constant_return(x.get_argument(), info, 1))
        .collect()
}

fn all_ints(consts: Vec<LangConstant>) -> Option<Vec<BigInt>> {
    consts
        .iter()
        .map(|x| int_arithmetic::convert_const(x).map(Cow::into_owned))
        .collect()
}

test_convertible!(OperatorNode, OperatorConverter);
