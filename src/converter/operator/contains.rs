use derive_new::new;
use itertools::Itertools;
use num::{BigInt, One, Signed, ToPrimitive, Zero};

use crate::converter::argument::Argument;
use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::constant::{BytesConstant, LangConstant, RangeConstant, StringConstant};
use crate::converter::convertible::{ConverterBase, ConverterTest, TestConvertible};
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::TypeObject;
use crate::converter::{
    int_arithmetic, CompileBytes, CompileConstant, CompileResult, CompileTypes,
};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::first;

use super::as_exception;

#[derive(Debug, new)]
pub(super) struct InConverter<'a> {
    in_type: bool,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> InConverter<'a> {
    pub fn constant_return(&self, info: &mut CompilerInfo) -> CompileConstant {
        if self.args.len() != 2 {
            return Ok(None);
        }
        let arg0_const = TestConverter::constant_return(self.args[0].get_argument(), info, 1)?;
        let arg1_const = TestConverter::constant_return(self.args[1].get_argument(), info, 1)?;
        let (arg0, arg1) = match (arg0_const, arg1_const) {
            (Some(x), Some(y)) => (x, y),
            _ => return Ok(None),
        };
        match arg1 {
            LangConstant::Bytes(arg1) => self.bytes_const(arg0, arg1),
            LangConstant::Range(arg1) => self.range_const(arg0, arg1),
            LangConstant::String(arg1) => self.string_const(arg0, arg1),
            _ => Ok(None),
        }
    }

    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().bool_type().clone()])
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.args.len() != 2 {
            return Err(CompilerException::of(
                format!(
                    "Expected 2 arguments for 'in' operator, got {}",
                    self.args.len()
                ),
                &self.line_info,
            )
            .into());
        }
        if let Option::Some(constant) = self.constant_return(info)? {
            return Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())));
        }
        let mut contained = self.args[0].get_argument().test_converter(1);
        let mut container = self.args[1].get_argument().test_converter(1);
        let container_type = first(container.return_type(info)?);
        let op_info = container_type.try_operator_info(&self.line_info, OpSpTypeNode::In, info)?;
        let contained_type = first(contained.return_type(info)?);
        if !op_info.matches(&[Argument::new(String::new(), contained_type.clone())]) {
            let args = op_info.get_args().get_normal_args();
            let arg_types = args.iter().map(|x| x.get_type().name()).format(", ");
            return Err(CompilerException::with_note(
                format!(
                    "Cannot call operator 'in' on type '{}'",
                    contained_type.name()
                ),
                format!(
                    "\nArguments received: {}\nArguments expected: {}",
                    contained_type.name(),
                    arg_types,
                ),
                &self.line_info,
            )
            .into());
        }
        let mut bytes = contained.convert(info)?;
        bytes.extend(container.convert(info)?);
        bytes.add(Bytecode::Swap2());
        bytes.add(Bytecode::Contains());
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        } else if !self.in_type {
            bytes.add(Bytecode::BoolNot());
        }
        Ok(bytes)
    }

    pub fn convert_with_as(&self) -> CompileResult<(BytecodeList, TypeObject)> {
        Err(as_exception(&self.line_info).into())
    }

    fn bytes_const(&self, arg0: LangConstant, arg1: BytesConstant) -> CompileConstant {
        let value = match arg0 {
            LangConstant::Int(i) => match u8::try_from(i.get_value()) {
                Result::Ok(x) => x,
                Result::Err(_) => return Ok(Some(false.into())),
            },
            LangConstant::Bigint(b) => match b.get_value().to_u8() {
                Option::Some(x) => x,
                Option::None => return Ok(Some(false.into())),
            },
            _ => return Ok(None),
        };
        Ok(Some(arg1.get_value().contains(&value).into()))
    }

    fn range_const(&self, arg0: LangConstant, arg1: RangeConstant) -> CompileConstant {
        let value = match int_arithmetic::convert_const(&arg0) {
            Option::Some(x) => x,
            Option::None => return Ok(None),
        };
        let step = arg1.get_step().clone().unwrap_or_else(BigInt::one);
        if let Option::Some(start) = arg1.get_start() {
            if let Option::Some(stop) = arg1.get_stop() {
                let is_between = if !step.is_negative() {
                    &*value >= start && &*value < stop && goes_into(&value, start, &step)
                } else {
                    &*value >= start && &*value < stop && goes_into(&value, start, &-step)
                };
                Ok(Some(is_between.into()))
            } else {
                let is_between = if !step.is_negative() {
                    &*value >= start && goes_into(&value, start, &step)
                } else {
                    &*value <= start && goes_into(&value, start, &-step)
                };
                Ok(Some(is_between.into()))
            }
        } else if let Option::Some(stop) = arg1.get_stop() {
            let is_between = if !step.is_negative() {
                &*value < stop && goes_into(&value, stop, &step)
            } else {
                &*value > stop && goes_into(&value, stop, &-step)
            };
            Ok(Some(is_between.into()))
        } else if !step.is_one() {
            Ok(None)
        } else {
            Ok(Some(true.into()))
        }
    }

    fn string_const(&self, arg0: LangConstant, arg1: StringConstant) -> CompileConstant {
        match arg0 {
            LangConstant::Char(c) => Ok(Some(arg1.get_value().contains(c.get_value()).into())),
            _ => Ok(None),
        }
    }
}

#[inline]
fn goes_into(value: &BigInt, start: &BigInt, step: &BigInt) -> bool {
    ((value - start) % step).is_zero()
}
