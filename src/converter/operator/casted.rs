use derive_new::new;

use crate::converter::bytecode::{Bytecode, Label};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::constant::LangConstant;
use crate::converter::convertible::{ConverterBase, ConverterTest, TestConvertible};
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::{TypeObject, TypeTypeObject};
use crate::converter::warning::{self, WarningType};
use crate::converter::{CompileBytes, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::util::first;

use super::as_exception;

#[derive(Debug, new)]
pub(super) struct CastedConverter<'a> {
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> CastedConverter<'a> {
    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        if self.args.len() != 2 {
            Err(self.args_exception().into())
        } else if self.ret_count > 1 {
            Err(self.ret_exception().into())
        } else {
            let ret_types = TestConverter::return_type(info, self.args[1].get_argument(), 1)?;
            match TypeTypeObject::try_from(first(ret_types)) {
                Result::Ok(ret) => Ok(vec![ret.represented_type().clone()]),
                Result::Err(ret) => Err(self.type_exception(ret).into()),
            }
        }
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.args.len() != 2 {
            Err(self.args_exception().into())
        } else if self.ret_count == 0 {
            warning::warn(
                "Cast is useless when not assigned to a value",
                WarningType::Unused,
                info,
                &self.line_info,
            )?;
            TestConverter::bytes(self.args[0].get_argument(), info, 0)
        } else if self.ret_count != 1 {
            Err(self.ret_exception().into())
        } else {
            self.convert_normal(info)
        }
    }

    pub fn convert_with_as(&self) -> CompileResult<(BytecodeList, TypeObject)> {
        Err(as_exception(&self.line_info).into())
    }

    fn convert_normal(&self, info: &mut CompilerInfo) -> CompileBytes {
        let mut arg_converter = self.args[0].get_argument().test_converter(1);
        let mut type_converter = self.args[1].get_argument().test_converter(1);
        let ret_type = match TypeTypeObject::try_from(first(type_converter.return_type(info)?)) {
            Ok(x) => x,
            Err(e) => return Err(self.type_exception(e).into()),
        };
        let represented_type = ret_type.represented_type().clone();
        let arg_type = first(arg_converter.return_type(info)?);
        if represented_type.is_superclass(&arg_type) {
            warning::warn(
                format!(
                    "Useless cast: {} is already a subclass of {}",
                    arg_type.name(),
                    represented_type.name()
                ),
                WarningType::TrivialValue,
                info,
                &self.line_info,
            )?;
            return arg_converter.convert(info);
        }
        let mut bytes = arg_converter.convert(info)?;
        bytes.add(Bytecode::DupTop());
        bytes.extend(type_converter.convert(info)?);
        bytes.add(Bytecode::Instanceof());
        let jumplbl = Label::new();
        bytes.add(Bytecode::Jump(jumplbl.clone().into()));
        let message = format!("Cannot cast tp type {}", represented_type.name());
        bytes.add(Bytecode::LoadConst(LangConstant::from(message).into()));
        bytes.add(Bytecode::ThrowQuick(1.into()));
        bytes.add_label(jumplbl);
        Ok(bytes)
    }

    fn ret_exception(&self) -> CompilerException {
        CompilerException::of(
            format!(
                "casted operator only returns one operand, not {}",
                self.ret_count
            ),
            &self.line_info,
        )
    }

    fn args_exception(&self) -> CompilerException {
        CompilerException::of(
            format!(
                "casted operator only takes two operands, got {}",
                self.args.len()
            ),
            &self.line_info,
        )
    }

    fn type_exception(&self, type_obj: TypeObject) -> CompilerException {
        CompilerException::of(
            format!(
                "Second argument of casted operator must be a type, not {}",
                type_obj.name()
            ),
            &self.line_info,
        )
    }
}
