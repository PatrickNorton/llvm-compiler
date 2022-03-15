use derive_new::new;

use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::{ConverterBase, ConverterTest, TestConvertible};
use crate::converter::error::{CompilerException, CompilerTodoError};
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::{TypeObject, UserType};
use crate::converter::warning::WarningType;
use crate::converter::{warning, CompileBytes, CompileConstant, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator::OperatorTypeNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::first;

use super::{as_exception, default_constant};

#[derive(Debug, new)]
pub(super) struct EqualsConverter<'a> {
    equals_type: bool,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> EqualsConverter<'a> {
    pub fn constant_return(&self, info: &mut CompilerInfo) -> CompileConstant {
        default_constant(
            if self.equals_type {
                OperatorTypeNode::Equals
            } else {
                OperatorTypeNode::NotEquals
            },
            info,
            self.args,
        )
    }

    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().bool_type().clone()])
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!(
                    "'{}' returns 1 value, {} expected",
                    self.eq_str(),
                    self.ret_count
                ),
                &self.line_info,
            )
            .into());
        }
        if let Option::Some(constant) = self.constant_return(info)? {
            return Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())));
        }
        match self.args.len() {
            0 | 1 => self.convert_0(info),
            2 => self.convert_2(info),
            _ => Err(CompilerTodoError::of(
                "Cannot compute == for more than 2 operands",
                &self.line_info,
            )
            .into()),
        }
    }

    pub fn convert_with_as(&self) -> CompileResult<(BytecodeList, TypeObject)> {
        Err(as_exception(&self.line_info).into())
    }

    fn convert_0(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert!(self.args.is_empty() || self.args.len() == 1);
        warning::warn(
            format!(
                "'{}' with fewer than 2 operands will always evaluate to {}",
                self.eq_str(),
                self.equals_type
            ),
            WarningType::TrivialValue,
            info,
            &self.line_info,
        )?;
        let mut bytes = self.args.first().map_or_else(
            || Ok(BytecodeList::new()),
            |x| TestConverter::bytes(x.get_argument(), info, 1),
        )?;
        bytes.add(Bytecode::PopTop());
        bytes.add(Bytecode::LoadConst(self.equals_type.into()));
        Ok(bytes)
    }

    fn convert_2(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert_eq!(self.args.len(), 2);
        let mut converter = self.args[0].get_argument().test_converter(1);
        // TODO: Equals for types where info doesn't match
        let return_type = first(converter.return_type(info)?);
        let mut bytes = converter.convert(info)?;
        bytes.extend(TestConverter::bytes(self.args[1].get_argument(), info, 1)?);
        let use_id = match UserType::try_from(return_type) {
            Result::Ok(x) => x.operator_info(OpSpTypeNode::Equals, info)?.is_none(),
            Result::Err(_) => false,
        };
        bytes.add(if use_id {
            Bytecode::Identical()
        } else {
            Bytecode::Equal()
        });
        if !self.equals_type {
            bytes.add(Bytecode::BoolNot());
        }
        Ok(bytes)
    }

    fn eq_str(&self) -> &'static str {
        if self.equals_type {
            "=="
        } else {
            "!="
        }
    }
}
