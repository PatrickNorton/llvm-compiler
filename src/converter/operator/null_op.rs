use derive_new::new;

use crate::converter::bytecode::{Bytecode, Label};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::{ConverterBase, ConverterTest, TestConvertible};
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::{OptionTypeObject, TypeObject};
use crate::converter::warning::WarningType;
use crate::converter::{warning, CompileBytes, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator::OperatorTypeNode;
use crate::util::first;

use super::as_exception;

#[derive(Debug, new)]
pub(super) struct NullOpConverter<'a> {
    op: OperatorTypeNode,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> NullOpConverter<'a> {
    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        match self.op {
            OperatorTypeNode::NullCoerce => self.null_coerce_return(info),
            OperatorTypeNode::NotNull => self.not_null_return(info),
            OperatorTypeNode::Optional => Ok(vec![info.builtins().bool_type().clone()]),
            _ => panic!(
                "Invalid operand for null-op converter: {}",
                self.op.sequence()
            ),
        }
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        match self.op {
            OperatorTypeNode::NullCoerce => self.convert_null_coerce(info),
            OperatorTypeNode::NotNull => self.convert_not_null(info),
            OperatorTypeNode::Optional => self.convert_question(info),
            _ => panic!(
                "Invalid operand for null-op converter: {}",
                self.op.sequence()
            ),
        }
    }

    pub fn convert_with_as(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, TypeObject)> {
        if self.op == OperatorTypeNode::Optional {
            self.convert_question_as(info)
        } else {
            Err(as_exception(&self.line_info).into())
        }
    }

    fn convert_null_coerce(&self, info: &mut CompilerInfo) -> CompileBytes {
        let ret_type = first(TestConverter::return_type(
            info,
            self.args[0].get_argument(),
            1,
        )?);
        if OptionTypeObject::try_from(ret_type.clone()).is_err() {
            warning::warn_note(
                "Using ?? operator on non-optional value",
                "This statement can be replaced with the first operand",
                WarningType::TrivialValue,
                info,
                &self.args[0],
            )?;
            TestConverter::bytes(self.args[0].get_argument(), info, 1)
        } else if ret_type == *info.builtins().null_type() {
            warning::warn_note(
                "Using ?? operator on value that is always null",
                "This statement can be replaced with the second operand",
                WarningType::TrivialValue,
                info,
                &self.args[0],
            )?;
            TestConverter::bytes(self.args[1].get_argument(), info, 1)
        } else {
            let mut bytes = TestConverter::bytes(self.args[0].get_argument(), info, 1)?;
            bytes.add(Bytecode::DupTop());
            bytes.add(Bytecode::UnwrapOption());
            bytes.add(Bytecode::Swap2());
            let post_label = Label::new();
            bytes.add(Bytecode::JumpNN(post_label.clone().into()));
            self.add_post_jump(info, &mut bytes, post_label)?;
            if self.ret_count == 0 {
                bytes.add(Bytecode::PopTop());
            }
            Ok(bytes)
        }
    }

    fn convert_not_null(&self, info: &mut CompilerInfo) -> CompileBytes {
        debug_assert_eq!(self.op, OperatorTypeNode::NotNull);
        let mut converter = self.args[0].get_argument().test_converter(1);
        let mut bytes = converter.convert(info)?;
        let ret_type = first(converter.return_type(info)?);
        if &ret_type == info.builtins().null_type() {
            return Err(CompilerException::of(
                "Cannot use !! operator on variable with type null",
                &self.args[0],
            )
            .into());
        } else if ret_type.is_option() {
            unwrap_option(info, &mut bytes, ""); // FIXME: self.args[0].to_string()
        } else {
            warning::warn_note(
                "Used !! operator on non-optional value",
                "Since this cannot be null, this statement does nothing",
                WarningType::TrivialValue,
                info,
                &self.args[0],
            )?;
        }
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }

    fn convert_question(&self, info: &mut CompilerInfo) -> CompileBytes {
        let mut converter = self.args[0].get_argument().test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        if !ret_type.is_option() {
            return Err(CompilerException::of(
                format!("Cannot use ? on non-optional type '{}'", ret_type.name()),
                &self.args[0],
            )
            .into());
        }
        let mut bytes = converter.convert(info)?;
        bytes.add(Bytecode::IsSome());
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }

    fn convert_question_as(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, TypeObject)> {
        let mut converter = self.args[0].get_argument().test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        let result_type = match OptionTypeObject::try_from(ret_type) {
            Result::Ok(x) => x.get_option_val().clone(),
            Result::Err(ret) => {
                return Err(CompilerException::of(
                    format!("Cannot use ? on a non-optional type '{}'", ret.name()),
                    &self.args[0],
                )
                .into())
            }
        };
        let mut bytes = converter.convert(info)?;
        bytes.add(Bytecode::DupTop());
        bytes.add(Bytecode::UnwrapOption());
        bytes.add(Bytecode::Swap2());
        bytes.add(Bytecode::IsSome());
        Ok((bytes, result_type))
    }

    fn null_coerce_return(&self, info: &mut CompilerInfo) -> CompileTypes {
        let ret0 = first(TestConverter::return_type(
            info,
            self.args[0].get_argument(),
            1,
        )?);
        let ret1 = first(TestConverter::return_type(
            info,
            self.args[1].get_argument(),
            1,
        )?);
        Ok(vec![if &ret0 == info.builtins().null_type() {
            ret1
        } else {
            TypeObject::union(info.builtins(), [ret0.strip_null(), ret1])
        }])
    }

    fn not_null_return(&self, info: &mut CompilerInfo) -> CompileTypes {
        let ret_type = first(TestConverter::return_type(
            info,
            self.args[0].get_argument(),
            1,
        )?);
        let builtins = info.builtins();
        if &ret_type == builtins.null_type() {
            // Doesn't particularly matter what, it'll fail later
            Ok(vec![builtins.throws_type().clone()])
        } else {
            Ok(vec![ret_type.strip_null()])
        }
    }

    fn add_post_jump(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        label: Label,
    ) -> CompileResult<()> {
        bytes.add(Bytecode::PopTop());
        bytes.extend(TestConverter::bytes(self.args[1].get_argument(), info, 1)?);
        bytes.add_label(label);
        Ok(())
    }
}

pub(super) fn unwrap_option(info: &mut CompilerInfo, bytes: &mut BytecodeList, value: &str) {
    bytes.add(Bytecode::DupTop());
    let jump = Label::new();
    bytes.add(Bytecode::JumpNN(jump.clone().into()));
    bytes.add(Bytecode::PopTop());
    let error = info.builtins().null_error_const().clone();
    bytes.add(Bytecode::LoadConst(error.into()));
    let message = format!("Value {value} asserted non-null, was null");
    bytes.add(Bytecode::LoadConst(message.into()));
    bytes.add(Bytecode::ThrowQuick(1.into()));
    bytes.add_label(jump);
    bytes.add(Bytecode::UnwrapOption());
}
