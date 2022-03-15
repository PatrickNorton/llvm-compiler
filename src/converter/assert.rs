use derive_new::new;

use crate::parser::assert::AssertStatementNode;
use crate::util::first;

use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::CompilerException;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct AssertConverter<'a> {
    node: &'a AssertStatementNode,
}

impl<'a> ConverterBase for AssertConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if info.global_info().is_debug() {
            let mut converter = self.node.get_assertion().test_converter(1);
            if let Option::Some(constant_ret) = converter.constant_return(info)? {
                if let Option::Some(constant) = constant_ret.bool_value() {
                    let mut bytes = BytecodeList::new();
                    if constant {
                        warning::warn(
                            "Value in assertion is always true",
                            WarningType::TrivialValue,
                            info,
                            self.node.get_assertion(),
                        )?;
                    } else {
                        bytes.add(Bytecode::LoadConst(
                            info.builtins().assert_error_const().clone().into(),
                        ));
                        self.convert_message(info, &mut bytes)?;
                        bytes.add(Bytecode::ThrowQuick(1.into()))
                    }
                    Ok(bytes)
                } else {
                    self.convert_standard(info, converter)
                }
            } else {
                self.convert_standard(info, converter)
            }
        } else {
            Ok(BytecodeList::new())
        }
    }
}

impl<'a> AssertConverter<'a> {
    fn convert_message(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
    ) -> CompileResult<()> {
        if self.node.get_as().is_empty() {
            bytes.add(Bytecode::LoadConst("Assertion failed".into()));
        } else {
            let mut converter = self.node.get_as().test_converter(1);
            let ret_type = first(converter.return_type(info)?);
            if !info.builtins().str_type().is_superclass(&ret_type) {
                return Err(CompilerException::of(
                    format!(
                        "'as' clause in an assert statement must return a 'str', not '{}'",
                        ret_type.name()
                    ),
                    self.node.get_as(),
                )
                .into());
            } else {
                bytes.extend(converter.convert(info)?)
            }
        }
        Ok(())
    }

    fn convert_standard(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileBytes {
        let mut bytes = converter.convert(info)?;
        let jump_tag = Label::new();
        bytes.add(Bytecode::JumpTrue(jump_tag.clone().into()));
        bytes.add(Bytecode::LoadConst(
            info.builtins().assert_error_const().clone().into(),
        ));
        self.convert_message(info, &mut bytes)?;
        bytes.add(Bytecode::ThrowQuick(1.into()));
        bytes.add_label(jump_tag);
        Ok(bytes)
    }
}

base_convertible!(AssertStatementNode, AssertConverter);
