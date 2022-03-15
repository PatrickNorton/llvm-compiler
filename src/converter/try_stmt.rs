use derive_new::new;

use crate::parser::line_info::Lined;
use crate::parser::try_stmt::TryStatementNode;

use super::base_converter::BaseConverter;
use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase};
use super::error::CompilerTodoError;
use super::test_converter::TestConverter;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct TryConverter<'a> {
    node: &'a TryStatementNode,
}

impl<'a> ConverterBase for TryConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        let jump_0 = Label::new();
        bytes.add(Bytecode::EnterTry(jump_0.clone().into()));
        bytes.extend(BaseConverter::bytes(self.node.get_body(), info)?);
        let jump_1 = Label::new();
        bytes.add(Bytecode::Jump(jump_1.clone().into()));
        bytes.add_label(jump_0);
        for except in self.node.get_excepted() {
            warning::warn(
                "Exceptions are very broken right now",
                WarningType::Todo,
                info,
                except,
            )?;
            bytes.extend(TestConverter::bytes(except, info, 1)?);
        }
        let excepted_len = u16::try_from(self.node.get_excepted().len()).unwrap();
        bytes.add(Bytecode::ExceptN(excepted_len.into()));
        if !self.node.get_as().is_empty() {
            let as_var = self.node.get_as();
            let var_index = info.add_variable(
                as_var.get_name().to_string(),
                TypeObject::union_of(info, info.types_of(self.node.get_excepted())?),
                true,
                as_var.line_info().clone(),
            );
            bytes.add(Bytecode::Store(var_index.into()))
        } else {
            bytes.add(Bytecode::PopTop());
        }
        bytes.extend(BaseConverter::bytes(self.node.get_except(), info)?);
        bytes.add(Bytecode::Jump(jump_1.clone().into()));
        if !self.node.get_finally().is_empty() {
            self.convert_finally(info, &mut bytes)?;
        }
        bytes.add_label(jump_1);
        bytes.add(Bytecode::EndTry(excepted_len.into()));
        Ok(bytes)
    }
}

impl<'a> TryConverter<'a> {
    fn convert_finally(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
    ) -> CompileResult<()> {
        debug_assert!(!self.node.get_finally().is_empty());
        let jump_3 = if !self.node.get_excepted().is_empty() {
            let x = Label::new();
            bytes.add(Bytecode::Jump(x.clone().into()));
            Some(x)
        } else {
            None
        };
        bytes.add(Bytecode::Finally());
        let (bytecode, diverging) =
            BaseConverter::bytes_with_return(self.node.get_finally(), info)?;
        bytes.extend(bytecode);
        if diverging.may_diverge() {
            warning::warn(
                "'return', 'break', or 'continue' statements in a \
                 finally' statement can cause unexpected behavior",
                WarningType::NoType,
                info,
                self.node.get_finally(),
            )?;
        }
        if let Option::Some(jump_3) = jump_3 {
            bytes.add_label(jump_3)
        }
        // Work out some kinks first
        Err(CompilerTodoError::of("Finally not implemented yet", self.node.get_finally()).into())
    }
}

base_convertible!(TryStatementNode, TryConverter);
