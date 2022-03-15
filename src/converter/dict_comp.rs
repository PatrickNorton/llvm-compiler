use derive_new::new;

use crate::parser::comprehension::DictComprehensionNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::variable::VarLikeNode;
use crate::util::first;

use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::CompilerException;
use super::test_converter::TestConverter;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileTypes};

#[derive(Debug, new)]
pub struct DictCompConverter<'a> {
    node: &'a DictComprehensionNode,
    ret_count: u16,
}

impl<'a> ConverterTest for DictCompConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        match &self.node.get_variables()[0] {
            VarLikeNode::Typed(typed_var) => {
                info.add_stack_frame();
                let converted_type = info.convert_type(typed_var.get_type().as_type())?;
                info.add_variable(
                    typed_var.get_variable().get_name().to_string(),
                    converted_type,
                    false,
                    typed_var.line_info().clone(),
                );
                let key_type = first(TestConverter::return_type(info, self.node.get_key(), 1)?);
                let val_type = first(TestConverter::return_type(
                    info,
                    self.node.get_builder(),
                    1,
                )?);
                info.remove_stack_frame();
                Ok(vec![info
                    .builtins()
                    .dict_type()
                    .generify(&LineInfo::empty(), vec![key_type, val_type])?])
            }
            VarLikeNode::Variable(_) => {
                let key_type = first(TestConverter::return_type(info, self.node.get_key(), 1)?);
                let val_type = first(TestConverter::return_type(
                    info,
                    self.node.get_builder(),
                    1,
                )?);
                info.remove_stack_frame();
                Ok(vec![info
                    .builtins()
                    .dict_type()
                    .generify(&LineInfo::empty(), vec![key_type, val_type])?])
            }
        }
    }
}

impl<'a> ConverterBase for DictCompConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            warning::warn(
                "Unnecessary dict creation",
                WarningType::Unused,
                info,
                self.node,
            )?;
        } else if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!(
                    "Dict comprehension only returns one value, got {}",
                    self.ret_count
                ),
                self.node,
            )
            .into());
        }
        let mut bytes = BytecodeList::new();
        bytes.add(Bytecode::DictCreate(0.into()));
        bytes.add(Bytecode::LoadConst(
            info.builtins().iter_constant().clone().into(),
        ));
        bytes.extend(TestConverter::bytes(&self.node.get_looped()[0], info, 1)?);
        bytes.add(Bytecode::CallTos(1.into()));
        let top_jump = Label::new();
        bytes.add_label(top_jump.clone());
        let for_jump = Label::new();
        bytes.add(Bytecode::ForIter(for_jump.clone().into(), 1.into()));
        // Add the variable for the loop
        let variable = &self.node.get_variables()[0];
        info.add_stack_frame();
        if let VarLikeNode::Typed(typed_var) = variable {
            info.check_definition(typed_var.get_name(), typed_var)?;
            let var_type = info.convert_type(typed_var.get_type().as_type())?;
            info.add_variable(
                typed_var.get_name().to_string(),
                var_type,
                false,
                typed_var.line_info().clone(),
            );
        }
        bytes.add(Bytecode::Store(
            info.var_index(variable.get_variable())?.into(),
        ));
        if !self.node.get_condition().is_empty() {
            bytes.extend(TestConverter::bytes(self.node.get_condition(), info, 1)?);
            bytes.add(Bytecode::JumpFalse(top_jump.clone().into()));
        }
        bytes.add(Bytecode::Swap2()); // The iterator object will be atop the list, swap it and back again
        bytes.extend(TestConverter::bytes(self.node.get_key(), info, 1)?);
        bytes.extend(TestConverter::bytes(self.node.get_builder(), info, 1)?);
        bytes.add(Bytecode::DictAdd());
        bytes.add(Bytecode::Swap2());
        bytes.add(Bytecode::Jump(top_jump.into()));
        bytes.add_label(for_jump);
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        info.remove_local_types();
        Ok(bytes)
    }
}

test_convertible!(DictComprehensionNode, DictCompConverter);
