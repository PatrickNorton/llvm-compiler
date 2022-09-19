use derive_new::new;

use crate::parser::variable::VariableNode;
use crate::util::levenshtein;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::CompilerException;
use super::error_builder::ErrorBuilder;
use super::warning::WarningType;
use super::{warning, CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct VariableConverter<'a> {
    node: &'a VariableNode,
    ret_count: u16,
}

impl<'a> ConverterBase for VariableConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            self.check_def(info)?;
            warning::warn(
                format!("Unused variable {}", self.node.get_name()),
                WarningType::Unused,
                info,
                self.node,
            )?;
            Ok(BytecodeList::new())
        } else if self.ret_count > 1 {
            self.check_def(info)?;
            Err(CompilerException::of(
                format!("Variable only returns 1 value, expected {}", self.ret_count),
                self.node,
            )
            .into())
        } else {
            let name = self.node.get_name();
            if name == "null" {
                return Ok(BytecodeList::of(Bytecode::LoadNull()));
            }
            self.check_def(info)?;
            Ok(BytecodeList::of(if info.variable_is_static(name) {
                let index = info.static_var_index(self.node)?;
                Bytecode::LoadStatic(index.into())
            } else if let Option::Some(constant) = info.get_constant(name) {
                Bytecode::LoadConst(constant.into_owned().into())
            } else {
                Bytecode::LoadValue(info.var_index(self.node)?.into())
            }))
        }
    }
}

impl<'a> ConverterTest for VariableConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        info.get_type(self.node.get_name())
            .map(|x| vec![x.into_owned()])
            .ok_or_else(|| self.name_error(info).into())
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        Ok(info
            .get_constant(self.node.get_name())
            .map(|x| x.into_owned()))
    }
}

impl<'a> VariableConverter<'a> {
    fn check_def(&self, info: &mut CompilerInfo) -> CompileResult<()> {
        if self.node.get_name() != "null" && info.var_is_undefined(self.node.get_name()) {
            Err(self.name_error(info).into())
        } else {
            Ok(())
        }
    }

    fn name_error(&self, info: &CompilerInfo) -> CompilerException {
        let name = self.node.get_name();
        CompilerException::from_builder(
            ErrorBuilder::new(self.node)
                .with_message(format!("Variable '{}' not defined", name))
                .when_some(
                    levenshtein::closest_name(name, info.defined_names()),
                    |builder, closest| builder.with_help(format!("Did you mean '{}'?", closest)),
                ),
        )
    }
}

test_convertible!(VariableNode, VariableConverter);
