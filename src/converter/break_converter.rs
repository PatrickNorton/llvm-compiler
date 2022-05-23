use derive_new::new;

use crate::parser::break_stmt::BreakStatementNode;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::error::CompilerInternalError;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct BreakConverter<'a> {
    node: &'a BreakStatementNode,
}

impl<'a> ConverterBase for BreakConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_with_return(info).map(|x| x.0)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        if !self.node.get_as().is_empty() {
            return Err(CompilerInternalError::of(
                "'break as' statements are not supported yet",
                self.node.get_as(),
            )
            .into());
        }
        let mut bytes = BytecodeList::new();
        let mut diverging_info = DivergingInfo::new();
        let levels = self.node.get_loops();
        if !self.node.get_cond().is_empty() {
            let mut cond_converter = self.node.get_cond().test_converter(1);
            if let Option::Some(constant) = cond_converter
                .constant_return(info)?
                .and_then(|x| x.bool_value())
            {
                if constant {
                    warning::warn(
                        "'break' condition is always true\n\
                         Note: 'break if true' is equivalent to 'break'",
                        WarningType::TrivialValue,
                        info,
                        self.node,
                    )?;
                    bytes.add(Bytecode::Jump(
                        info.loop_manager().break_label(levels).clone().into(),
                    ));
                } else {
                    warning::warn(
                        "'break' condition is always false\n\
                         Note: 'break if false' will never be taken and can be removed",
                        WarningType::TrivialValue,
                        info,
                        self.node,
                    )?;
                }
            } else {
                bytes.extend(cond_converter.convert(info)?);
                bytes.add(Bytecode::JumpTrue(
                    info.loop_manager().break_label(levels).clone().into(),
                ));
            }
            diverging_info.possible_break(levels);
        } else {
            bytes.add(Bytecode::Jump(
                info.loop_manager().break_label(levels).clone().into(),
            ));
            diverging_info.known_break(levels);
        }
        Ok((bytes, diverging_info))
    }
}

base_convertible!(BreakStatementNode, BreakConverter);
