use derive_new::new;

use crate::parser::continue_stmt::ContinueStatementNode;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct ContinueConverter<'a> {
    node: &'a ContinueStatementNode,
}

impl<'a> ConverterBase for ContinueConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_with_return(info).map(|x| x.0)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut bytes = BytecodeList::new();
        let mut diverging_info = DivergingInfo::new();
        if !self.node.get_cond().is_empty() {
            let mut cond_converter = self.node.get_cond().test_converter(1);
            if let Option::Some(value) = cond_converter
                .constant_return(info)?
                .and_then(|x| x.bool_value())
            {
                if value {
                    warning::warn_note(
                        "'continue' condition is always true",
                        "'continue if true' is equivalent to 'continue'",
                        WarningType::TrivialValue,
                        info,
                        self.node,
                    )?;
                    bytes.add(Bytecode::Jump(
                        info.loop_manager().continue_label().clone().into(),
                    ));
                } else {
                    warning::warn_note(
                        "'continue' condition is always false",
                        "'continue if false' will never be taken and can be removed'",
                        WarningType::TrivialValue,
                        info,
                        self.node,
                    )?;
                }
            } else {
                bytes.extend(cond_converter.convert(info)?);
                bytes.add(Bytecode::JumpTrue(
                    info.loop_manager().continue_label().clone().into(),
                ))
            }
            diverging_info.possible_continue();
        } else {
            bytes.add(Bytecode::Jump(
                info.loop_manager().continue_label().clone().into(),
            ));
            diverging_info.known_continue();
        }
        Ok((bytes, diverging_info))
    }
}

base_convertible!(ContinueStatementNode, ContinueConverter);
