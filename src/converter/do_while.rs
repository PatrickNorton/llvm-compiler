use derive_new::new;

use crate::parser::do_stmt::DoStatementNode;

use super::base_converter::BaseConverter;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::loop_converter::LoopConverter;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct DoWhileConverter<'a> {
    node: &'a DoStatementNode,
}

impl<'a> LoopConverter for DoWhileConverter<'a> {
    const HAS_CONTINUE: bool = true;

    fn true_convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.true_convert_with_return(info).map(|x| x.0)
    }

    fn true_convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let label = info.loop_manager().continue_label().clone();
        let (mut bytes, diverging_info) = self.convert_body(info)?;
        bytes.add_label(label.clone());
        bytes.extend(self.convert_cond(info)?);
        bytes.add(Bytecode::JumpTrue(label.into()));
        Ok((bytes, diverging_info))
    }
}

impl<'a> DoWhileConverter<'a> {
    fn convert_body(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let (bytes, diverging_info) = BaseConverter::bytes_with_return(self.node.get_body(), info)?;
        if (diverging_info.will_break() || diverging_info.will_return())
            && diverging_info.may_continue()
        {
            warning::warn(
                "Loop executes exactly once",
                WarningType::Unreachable,
                info,
                self.node,
            )?;
        }
        Ok((bytes, diverging_info))
    }

    fn convert_cond(&self, info: &mut CompilerInfo) -> CompileBytes {
        let mut converter = self.node.get_cond().test_converter(1);
        let constant_return = converter.constant_return(info)?;
        if let Option::Some(constant_bool) = constant_return.and_then(|x| x.bool_value()) {
            if constant_bool {
                warning::warn(
                    "'do-while' loop with always-true parameter is \
                     equivalent to a 'while true' loop",
                    WarningType::TrivialValue,
                    info,
                    self.node.get_cond(),
                )?;
            } else {
                warning::warn(
                    "'do-while' loop with always-false parameter will execute exactly once",
                    WarningType::TrivialValue,
                    info,
                    self.node.get_cond(),
                )?;
            }
        }
        converter.convert(info)
    }
}

base_convertible!(DoStatementNode, DoWhileConverter);
