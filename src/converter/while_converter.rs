use derive_new::new;

use crate::parser::line_info::Lined;
use crate::parser::operator::OperatorNode;
use crate::parser::variable::VariableNode;
use crate::parser::while_stmt::WhileStatementNode;

use super::base_converter::BaseConverter;
use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::loop_converter::LoopConverter;
use super::operator::OperatorConverter;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct WhileConverter<'a> {
    node: &'a WhileStatementNode,
}

impl<'a> LoopConverter for WhileConverter<'a> {
    const HAS_CONTINUE: bool = true;

    fn true_convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.true_convert_with_return(info).map(|x| x.0)
    }

    fn true_convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut bytes = BytecodeList::new();
        let has_as = !self.node.get_as().is_empty();
        bytes.add_label(info.loop_manager().continue_label().clone());
        let (is_while_true, constant_cond) = self.convert_cond(&mut bytes, info, has_as)?;
        let jump_label = Label::new();
        if let Option::Some(constant_cond) = constant_cond {
            if !constant_cond {
                bytes.add(Bytecode::Jump(jump_label.clone().into()))
            }
        } else {
            bytes.add(Bytecode::JumpFalse(jump_label.clone().into()))
        }
        if has_as {
            info.add_stack_frame();
            bytes.add(Bytecode::Store(info.var_index(self.node.get_as())?.into()));
        }
        let (body, mut will_return) = BaseConverter::bytes_with_return(self.node.get_body(), info)?;
        if (will_return.will_break() || will_return.will_return()) && !will_return.may_continue() {
            let msg = if is_while_true {
                "exactly"
            } else {
                "no more than"
            };
            warning::warn(
                format!("Loop executes {} once", msg),
                WarningType::Unreachable,
                info,
                self.node,
            )?;
        }
        bytes.extend(body);
        if has_as {
            info.remove_stack_frame();
        }
        bytes.add(Bytecode::Jump(
            info.loop_manager().continue_label().clone().into(),
        ));
        if !self.node.get_nobreak().is_empty() {
            if is_while_true {
                warning::warn(
                    "'nobreak' statement in a 'while true' loop is unreachable",
                    WarningType::Unreachable,
                    info,
                    self.node.get_nobreak(),
                )?;
            }
            let nobreak_returns = self.add_nobreak(
                info,
                &mut bytes,
                jump_label,
                !constant_cond.unwrap_or(false),
            )?;
            if !is_while_true {
                will_return.and_with(nobreak_returns);
            }
        } else if has_as {
            will_return.make_uncertain(); // 'while true' cannot have an 'as' clause
            if !constant_cond.unwrap_or(false) {
                bytes.add_label(jump_label);
            }
            bytes.add(Bytecode::PopTop());
        } else if !is_while_true {
            will_return.make_uncertain();
            if !constant_cond.unwrap_or(false) {
                bytes.add_label(jump_label);
            }
        }
        if is_while_true && !will_return.may_break() {
            if !will_return.may_return() && !info.get_fn_returns().is_generator() {
                // Generators may infinitely yield, so no warnings for them
                // In the future, we may want to keep track of yields too, so
                // we can warn on yield-less infinite loops
                warning::warn("Infinite loop", WarningType::InfiniteLoop, info, self.node)?;
            } else if will_return.may_return() {
                will_return.known_return();
            }
        }
        Ok((bytes, will_return))
    }
}

impl<'a> WhileConverter<'a> {
    fn convert_cond(
        &self,
        bytes: &mut BytecodeList,
        info: &mut CompilerInfo,
        has_as: bool,
    ) -> CompileResult<(bool, Option<bool>)> {
        if !has_as {
            // TODO:
            if let Result::Ok(var) = <&VariableNode>::try_from(self.node.get_cond()) {
                if var.get_name() == "true" {
                    return Ok((true, Some(true)));
                }
            }
            let mut converter = self.node.get_cond().test_converter(1);
            if let Option::Some(constant) = constant_bool(&mut converter, info)? {
                warning::warn(
                    format!("While loop condition always evaluates to {}", constant),
                    WarningType::TrivialValue,
                    info,
                    self.node.get_cond(),
                )?;
                Ok((false, Some(constant)))
            } else {
                bytes.extend(converter.convert(info)?);
                Ok((false, None))
            }
        } else {
            self.convert_cond_with_as(info, bytes)?;
            Ok((false, None))
        }
    }

    fn convert_cond_with_as(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
    ) -> CompileResult<()> {
        let condition = <&OperatorNode>::try_from(self.node.get_cond()).map_err(|_| {
            CompilerException::of(
                "Cannot use 'as' here: condition must be 'instanceof' or 'is not null'",
                self.node.get_cond(),
            )
        })?;
        let (bytecode, as_type) = OperatorConverter::convert_with_as(condition, info, 1)?;
        info.add_stack_frame();
        info.add_variable(
            self.node.get_as().get_name().to_string(),
            as_type,
            false,
            self.node.get_as().line_info().clone(),
        );
        bytes.extend(bytecode);
        Ok(())
    }

    fn add_nobreak(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        jump_label: Label,
        emplace_jump: bool,
    ) -> CompileResult<DivergingInfo> {
        let (bytecode, ret_info) = BaseConverter::bytes_with_return(self.node.get_nobreak(), info)?;
        bytes.extend(bytecode);
        if !self.node.get_as().is_empty() {
            let label = Label::new();
            bytes.add(Bytecode::Jump(label.clone().into()));
            if emplace_jump {
                bytes.add_label(jump_label);
            }
            bytes.add(Bytecode::PopTop());
            bytes.add_label(label);
        }
        Ok(ret_info)
    }
}

fn constant_bool(
    converter: &mut impl ConverterTest,
    info: &mut CompilerInfo,
) -> CompileResult<Option<bool>> {
    Ok(converter
        .constant_return(info)?
        .and_then(|x| x.bool_value()))
}

base_convertible!(WhileStatementNode, WhileConverter);
