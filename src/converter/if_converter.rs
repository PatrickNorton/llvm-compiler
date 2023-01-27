use derive_new::new;

use crate::parser::if_stmt::{ElifStatementNode, IfStatementNode};
use crate::parser::line_info::Lined;
use crate::parser::operator::{OperatorNode, OperatorTypeNode};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;

use super::base_converter::BaseConverter;
use super::bytecode::{Bytecode, Label, VariableBytecode};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::convertible::{base_convertible, ConverterBase};
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::operator::OperatorConverter;
use super::test_converter::TestConverter;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct IfConverter<'a> {
    node: &'a IfStatementNode,
}

impl<'a> ConverterBase for IfConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_with_return(info).map(|x| x.0)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let has_as = !self.node.get_as().is_empty();
        let has_else = !self.node.get_else().is_empty();
        self.check_conditions(info)?;
        let end_label = Label::new();
        let mut bytes;
        let mut will_return;
        if has_as {
            bytes = self.add_as(info, self.node.get_conditional(), self.node.get_as())?;
            will_return = self.add_body_with_as(
                info,
                &mut bytes,
                self.node.get_body(),
                self.node.get_as(),
                end_label.clone(),
            )?;
            info.remove_stack_frame();
        } else {
            let (start_bytes, jump_type) =
                self.convert_optimized_not(info, self.node.get_conditional())?;
            bytes = start_bytes;
            will_return = self.add_body(
                info,
                &mut bytes,
                self.node.get_body(),
                jump_type,
                end_label.clone(),
            )?;
        }
        for elif in self.node.get_elifs() {
            will_return.and_with(self.add_elif(info, &mut bytes, elif, end_label.clone())?);
        }
        if has_else {
            will_return.and_with(self.add_else(info, &mut bytes, self.node.get_else())?);
        } else {
            will_return.make_uncertain();
        }
        bytes.add_label(end_label);
        Ok((bytes, will_return))
    }
}

impl<'a> IfConverter<'a> {
    fn add_body(
        &mut self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        body: &StatementBodyNode,
        jump_type: bool,
        end_label: Label,
    ) -> CompileResult<DivergingInfo> {
        let (body_bytes, diverging_info) = BaseConverter::bytes_with_return(body, info)?;
        let jump_target = Label::new();
        bytes.add(Bytecode::jump_if(jump_type, jump_target.clone()));
        bytes.extend(body_bytes);
        bytes.add(Bytecode::Jump(end_label.into()));
        bytes.add_label(jump_target);
        Ok(diverging_info)
    }

    fn add_body_with_as(
        &mut self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        body: &StatementBodyNode,
        as_name: &VariableNode,
        end_label: Label,
    ) -> CompileResult<DivergingInfo> {
        let jump_label = Label::new();
        bytes.add(Bytecode::JumpFalse(jump_label.clone().into()));
        bytes.add(Bytecode::Store(VariableBytecode::new(
            info.var_index(as_name)?,
        )));
        let (bytecode, diverging) = BaseConverter::bytes_with_return(body, info)?;
        bytes.extend(bytecode);
        bytes.add(Bytecode::Jump(end_label.into()));
        bytes.add_label(jump_label);
        Ok(diverging)
    }

    fn add_elif(
        &mut self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        elif: &ElifStatementNode,
        end_label: Label,
    ) -> CompileResult<DivergingInfo> {
        let cond = elif.get_test();
        let body = elif.get_body();
        let will_return;
        if elif.get_as().is_empty() {
            let (bytecode, jump_type) = self.convert_optimized_not(info, cond)?;
            bytes.extend(bytecode);
            will_return = self.add_body(info, bytes, body, jump_type, end_label)?;
        } else {
            bytes.extend(self.add_as(info, cond, elif.get_as())?);
            will_return = self.add_body_with_as(info, bytes, body, elif.get_as(), end_label)?;
            info.remove_stack_frame();
        }
        Ok(will_return)
    }

    fn add_else(
        &mut self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        body: &StatementBodyNode,
    ) -> CompileResult<DivergingInfo> {
        let (bytecode, divergence) = BaseConverter::bytes_with_return(body, info)?;
        bytes.extend(bytecode);
        Ok(divergence)
    }

    fn add_as(
        &mut self,
        info: &mut CompilerInfo,
        condition: &TestNode,
        as_var: &VariableNode,
    ) -> CompileBytes {
        let condition = match condition.try_into() {
            Result::Ok(cond) => cond,
            Result::Err(_) => {
                return Err(CompilerException::of(
                    "Cannot use 'as' here: condition must be 'instanceof' or 'is not null'",
                    condition,
                )
                .into())
            }
        };
        let (bytes, as_type) = OperatorConverter::convert_with_as(condition, info, 1)?;
        info.add_stack_frame();
        info.check_definition(as_var.get_name(), as_var)?;
        info.add_variable(
            as_var.get_name().to_string(),
            as_type,
            false,
            as_var.line_info().clone(),
        );
        Ok(bytes)
    }

    fn check_conditions(&mut self, info: &mut CompilerInfo) -> CompileResult<()> {
        // TODO: Use this information to actually change emitted bytecode
        let first_const = TestConverter::constant_return(self.node.get_conditional(), info, 1)?;
        if let Option::Some(constant) = first_const {
            self.check_cond(info, &constant, self.node.get_conditional())?;
        }
        for elif in self.node.get_elifs() {
            let constant = TestConverter::constant_return(elif.get_test(), info, 1)?;
            if let Option::Some(c) = constant {
                self.check_cond(info, &c, self.node.get_conditional())?;
            }
        }
        Ok(())
    }

    fn check_cond(
        &self,
        info: &CompilerInfo,
        value: &LangConstant,
        node: &dyn Lined,
    ) -> CompileResult<()> {
        if let Option::Some(bool_val) = value.bool_value() {
            warning::warn(
                format!("Statement in conditional will always evaluate to {bool_val}"),
                WarningType::Unreachable,
                info,
                node,
            )
        } else {
            Ok(())
        }
    }

    fn convert_optimized_not(
        &mut self,
        info: &mut CompilerInfo,
        cond: &TestNode,
    ) -> CompileResult<(BytecodeList, bool)> {
        if let Result::Ok(op) = <&OperatorNode>::try_from(cond) {
            if op.get_operator() == OperatorTypeNode::BoolNot {
                if op.get_operands().len() != 1 {
                    return Err(CompilerException::of(
                        format!(
                            "'not' statement expected one operand, got {}",
                            op.get_operands().len()
                        ),
                        op,
                    )
                    .into());
                } else {
                    return TestConverter::bytes(op.get_operands()[0].get_argument(), info, 1)
                        .map(|x| (x, true));
                }
            }
        }
        TestConverter::bytes(cond, info, 1).map(|x| (x, false))
    }

    pub fn add_jump(
        bytes: &mut BytecodeList,
        cond: &TestNode,
        info: &mut CompilerInfo,
    ) -> CompileResult<Option<Label>> {
        if !cond.is_empty() {
            bytes.extend(TestConverter::bytes(cond, info, 1)?);
            let label = Label::new();
            bytes.add(Bytecode::JumpFalse(label.clone().into()));
            Ok(Some(label))
        } else {
            Ok(None)
        }
    }
}

base_convertible!(IfStatementNode, IfConverter);
