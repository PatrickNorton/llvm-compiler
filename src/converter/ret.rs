use derive_new::new;

use crate::parser::return_stmt::ReturnStatementNode;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase};
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::if_converter::IfConverter;
use super::ret_list::{RetListBytecode, ReturnListConverter};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct ReturnConverter<'a> {
    node: &'a ReturnStatementNode,
}

impl<'a> ConverterBase for ReturnConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        let jump_pos = IfConverter::add_jump(&mut bytes, self.node.get_cond(), info)?;
        let ret_info = info.get_fn_returns();
        if ret_info.not_in_function() {
            return Err(CompilerException::of("Cannot return from here", self.node).into());
        } else if ret_info.is_generator() && !self.node.get_returned().is_empty() {
            return Err(CompilerException::of(
                "Return with arguments invalid in generator",
                self.node,
            )
            .into());
        }
        let fn_returns = ret_info.current_fn_returns();
        if fn_returns.is_empty() || ret_info.is_generator() {
            if !self.node.get_returned().is_empty() {
                return Err(CompilerException::of(
                    "Non-empty 'return' statement invalid in function with no return types",
                    self.node,
                )
                .into());
            } else {
                bytes.add(Bytecode::Return(0.into()));
            }
        } else {
            let mut ret_converter = ReturnListConverter::new(
                self.node.get_returned(),
                fn_returns.to_vec(),
                RetListBytecode::Return,
            );
            bytes.extend(ret_converter.convert(info)?);
        }
        if let Option::Some(jump_pos) = jump_pos {
            bytes.add_label(jump_pos)
        }
        Ok(bytes)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut diverging_info = DivergingInfo::new();
        if self.node.get_cond().is_empty() {
            diverging_info.known_return();
        } else {
            diverging_info.possible_return();
        }
        Ok((self.convert(info)?, diverging_info))
    }
}

base_convertible!(ReturnStatementNode, ReturnConverter);
