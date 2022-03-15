use std::iter::zip;

use derive_new::new;

use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::yield_stmt::YieldStatementNode;
use crate::util::first;

use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::CompilerException;
use super::if_converter::IfConverter;
use super::ret_list::{RetListBytecode, ReturnListConverter};
use super::type_obj::{OptionTypeObject, TypeObject};
use super::{for_converter, CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct YieldConverter<'a> {
    node: &'a YieldStatementNode,
}

impl<'a> ConverterBase for YieldConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        let jump_lbl = IfConverter::add_jump(&mut bytes, self.node.get_cond(), info)?;
        if self.node.get_yielded().is_empty() {
            return Err(
                CompilerException::of("Empty yield statements are illegal", self.node).into(),
            );
        } else if self.node.is_from() {
            self.convert_from(info, &mut bytes)?;
        } else {
            let ret_info = info.get_fn_returns();
            if !ret_info.is_generator() {
                return Err(self.no_generator_error().into());
            }
            let fn_returns = ret_info.current_fn_returns().to_vec();
            let mut converter = ReturnListConverter::new(
                self.node.get_yielded(),
                fn_returns,
                RetListBytecode::Yield,
            );
            bytes.extend(converter.convert(info)?);
        }
        if let Option::Some(jump_lbl) = jump_lbl {
            bytes.add_label(jump_lbl)
        }
        Ok(bytes)
    }
}

impl<'a> YieldConverter<'a> {
    fn convert_from(&self, info: &mut CompilerInfo, bytes: &mut BytecodeList) -> CompileResult<()> {
        debug_assert!(self.node.is_from());
        let ret_info = info.get_fn_returns();
        if !ret_info.is_generator() {
            return Err(self.no_generator_error().into());
        }
        let mut converter =
            self.node.get_yielded()[0].test_converter(ret_info.current_fn_returns().len() as u16);
        let raw_ret = first(
            first(converter.return_type(info)?).try_operator_return_type(
                self.node,
                OpSpTypeNode::Iter,
                info,
            )?,
        );
        let ret_types = info.builtins().de_iterable(&raw_ret)?;
        self.check_return_type(info.get_fn_returns().current_fn_returns(), &ret_types)?;
        for_converter::add_iter(info, bytes, &mut converter)?;
        let top_label = Label::new();
        bytes.add_label(top_label.clone());
        let label = Label::new();
        bytes.add(Bytecode::ForIter(label.clone().into(), 1.into()));
        bytes.add(Bytecode::Yield(1.into()));
        bytes.add(Bytecode::Jump(top_label.into()));
        bytes.add_label(label);
        Ok(())
    }

    fn check_return_type(
        &self,
        expected: &[TypeObject],
        gotten: &[TypeObject],
    ) -> CompileResult<()> {
        if expected.len() > gotten.len() {
            Err(CompilerException::of(
                format!(
                    "Mismatched types: function yields {} items, yield statement gave {}",
                    expected.len(),
                    gotten.len()
                ),
                self.node,
            )
            .into())
        } else {
            for (i, (fn_return, ret_type)) in zip(expected, gotten).enumerate() {
                if !fn_return.is_superclass(ret_type)
                    && !OptionTypeObject::needs_and_super(fn_return, ret_type)
                {
                    return Err(CompilerException::of(
                        format!(
                            "Type mismatch: in position {}, function expected \
                         a superclass of type '{}' to be yielded, got type '{}'",
                            i,
                            fn_return.name(),
                            ret_type.name()
                        ),
                        self.node,
                    )
                    .into());
                }
            }
            Ok(())
        }
    }

    fn no_generator_error(&self) -> CompilerException {
        CompilerException::of("'yield' is only valid in a generator", self.node)
    }
}

base_convertible!(YieldStatementNode, YieldConverter);
