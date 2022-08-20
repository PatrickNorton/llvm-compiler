use derive_new::new;
use num::{One, Signed, Zero};

use crate::parser::dotimes::DotimesStatementNode;
use crate::util::first;

use super::base_converter::BaseConverter;
use super::bytecode::{Bytecode, Label};
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::CompilerException;
use super::loop_converter::LoopConverter;
use super::warning::{self, WarningType};
use super::{int_arithmetic, CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct DotimesConverter<'a> {
    node: &'a DotimesStatementNode,
}

impl<'a> LoopConverter for DotimesConverter<'a> {
    const HAS_CONTINUE: bool = true;

    fn true_convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut count_converter = self.node.get_iterations().test_converter(1);
        let ret_type = first(count_converter.return_type(info)?);
        if !info.builtins().int_type().is_superclass(&ret_type) {
            return Err(CompilerException::of(
                format!(
                    "dotimes loop's iteration count has type '{}', which is not a subclass of int",
                    ret_type.name()
                ),
                self.node.get_iterations(),
            )
            .into());
        }
        if let Option::Some(constant) = count_converter.constant_return(info)? {
            self.check_constant(info, constant)?
        }
        let mut bytes = count_converter.convert(info)?;
        bytes.add_label(info.loop_manager().continue_label().clone());
        let top_label = Label::new();
        bytes.add_label(top_label.clone());
        let label = Label::new();
        bytes.add(Bytecode::Dotimes(label.clone().into()));
        bytes.extend(self.convert_body(info)?);
        bytes.add(Bytecode::Jump(top_label.into()));
        bytes.add_label(label);
        Ok(bytes)
    }
}

impl<'a> DotimesConverter<'a> {
    fn check_constant(&self, info: &mut CompilerInfo, constant: LangConstant) -> CompileResult<()> {
        let value = int_arithmetic::convert_const(&constant).unwrap();
        if value.is_negative() {
            warning::warn_note(
                "Loop will never execute",
                "'dotimes' loops with negative values may become an error in the future",
                WarningType::TrivialValue,
                info,
                self.node.get_iterations(),
            )?;
        } else if value.is_zero() {
            warning::warn(
                "Loop will never execute",
                WarningType::TrivialValue,
                info,
                self.node.get_iterations(),
            )?;
        } else if value.is_one() {
            warning::warn(
                "'dotimes 1' is unnecessary, as loop will only execute once",
                WarningType::TrivialValue,
                info,
                self.node.get_iterations(),
            )?;
        }
        Ok(())
    }

    fn convert_body(&self, info: &mut CompilerInfo) -> CompileBytes {
        let (bytes, diverging) = BaseConverter::bytes_with_return(self.node.get_body(), info)?;
        if (diverging.will_break() || diverging.will_return()) && diverging.may_continue() {
            warning::warn(
                "Loop executes no more than once",
                WarningType::Unreachable,
                info,
                self.node,
            )?;
        }
        Ok(bytes)
    }
}

base_convertible!(DotimesStatementNode, DotimesConverter);
