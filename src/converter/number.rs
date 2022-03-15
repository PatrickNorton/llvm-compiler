use derive_new::new;

use crate::parser::number::{Number, NumberNode};

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::CompilerException;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileConstant, CompileTypes};

#[derive(Debug, new)]
pub struct NumberConverter<'a> {
    node: &'a NumberNode,
    ret_count: u16,
}

impl<'a> ConverterTest for NumberConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![match self.node.get_value() {
            Number::Integer(_) => info.builtins().int_type().clone(),
            Number::Decimal(_) => info.builtins().dec_type().clone(),
        }])
    }

    fn constant_return(&mut self, _info: &mut CompilerInfo) -> CompileConstant {
        Ok(Some(self.constant()))
    }
}

impl<'a> ConverterBase for NumberConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            warning::warn(
                "Numeric literal unused",
                WarningType::Unused,
                info,
                self.node,
            )?;
            Ok(BytecodeList::new())
        } else if self.ret_count > 1 {
            Err(CompilerException::of(
                format!(
                    "Numeric literals return 1 value, {} were expected",
                    self.ret_count
                ),
                self.node,
            )
            .into())
        } else {
            Ok(BytecodeList::of(Bytecode::LoadConst(
                self.constant().into(),
            )))
        }
    }
}

impl<'a> NumberConverter<'a> {
    fn constant(&self) -> LangConstant {
        match self.node.get_value() {
            Number::Integer(i) => i.clone().into(),
            Number::Decimal(d) => d.clone().into(),
        }
    }
}

test_convertible!(NumberNode, NumberConverter);
