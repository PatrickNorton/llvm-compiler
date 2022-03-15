use derive_new::new;
use num::BigInt;

use crate::parser::range::RangeLiteralNode;
use crate::parser::test_node::TestNode;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::RangeConstant;
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::CompilerException;
use super::test_converter::TestConverter;
use super::warning::{self, WarningType};
use super::{int_arithmetic, CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct RangeConverter<'a> {
    node: &'a RangeLiteralNode,
    ret_count: u16,
}

impl<'a> ConverterTest for RangeConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().range_type().clone()])
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        let start = if self.node.get_start().is_empty() {
            None
        } else {
            match ret_val(self.node.get_start(), info)? {
                None => return Ok(None),
                x => x,
            }
        };
        let stop = if self.node.get_stop().is_empty() {
            None
        } else {
            match ret_val(self.node.get_stop(), info)? {
                None => return Ok(None),
                x => x,
            }
        };
        let step = if self.node.get_step().is_empty() {
            None
        } else {
            match ret_val(self.node.get_step(), info)? {
                None => return Ok(None),
                x => x,
            }
        };
        Ok(Some(RangeConstant::new(start, stop, step).into()))
    }
}

impl<'a> ConverterBase for RangeConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            warning::warn(
                "Range literal creation has unused result",
                WarningType::Unused,
                info,
                self.node,
            )?;
            return Ok(BytecodeList::new());
        }
        if let Option::Some(constant) = self.constant_return(info)? {
            return Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())));
        }
        let mut bytes = BytecodeList::new();
        bytes.add(Bytecode::LoadConst(
            info.builtins().range_const().clone().into(),
        ));
        self.convert_portion(info, &mut bytes, self.node.get_start(), 0)?;
        self.convert_portion(info, &mut bytes, self.node.get_stop(), 0)?;
        self.convert_portion(info, &mut bytes, self.node.get_step(), 0)?;
        bytes.add(Bytecode::CallTos(1.into()));
        Ok(bytes)
    }
}

impl<'a> RangeConverter<'a> {
    fn convert_portion(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        node: &TestNode,
        default_val: u32,
    ) -> CompileResult<()> {
        if !node.is_empty() {
            let mut converter = node.test_converter(1);
            let ret_types = converter.return_type(info)?;
            if !info.builtins().int_type().is_superclass(&ret_types[0]) {
                return Err(CompilerException::of(
                    format!(
                        "Type {} does not match required type {}",
                        ret_types[0].name(),
                        info.builtins().int_type().name()
                    ),
                    self.node,
                )
                .into());
            }
            bytes.extend(converter.convert(info)?)
        } else {
            bytes.add(Bytecode::LoadConst(default_val.into()))
        }
        Ok(())
    }
}

fn ret_val(value: &TestNode, info: &mut CompilerInfo) -> CompileResult<Option<BigInt>> {
    let ret = TestConverter::constant_return(value, info, 1)?;
    Ok(ret.and_then(|x| int_arithmetic::convert_const(&x).map(|x| x.into_owned())))
}

test_convertible!(RangeLiteralNode, RangeConverter);
