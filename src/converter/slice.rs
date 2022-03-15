use derive_new::new;

use crate::parser::slice::SliceNode;
use crate::parser::test_node::TestNode;
use crate::util::first;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::CompilerException;
use super::test_converter::TestConverter;
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct SliceConverter<'a> {
    node: &'a SliceNode,
    ret_count: u16,
}

impl<'a> ConverterBase for SliceConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        debug_assert_eq!(self.ret_count, 1);
        let mut bytes = BytecodeList::new();
        add_part(info, &mut bytes, self.node.get_start())?;
        add_part(info, &mut bytes, self.node.get_stop())?;
        add_part(info, &mut bytes, self.node.get_step())?;
        bytes.add(Bytecode::MakeSlice());
        Ok(bytes)
    }
}

impl<'a> ConverterTest for SliceConverter<'a> {
    fn return_type(&mut self, _info: &mut CompilerInfo) -> CompileTypes {
        Ok(Vec::new())
    }
}

fn add_part(
    info: &mut CompilerInfo,
    bytes: &mut BytecodeList,
    part: &TestNode,
) -> CompileResult<()> {
    if !part.is_empty() {
        check_types(info, part)?;
        bytes.extend(TestConverter::bytes(part, info, 1)?);
    } else {
        bytes.add(Bytecode::LoadNull());
    }
    Ok(())
}

fn check_types(info: &mut CompilerInfo, sub_node: &TestNode) -> CompileResult<()> {
    let ret_type = first(TestConverter::return_type(info, sub_node, 1)?);
    if !info.builtins().int_type().is_superclass(&ret_type) {
        Err(CompilerException::of(
            format!(
                "Type {} is not a subclass of int, cannot be used in a slice",
                ret_type.name()
            ),
            sub_node,
        )
        .into())
    } else {
        Ok(())
    }
}

test_convertible!(SliceNode, SliceConverter);
