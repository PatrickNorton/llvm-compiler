use derive_new::new;
use num::{BigInt, One, Signed, ToPrimitive};

use crate::converter::type_obj::TypeObject;
use crate::parser::index::IndexNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::slice::SliceNode;
use crate::parser::test_node::TestNode;
use crate::util::first;

use super::argument::Argument;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{LangConstant, RangeConstant};
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::CompilerException;
use super::test_converter::TestConverter;
use super::{int_arithmetic, CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct IndexConverter<'a> {
    node: &'a IndexNode,
    ret_count: u16,
}

impl<'a> ConverterBase for IndexConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = TestConverter::bytes(self.node.get_var(), info, 1)?;
        if let Option::Some(slice) = Self::is_slice(self.node.get_indices()) {
            self.check_slice_type(info)?;
            bytes.extend(TestConverter::bytes(slice, info, 1)?);
            bytes.add(Bytecode::CallOp(OpSpTypeNode::GetSlice.into(), 1.into()));
        } else {
            Self::convert_indices(info, &mut bytes, self.node.get_indices())?;
            if self.ret_count == 0 {
                bytes.add(Bytecode::PopTop());
            }
        }
        Ok(bytes)
    }
}

impl<'a> ConverterTest for IndexConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        if let Option::Some(ty) = TypeObject::of_index(info, self.node)? {
            return Ok(vec![ty.get_type()]);
        }
        let operator = if Self::is_slice(self.node.get_indices()).is_some() {
            OpSpTypeNode::GetSlice
        } else {
            OpSpTypeNode::GetAttr
        };
        first(TestConverter::return_type(info, self.node.get_var(), 1)?)
            .try_operator_return_type(self.node, operator, info)
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        if self.node.get_indices().len() > 1 {
            return Ok(None);
        }
        let pre_dot_const = match TestConverter::constant_return(self.node.get_var(), info, 1)? {
            Some(x) => x,
            None => return Ok(None),
        };
        match pre_dot_const {
            LangConstant::String(constant) => self.string_constant(info, constant.get_value()),
            LangConstant::Range(constant) => self.range_constant(info, &constant),
            LangConstant::Bytes(constant) => self.bytes_constant(info, constant.get_value()),
            _ => Ok(None),
        }
    }

    fn try_convert_slice(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.node_is_slice() {
            self.convert_iter_slice(info)
        } else {
            self.convert(info)
        }
    }
}

impl<'a> IndexConverter<'a> {
    pub fn is_slice(indices: &[TestNode]) -> Option<&SliceNode> {
        match indices {
            [TestNode::Slice(s)] => Some(s),
            _ => None,
        }
    }

    pub fn node_is_slice(&self) -> bool {
        Self::is_slice(self.node.get_indices()).is_some()
    }

    pub fn convert_indices(
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        indices: &[TestNode],
    ) -> CompileResult<()> {
        for index in indices {
            bytes.extend(TestConverter::bytes(index, info, 1)?);
        }
        if indices.len() == 1 {
            bytes.add(Bytecode::Subscript());
        } else {
            bytes.add(Bytecode::LoadSubscript((indices.len() as u16).into()))
        }
        Ok(())
    }

    pub fn convert_duplicate(
        mut converter: impl ConverterTest,
        indices: &[TestNode],
        info: &mut CompilerInfo,
        argc: u16,
    ) -> CompileBytes {
        let mut bytes = converter.convert(info)?;
        for param in indices {
            bytes.extend(TestConverter::bytes(param, info, 1)?);
        }
        if indices.len() == 1 {
            bytes.add(Bytecode::DupTop2());
        } else {
            bytes.add(Bytecode::DupTopN(((indices.len() + 1) as u16).into()));
        }
        bytes.add(Bytecode::LoadSubscript(argc.into()));
        Ok(bytes)
    }

    fn convert_iter_slice(&self, info: &mut CompilerInfo) -> CompileBytes {
        let slice = Self::is_slice(self.node.get_indices()).unwrap();
        let mut converter = self.node.get_var().test_converter(1);
        let ret = first(converter.return_type(info)?);
        let has_iter = ret.operator_info(OpSpTypeNode::IterSlice, info).is_some();
        let mut bytes = converter.convert(info)?;
        self.check_slice_type(info)?;
        bytes.extend(TestConverter::bytes(slice, info, 1)?);
        let operator = if has_iter {
            OpSpTypeNode::IterSlice
        } else {
            OpSpTypeNode::GetSlice
        };
        bytes.add(Bytecode::CallOp(operator.into(), 1.into()));
        Ok(bytes)
    }

    fn check_slice_type(&self, info: &mut CompilerInfo) -> CompileResult<()> {
        let ret_type = first(TestConverter::return_type(info, self.node.get_var(), 1)?);
        let fn_info = ret_type.try_operator_info(self.node, OpSpTypeNode::GetSlice, info)?;
        if !fn_info.matches(&[Argument::new(
            String::new(),
            info.builtins().slice_type().clone(),
        )]) {
            Err(CompilerException::of(
                format!(
                    "Type '{}' has an operator [:] that does not take a slice as its argument",
                    ret_type.name()
                ),
                self.node,
            )
            .into())
        } else {
            Ok(())
        }
    }

    fn index_constant(&self, info: &mut CompilerInfo) -> CompileResult<Option<BigInt>> {
        debug_assert_eq!(self.node.get_indices().len(), 1);
        Ok(
            TestConverter::constant_return(&self.node.get_indices()[0], info, 1)?
                .and_then(|x| int_arithmetic::convert_const(&x).map(|x| x.into_owned())),
        )
    }

    fn int_index_constant(&self, info: &mut CompilerInfo) -> CompileResult<Option<i32>> {
        debug_assert_eq!(self.node.get_indices().len(), 1);
        Ok(
            TestConverter::constant_return(&self.node.get_indices()[0], info, 1)?
                .and_then(|x| int_arithmetic::convert_to_int(&x)),
        )
    }

    fn string_constant(&self, info: &mut CompilerInfo, value: &str) -> CompileConstant {
        Ok(self
            .int_index_constant(info)?
            .and_then(|x| x.try_into().ok())
            .and_then(|index| value.chars().nth(index).map(|x| x.into())))
    }

    fn range_constant(&self, info: &mut CompilerInfo, value: &RangeConstant) -> CompileConstant {
        if let Option::Some(index) = self.index_constant(info)? {
            if let Option::Some(start) = value.get_start() {
                let one = BigInt::one();
                let step = value.get_step().as_ref().unwrap_or(&one);
                let result = start + (index * step);
                if value
                    .get_stop()
                    .as_ref()
                    .map_or_else(|| true, |stop| in_range(&result, stop, step))
                {
                    return Ok(Some(result.into()));
                }
            }
        }
        Ok(None)
    }

    fn bytes_constant(&self, info: &mut CompilerInfo, value: &[u8]) -> CompileConstant {
        Ok(self
            .int_index_constant(info)?
            .and_then(|index| index.to_usize())
            .and_then(|index| value.get(index).cloned().map(Into::into)))
    }
}

fn in_range(value: &BigInt, stop: &BigInt, step: &BigInt) -> bool {
    if !step.is_negative() {
        value < stop
    } else {
        value > stop
    }
}

test_convertible!(IndexNode, IndexConverter);
