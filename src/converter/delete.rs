use derive_new::new;
use itertools::Itertools;
use num::ToPrimitive;

use crate::parser::delete::DeleteStatementNode;
use crate::parser::dotted::DottedVariableNode;
use crate::parser::index::IndexNode;
use crate::parser::name::NameNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::slice::SliceNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::first;

use super::argument::Argument;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::dotted_var::DotConverter;
use super::error::CompilerException;
use super::fn_info::FunctionInfo;
use super::index::IndexConverter;
use super::test_converter::TestConverter;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct DeleteConverter<'a> {
    node: &'a DeleteStatementNode,
}

impl<'a> ConverterBase for DeleteConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        match self.node.get_deleted() {
            TestNode::Name(NameNode::Dotted(del)) => self.convert_dot(info, del),
            TestNode::Name(NameNode::Index(del)) => {
                if IndexConverter::is_slice(del.get_indices()).is_some() {
                    self.convert_slice(info, del)
                } else {
                    self.convert_index(info, del)
                }
            }
            TestNode::Name(NameNode::Variable(var)) => self.convert_variable(info, var),
            _ => Err(CompilerException::of(
                "'del' statement can only be used on a variable or an indexed statement",
                self.node,
            )
            .into()),
        }
    }
}

impl<'a> DeleteConverter<'a> {
    fn convert_dot(
        &self,
        info: &mut CompilerInfo,
        dotted_var: &DottedVariableNode,
    ) -> CompileBytes {
        match dotted_var.get_last().get_post_dot() {
            NameNode::Index(_) => self.convert_dot_index(info, dotted_var),
            _ => Err(CompilerException::of(
                "Cannot delete non-index from dotted variable",
                self.node,
            )
            .into()),
        }
    }

    fn convert_dot_index(
        &self,
        info: &mut CompilerInfo,
        dotted_var: &DottedVariableNode,
    ) -> CompileBytes {
        let (converter, indices) = DotConverter::except_last_index(dotted_var, 1)?;
        if let Option::Some(slice) = IndexConverter::is_slice(indices) {
            self.convert_slice_inner(info, converter, slice)
        } else {
            self.convert_index_inner(info, converter, indices)
        }
    }

    fn convert_index(&self, info: &mut CompilerInfo, del: &IndexNode) -> CompileBytes {
        let var_converter = del.get_var().test_converter(1);
        self.convert_index_inner(info, var_converter, del.get_indices())
    }

    fn convert_index_inner(
        &self,
        info: &mut CompilerInfo,
        mut var_converter: impl ConverterTest,
        indices: &[TestNode],
    ) -> CompileBytes {
        let mut index_converters = indices.iter().map(|x| x.test_converter(1)).collect_vec();
        self.check_access(info, &mut var_converter, &mut index_converters)?;
        let mut bytes = var_converter.convert(info)?;
        let index_len = index_converters.len().to_u16().unwrap();
        for mut converter in index_converters {
            bytes.extend(converter.convert(info)?);
        }
        if index_len == 1 {
            bytes.add(Bytecode::DelSubscript());
        } else {
            bytes.add(Bytecode::CallOp(
                OpSpTypeNode::DelAttr.into(),
                index_len.into(),
            ))
        }
        Ok(bytes)
    }

    fn convert_slice(&self, info: &mut CompilerInfo, del: &IndexNode) -> CompileBytes {
        let slice = IndexConverter::is_slice(del.get_indices()).unwrap();
        let var_converter = del.get_var().test_converter(1);
        self.convert_slice_inner(info, var_converter, slice)
    }

    fn convert_slice_inner(
        &self,
        info: &mut CompilerInfo,
        mut var_converter: impl ConverterTest,
        slice: &SliceNode,
    ) -> CompileBytes {
        let mut slice_converter = slice.test_converter(1);
        let var_type = first(var_converter.return_type(info)?);
        let slice_info = var_type.try_operator_info(self.node, OpSpTypeNode::DelSlice, info)?;
        if !slice_info.matches(&[Argument::new(
            String::new(),
            info.builtins().slice_type().clone(),
        )]) {
            return Err(CompilerException::of(
                format!(
                    "Cannot call '{}'.operator del[:] \
                     (operator del[:] should always be callable with a slice)",
                    var_type.name()
                ),
                self.node,
            )
            .into());
        }
        let mut bytes = var_converter.convert(info)?;
        bytes.extend(slice_converter.convert(info)?);
        bytes.add(Bytecode::CallOp(OpSpTypeNode::DelSlice.into(), 1.into()));
        Ok(bytes)
    }

    fn convert_variable(&self, info: &mut CompilerInfo, del: &VariableNode) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        let name = del.get_name();
        let index = info.var_index(del)?;
        bytes.add(Bytecode::LoadNull());
        bytes.add(Bytecode::Store(index.into())); // Drops value currently stored
        let var_holder = info.var_holder_mut();
        if var_holder.var_def_in_current_frame(name).is_some() {
            var_holder.remove_variable(name);
        } else {
            warning::warn(
                "del on variable not defined in current frame \
                 will not remove the variable properly",
                WarningType::Todo,
                info,
                self.node,
            )?;
        }
        Ok(bytes)
    }

    fn check_access(
        &self,
        info: &mut CompilerInfo,
        value: &mut impl ConverterTest,
        indices: &mut [TestConverter],
    ) -> CompileResult<()> {
        let ret_type = first(value.return_type(info)?);
        match ret_type.operator_info(OpSpTypeNode::DelAttr, info) {
            Option::None => Err(CompilerException::of(
                format!(
                    "Delete cannot be called on an index with a variable of type '{}' \
                     ('{}' has no usable operator del[])",
                    ret_type.name(),
                    ret_type.name()
                ),
                self.node,
            )
            .into()),
            Option::Some(op_info) => {
                let args = indices
                    .iter_mut()
                    .map(|x| Ok(Argument::new(String::new(), first(x.return_type(info)?))))
                    .collect::<CompileResult<Vec<_>>>()?;
                if !op_info.matches(&args) {
                    Err(self.type_mismatch_error(&args, &ret_type, &op_info).into())
                } else {
                    Ok(())
                }
            }
        }
    }

    fn type_mismatch_error(
        &self,
        args: &[Argument],
        ret_type: &TypeObject,
        op_info: &FunctionInfo,
    ) -> CompilerException {
        if args.len() == 1 {
            CompilerException::of(
                format!(
                    "operator del[] on type '{}' does not accept '{}' as an index type",
                    ret_type.name(),
                    args[0].get_type().name()
                ),
                self.node,
            )
        } else {
            let args_string = args.iter().map(|x| x.get_type().name()).format(", ");
            let expected_str = op_info
                .get_args()
                .get_normal_args()
                .iter()
                .map(|x| x.get_type().name())
                .format(", ");
            CompilerException::of(
                format!(
                    "operator del[] on type '{}' expected types [{}], got [{}]",
                    ret_type.name(),
                    args_string,
                    expected_str
                ),
                self.node,
            )
        }
    }
}

base_convertible!(DeleteStatementNode, DeleteConverter);
