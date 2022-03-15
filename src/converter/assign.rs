use std::borrow::Cow;
use std::iter::zip;

use derive_new::new;

use crate::parser::assign::{AssignableNode, AssignmentNode};
use crate::parser::dotted::DottedVariableNode;
use crate::parser::index::IndexNode;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::slice::SliceNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::{first, levenshtein};

use super::argument::Argument;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::dotted_var::DotConverter;
use super::error::{CompilerException, CompilerInternalError};
use super::index::IndexConverter;
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct AssignConverter<'a> {
    node: &'a AssignmentNode,
}

impl<'a> ConverterBase for AssignConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.node.is_colon() {
            Err(CompilerException::of(
                "Colon assignment is not supported outside of class definitions",
                self.node,
            )
            .into())
        } else if self.node.get_names().len() > 1 && self.node.get_values().len() == 1 {
            self.convert_single(info)
        } else {
            self.convert_multiple(info)
        }
    }
}

impl<'a> AssignConverter<'a> {
    fn convert_single(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert_eq!(self.node.get_values().len(), 1);
        let names = self.node.get_names();
        let value = &self.node.get_values()[0];
        let mut value_converter = value.test_converter(names.len().try_into().unwrap());
        let ret_types = value_converter.return_type(info)?;
        let mut bytes = value_converter.convert(info)?;
        // For this section, we need to take care of possible side-effects and
        // the order in which they occur.
        // Assignment to indices (via operator []=) and fields (via properties)
        // may contain side-effects, and the language specifies that such
        // assignments should occur in the order in which they were specified
        // in the code.
        // Complicating our lives, however, is the fact that with multiple
        // returns, the stack is in reverse order from what we want. Since in
        // the majority of cases there will be no side-effects, we want to
        // optimize away the stack reversal whenever possible.
        match quick_non_var_count(names) {
            NonVarCount::None => {
                // All variables, guaranteed side-effect free, so optimize reverse
                // order
                for i in (0..names.len()).rev() {
                    let name = match &names[i] {
                        AssignableNode::Name(NameNode::Variable(var)) => var,
                        _ => panic!(),
                    };
                    self.assign_top_to_variable(info, &mut bytes, name, &ret_types[i])?;
                }
            }
            NonVarCount::One => {
                // Only 1 possible side-effect, cannot be switched with another
                for i in (0..names.len()).rev() {
                    self.assign_top(info, &mut bytes, &ret_types[i], &names[i])?;
                }
            }
            NonVarCount::Multiple => {
                // Have to swap everything (sigh)
                for i in 0..names.len() {
                    Self::bring_to_top(&mut bytes, (names.len() - i - 1).try_into().unwrap());
                    self.assign_top(info, &mut bytes, &ret_types[i], &names[i])?;
                }
            }
        }
        Ok(bytes)
    }

    fn convert_multiple(&self, info: &mut CompilerInfo) -> CompileBytes {
        let names = self.node.get_names();
        let values = self.node.get_values();
        if names.len() != values.len() {
            return Err(CompilerException::of(
                format!(
                    "Multiple returns are not supported in = statements \
                 with more than one operand (expected {}, got {})",
                    names.len(),
                    values.len()
                ),
                self.node,
            )
            .into());
        }
        let mut assign_bytes = BytecodeList::new();
        let mut store_bytes = BytecodeList::with_capacity(names.len());
        for (name, value) in zip(names, values) {
            let value_converter = value.test_converter(1);
            match name {
                AssignableNode::Name(NameNode::Variable(var)) => {
                    let var_type = info
                        .get_type(var.get_name())
                        .map(Cow::into_owned)
                        .ok_or_else(|| def_error(info, var))?;
                    let val_converter = value.test_conv_expected(1, vec![var_type]);
                    self.assign_to_variable(
                        info,
                        &mut assign_bytes,
                        &mut store_bytes,
                        var,
                        val_converter,
                    )?;
                }
                AssignableNode::Name(NameNode::Index(idx)) => self.assign_to_index(
                    info,
                    &mut assign_bytes,
                    &mut store_bytes,
                    idx,
                    value_converter,
                )?,
                AssignableNode::Name(NameNode::Dotted(dot)) => {
                    match dot.get_last().get_post_dot() {
                        NameNode::Index(_) => self.assign_to_dot_index(
                            info,
                            &mut assign_bytes,
                            &mut store_bytes,
                            dot,
                            value_converter,
                        )?,
                        NameNode::Variable(_) => {
                            self.assign_to_dot(info, &mut assign_bytes, &mut store_bytes, dot)?
                        }
                        NameNode::SpecialOp(_) => {
                            return Err(CompilerException::of(
                                "Cannot assign to constant value",
                                self.node,
                            )
                            .into())
                        }
                        _ => {
                            return Err(CompilerInternalError::of("Illegal node", self.node).into())
                        }
                    }
                }
                _ => {
                    return Err(CompilerException::of(
                        "Assignment must be to a variable, index, or dotted variable",
                        self.node,
                    )
                    .into())
                }
            }
        }
        assign_bytes.extend(store_bytes);
        Ok(assign_bytes)
    }

    fn assign_top(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        ty: &TypeObject,
        name: &AssignableNode,
    ) -> CompileResult<()> {
        match name {
            AssignableNode::Name(NameNode::Variable(var)) => {
                self.assign_top_to_variable(info, bytes, var, ty)
            }
            AssignableNode::Name(NameNode::Index(idx)) => {
                self.assign_top_to_index(info, bytes, idx, ty)
            }
            AssignableNode::Name(NameNode::Dotted(dot)) => {
                self.assign_top_to_dot(info, bytes, dot, ty)
            }
            _ => Err(CompilerException::of(
                "Assignment must be to a variable, index, or dotted variable",
                name,
            )
            .into()),
        }
    }

    fn assign_to_variable(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        store_bytes: &mut BytecodeList,
        variable: &VariableNode,
        mut value_converter: impl ConverterTest,
    ) -> CompileResult<()> {
        let value_type = first(value_converter.return_type(info)?);
        check_def(info, variable)?;
        let var_type = info.get_type(variable.get_name()).unwrap();
        if !var_type.is_superclass(&value_type) {
            if !OptionTypeObject::needs_make_option(&var_type, &value_type) {
                if var_type.make_mut().is_superclass(&value_type) {
                    return Err(CompilerException::of(
                        "Cannot assign: Value must be 'mut' or 'final'",
                        self.node,
                    )
                    .into());
                } else {
                    return Err(CompilerException::of(
                        format!(
                            "Cannot assign value of type '{}' to variable of type '{}'",
                            value_type.name(),
                            var_type.name()
                        ),
                        self.node,
                    )
                    .into());
                }
            } else {
                bytes.extend(OptionTypeObject::wrap_bytes(value_converter.convert(info)?));
            }
        }
        store_bytes.add_first(Bytecode::Store(info.var_index(variable)?.into()));
        Ok(())
    }

    fn assign_top_to_variable(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        variable: &VariableNode,
        value_type: &TypeObject,
    ) -> CompileResult<()> {
        check_def(info, variable)?;
        let var_type = info.get_type(variable.get_name()).unwrap();
        if !var_type.is_superclass(value_type) {
            if !OptionTypeObject::needs_make_option(&var_type, value_type) {
                return Err(CompilerException::of(
                    format!(
                        "Cannot assign value of type '{}' to variable of type '{}'",
                        value_type.name(),
                        var_type.name()
                    ),
                    self.node,
                )
                .into());
            } else {
                bytes.add(Bytecode::MakeOption());
            }
        }
        bytes.add_first(Bytecode::Store(info.var_index(variable)?.into()));
        Ok(())
    }

    fn assign_to_index(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        store_bytes: &mut BytecodeList,
        variable: &IndexNode,
        mut value_converter: impl ConverterTest,
    ) -> CompileResult<()> {
        // FIXME: Make value an option when necessary
        let indices = variable.get_indices();
        let mut var_converter = variable.get_var().test_converter(1);
        if let Option::Some(slice) = IndexConverter::is_slice(indices) {
            let var_types = var_converter.return_type(info)?;
            let val_types = value_converter.return_type(info)?;
            self.check_slice(info, &var_types[0], &val_types[0])?;
            bytes.extend(var_converter.convert(info)?);
            self.finish_slice(info, bytes, store_bytes, value_converter, slice)
        } else {
            let mut index_converters = convert_indices(indices);
            let var_types = var_converter.return_type(info)?;
            let val_types = value_converter.return_type(info)?;
            self.check_types(info, &var_types[0], &mut index_converters, &val_types[0])?;
            bytes.extend(TestConverter::bytes(variable.get_var(), info, 1)?);
            self.finish_index(info, bytes, store_bytes, value_converter, indices)
        }
    }

    fn assign_top_to_index(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        variable: &IndexNode,
        value_type: &TypeObject,
    ) -> CompileResult<()> {
        let var_converter = variable.get_var().test_converter(1);
        self.top_to_index(
            info,
            bytes,
            var_converter,
            variable.get_indices(),
            value_type,
        )
    }

    fn top_to_index(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        mut pre_dot: impl ConverterTest,
        indices: &[TestNode],
        value_type: &TypeObject,
    ) -> CompileResult<()> {
        if let Option::Some(index) = IndexConverter::is_slice(indices) {
            let pre_types = pre_dot.return_type(info)?;
            self.check_slice(info, value_type, &pre_types[0])?;
            bytes.extend(pre_dot.convert(info)?);
            bytes.extend(index.test_converter(1).convert(info)?);
            bytes.add(Bytecode::Swap3());
            bytes.add(Bytecode::CallOp(OpSpTypeNode::SetSlice.into(), 2.into()));
        } else {
            // FIXME: Make value an option when necessary
            let mut index_converters = convert_indices(indices);
            let pre_types = pre_dot.return_type(info)?;
            self.check_types(info, &pre_types[0], &mut index_converters, value_type)?;
            bytes.extend(pre_dot.convert(info)?);
            for mut index_param in index_converters {
                bytes.extend(index_param.convert(info)?);
            }
            Self::bring_to_top(bytes, (indices.len() + 1).try_into().unwrap());
            bytes.add_first(Bytecode::StoreSubscript((indices.len() as u16).into()));
        }
        Ok(())
    }

    fn check_types(
        &self,
        info: &mut CompilerInfo,
        var_type: &TypeObject,
        values: &mut [TestConverter],
        set_type: &TypeObject,
    ) -> CompileResult<()> {
        let mut index_types = values
            .iter_mut()
            .map(|x| Ok(Argument::new(String::new(), first(x.return_type(info)?))))
            .collect::<CompileResult<Vec<_>>>()?;
        index_types.push(Argument::new(String::new(), set_type.clone()));
        let op_info = var_type.try_operator_info(self.node, OpSpTypeNode::SetAttr, info)?;
        op_info.generify_args(&index_types)?;
        Ok(())
    }

    fn finish_index(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        store_bytes: &mut BytecodeList,
        mut value_converter: impl ConverterTest,
        indices: &[TestNode],
    ) -> CompileResult<()> {
        for index in indices {
            bytes.extend(TestConverter::bytes(index, info, 1)?);
        }
        bytes.extend(value_converter.convert(info)?);
        store_bytes.add_first(Bytecode::StoreSubscript((indices.len() as u16).into()));
        Ok(())
    }

    fn check_slice(
        &self,
        info: &mut CompilerInfo,
        var_type: &TypeObject,
        set_type: &TypeObject,
    ) -> CompileResult<()> {
        let ops = var_type.try_operator_info(self.node, OpSpTypeNode::SetSlice, info)?;
        let args = vec![
            Argument::new(String::new(), info.builtins().slice_type().clone()),
            Argument::new(String::new(), set_type.clone()),
        ];
        ops.generify_args(&args)?;
        Ok(())
    }

    fn finish_slice(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        store_bytes: &mut BytecodeList,
        mut value_converter: impl ConverterTest,
        index: &SliceNode,
    ) -> CompileResult<()> {
        bytes.extend(index.test_converter(1).convert(info)?);
        bytes.extend(value_converter.convert(info)?);
        store_bytes.add_first(Bytecode::CallOp(OpSpTypeNode::SetSlice.into(), 2.into()));
        Ok(())
    }

    fn assign_to_dot_index(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        store_bytes: &mut BytecodeList,
        variable: &DottedVariableNode,
        mut value_converter: impl ConverterTest,
    ) -> CompileResult<()> {
        let (mut var_converter, indices) = DotConverter::except_last_index(variable, 1)?;
        if let Option::Some(slice) = IndexConverter::is_slice(indices) {
            let var_types = var_converter.return_type(info)?;
            let val_types = value_converter.return_type(info)?;
            self.check_slice(info, &var_types[0], &val_types[0])?;
            bytes.extend(var_converter.convert(info)?);
            self.finish_slice(info, bytes, store_bytes, value_converter, slice)?;
        } else {
            let mut index_converters = convert_indices(indices);
            let pre_types = var_converter.return_type(info)?;
            let value_types = value_converter.return_type(info)?;
            self.check_types(info, &pre_types[0], &mut index_converters, &value_types[0])?;
            bytes.extend(var_converter.convert(info)?);
            self.finish_index(info, bytes, store_bytes, value_converter, indices)?;
        }
        Ok(())
    }

    fn assign_to_dot(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        store_bytes: &mut BytecodeList,
        variable: &DottedVariableNode,
    ) -> CompileResult<()> {
        let (mut pre_dot, name) = DotConverter::except_last(variable, 1)?;
        let assigned_type = assign_type(info, &mut pre_dot, name, variable)?;
        let mut value_converter = variable.test_conv_expected(1, vec![assigned_type]);
        let value_type = first(value_converter.return_type(info)?);
        let needs_make_option =
            self.check_assign(info, &mut pre_dot, name, &value_type, variable)?;
        bytes.extend(pre_dot.convert(info)?);
        bytes.extend(OptionTypeObject::maybe_wrap_bytes(
            value_converter.convert(info)?,
            needs_make_option,
        ));
        store_bytes.add_first(Bytecode::StoreAttr(name.into()));
        Ok(())
    }

    fn assign_top_to_dot(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        variable: &DottedVariableNode,
        value_type: &TypeObject,
    ) -> CompileResult<()> {
        match variable.get_last().get_post_dot() {
            NameNode::Index(_) => self.assign_top_to_dot_index(info, bytes, variable, value_type),
            NameNode::Variable(_) => {
                self.assign_top_to_normal_dot(info, bytes, variable, value_type)
            }
            _ => Err(CompilerException::of("Cannot assign", variable).into()),
        }
    }

    fn assign_top_to_dot_index(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        variable: &DottedVariableNode,
        value_type: &TypeObject,
    ) -> CompileResult<()> {
        let (conv, indices) = DotConverter::except_last_index(variable, 1)?;
        self.top_to_index(info, bytes, conv, indices, value_type)
    }

    fn assign_top_to_normal_dot(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        variable: &DottedVariableNode,
        value_type: &TypeObject,
    ) -> CompileResult<()> {
        let (mut pre_dot, name) = DotConverter::except_last(variable, 1)?;
        let needs_make_option =
            self.check_assign(info, &mut pre_dot, name, value_type, variable)?;
        bytes.extend(pre_dot.convert(info)?);
        bytes.add(Bytecode::Swap2());
        if needs_make_option {
            bytes.add(Bytecode::MakeOption());
        }
        bytes.add_first(Bytecode::StoreAttr(name.into()));
        Ok(())
    }

    fn check_assign(
        &self,
        info: &mut CompilerInfo,
        pre_dot: &mut impl ConverterTest,
        name: &str,
        value_type: &TypeObject,
        variable: &DottedVariableNode,
    ) -> CompileResult<bool> {
        let pre_dot_type = first(pre_dot.return_type(info)?);
        let dot_type = pre_dot_type.try_attr_type(self.node, name, info)?;
        if !dot_type.is_superclass(value_type) {
            if OptionTypeObject::needs_and_super(&dot_type, value_type) {
                Ok(true)
            } else {
                Err(CompilerException::of(
                    format!(
                        "Cannot assign: '{}'.{} has type of '{}', \
                         which is not a superclass of '{}'",
                        pre_dot_type.name(),
                        name,
                        dot_type.name(),
                        value_type.name()
                    ),
                    self.node,
                )
                .into())
            }
        } else if !pre_dot_type.can_set_attr(name, info)
            && !is_constructor_exception(info, &pre_dot_type, variable)
        {
            if pre_dot_type.make_mut().can_set_attr(name, info) {
                Err(CompilerException::of(
                    "Cannot assign to value that is not 'mut' or 'mref'",
                    self.node,
                )
                .into())
            } else {
                Err(CompilerException::of(
                    format!(
                        "Cannot assign: '{}'.{} does not support assignment",
                        pre_dot_type.name(),
                        name
                    ),
                    self.node,
                )
                .into())
            }
        } else {
            Ok(false)
        }
    }

    pub fn bring_to_top(bytes: &mut BytecodeList, dist_from_top: u16) {
        match dist_from_top {
            0 => {}
            1 => bytes.add(Bytecode::Swap2()),
            2 => bytes.add(Bytecode::Swap3()),
            _ => bytes.add(Bytecode::SwapN((dist_from_top + 1).into())),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum NonVarCount {
    None,
    One,
    Multiple,
}

fn quick_non_var_count(names: &[AssignableNode]) -> NonVarCount {
    let mut count = NonVarCount::None;
    for name in names {
        if !matches!(name, AssignableNode::Name(NameNode::Variable(_))) {
            match count {
                NonVarCount::None => count = NonVarCount::One,
                NonVarCount::One | NonVarCount::Multiple => return NonVarCount::Multiple,
            }
        }
    }
    count
}

fn convert_indices(indices: &[TestNode]) -> Vec<TestConverter> {
    indices.iter().map(|x| x.test_converter(1)).collect()
}

fn check_def(info: &CompilerInfo, variable: &VariableNode) -> CompileResult<()> {
    let name = variable.get_name();
    if info.var_is_undefined(name) {
        Err(def_error(info, variable).into())
    } else if info.variable_is_immutable(name) {
        Err(CompilerException::of(
            format!("Cannot assign to const variable {}", name),
            variable,
        )
        .into())
    } else {
        Ok(())
    }
}

fn def_error(info: &CompilerInfo, variable: &VariableNode) -> CompilerException {
    let name = variable.get_name();
    if let Option::Some(closest) = levenshtein::closest_name(name, info.defined_names()) {
        CompilerException::of(
            format!(
                "Attempted to assign to undefined name {}\n\
                 Help: Did you mean {}?",
                name, closest
            ),
            variable,
        )
    } else {
        CompilerException::of(
            format!("Attempted to assign to undefined name {}", name),
            variable,
        )
    }
}

fn assign_type(
    info: &mut CompilerInfo,
    pre_dot: &mut impl ConverterTest,
    post_dot: &str,
    node: impl Lined,
) -> CompileResult<TypeObject> {
    let pre_dot = &pre_dot.return_type(info)?[0];
    pre_dot
        .try_attr_type(node, post_dot, info)
        .map(|x| x.into_owned())
}

fn is_constructor_exception(
    info: &mut CompilerInfo,
    pre_dot_type: &TypeObject,
    variable_node: &DottedVariableNode,
) -> bool {
    if pre_dot_is_self(variable_node) {
        info.access_handler().is_in_constructor(pre_dot_type)
    } else {
        false
    }
}

fn pre_dot_is_self(node: &DottedVariableNode) -> bool {
    if node.get_post_dots().len() != 1 {
        false
    } else if let Result::Ok(var) = <&VariableNode>::try_from(node.get_pre_dot()) {
        // 'self' is a reserved name, so this is enough to check
        var.get_name() == "self"
    } else {
        false
    }
}

base_convertible!(AssignmentNode, AssignConverter);
