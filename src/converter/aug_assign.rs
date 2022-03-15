use derive_new::new;

use crate::parser::aug_assign::{AugAssignTypeNode, AugmentedAssignmentNode};
use crate::parser::dotted::DottedVariableNode;
use crate::parser::index::IndexNode;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::first;

use super::argument::Argument;
use super::bytecode::{Bytecode, Label};
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::dotted_var::DotConverter;
use super::error::{CompilerException, CompilerInternalError};
use super::fn_info::FunctionInfo;
use super::index::IndexConverter;
use super::operator::operator_bytecode;
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct AugAssignConverter<'a> {
    node: &'a AugmentedAssignmentNode,
}

impl<'a> ConverterBase for AugAssignConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.node.get_operator() == AugAssignTypeNode::NullCoerce {
            return self.convert_null_coerce(info);
        }
        let name = remove_illegal(self.node.get_name())?;
        match name {
            NameNode::Variable(_) => self.convert_var(info),
            NameNode::Dotted(dot) => {
                let last = dot.get_last();
                match remove_illegal(last.get_post_dot())? {
                    NameNode::Variable(_) => self.convert_dot(info),
                    NameNode::Index(_) => self.convert_dot_index(info),
                    NameNode::Dotted(_) => Err(CompilerInternalError::of(
                        "Dotted variables should not have dotted variables as post-dots",
                        name,
                    )
                    .into()),
                    post_dot => Err(CompilerInternalError::of(
                        format!("Type not filtered by remove_illegal(): {:?}", post_dot),
                        name,
                    )
                    .into()),
                }
            }
            NameNode::Index(_) => self.convert_index(info),
            _ => Err(CompilerInternalError::of(
                format!("Type not filtered by removeIllegal(): {:?}", name),
                name,
            )
            .into()),
        }
    }
}

impl<'a> AugAssignConverter<'a> {
    fn convert_var(&self, info: &mut CompilerInfo) -> CompileBytes {
        let name = <&VariableNode>::try_from(self.node.get_name()).unwrap();
        let mut assigned_converter = name.test_converter(1);
        let mut value_converter = self.node.get_value().test_converter(1);
        let converter_return = first(assigned_converter.return_type(info)?);
        let operator = self.node.get_operator().get_operator();
        let op_info = converter_return.operator_info(OpSpTypeNode::translate(operator), info)?;
        let value_return = first(value_converter.return_type(info)?);
        self.check_info(op_info.as_deref(), &converter_return, &value_return)?;
        let mut bytes = assigned_converter.convert(info)?;
        bytes.extend(value_converter.convert(info)?);
        bytes.add(operator_bytecode(operator));
        bytes.add(Bytecode::Store(info.var_index(name)?.into()));
        Ok(bytes)
    }

    fn convert_index(&self, info: &mut CompilerInfo) -> CompileBytes {
        let index = <&IndexNode>::try_from(self.node.get_name()).unwrap();
        let assigned_converter = index.get_var().test_converter(1);
        self.convert_index_inner(info, assigned_converter, index.get_indices())
    }

    fn convert_index_inner(
        &self,
        info: &mut CompilerInfo,
        mut assigned_converter: impl ConverterTest,
        indices: &[TestNode],
    ) -> CompileBytes {
        let operator = self.node.get_operator().get_operator();
        let true_op = OpSpTypeNode::translate(operator);
        let mut value_converter = self.node.get_value().test_converter(1);
        let converter_return = first(assigned_converter.return_type(info)?);
        let attr_info =
            converter_return.try_operator_info(self.node, OpSpTypeNode::SetAttr, info)?;
        converter_return.try_operator_info(self.node, OpSpTypeNode::SetAttr, info)?;
        let dot_type = &attr_info.get_returns()[0];
        let return_info = dot_type.operator_info(true_op, info)?;
        self.check_info(
            return_info.as_deref(),
            dot_type,
            &value_converter.return_type(info)?[0],
        )?;
        let mut bytes = IndexConverter::convert_duplicate(
            assigned_converter,
            indices,
            info,
            indices.len() as u16,
        )?;
        bytes.extend(value_converter.convert(info)?);
        bytes.add(operator_bytecode(operator));
        bytes.add(Bytecode::StoreSubscript((indices.len() as u16).into()));
        Ok(bytes)
    }

    fn convert_dot(&self, info: &mut CompilerInfo) -> CompileBytes {
        let operator = self.node.get_operator().get_operator();
        let true_op = OpSpTypeNode::translate(operator);
        let name = <&DottedVariableNode>::try_from(self.node.get_name()).unwrap();
        let (mut assigned_converter, str_name) = DotConverter::except_last(name, 1)?;
        let mut value_converter = self.node.get_value().test_converter(1);
        let converter_return = first(assigned_converter.return_type(info)?);
        let dot_type = converter_return.try_attr_type(self.node, str_name, info)?;
        let return_info = dot_type.operator_info(true_op, info)?;
        self.check_info(
            return_info.as_deref(),
            &dot_type,
            &value_converter.return_type(info)?[0],
        )?;
        let mut bytes = assigned_converter.convert(info)?;
        bytes.add(Bytecode::PopTop());
        bytes.add(Bytecode::LoadDot(str_name.into()));
        bytes.extend(value_converter.convert(info)?);
        bytes.add(operator_bytecode(operator));
        bytes.add(Bytecode::StoreAttr(str_name.into()));
        Ok(bytes)
    }

    fn convert_dot_index(&self, info: &mut CompilerInfo) -> CompileBytes {
        let name = <&DottedVariableNode>::try_from(self.node.get_name()).unwrap();
        let (assigned_converter, indices) = DotConverter::except_last_index(name, 1)?;
        self.convert_index_inner(info, assigned_converter, indices)
    }

    fn convert_null_coerce(&self, info: &mut CompilerInfo) -> CompileBytes {
        match remove_illegal(self.node.get_name())? {
            NameNode::Variable(_) => self.convert_null_coerce_var(info),
            NameNode::Dotted(_) => self.convert_null_coerce_dot(info),
            NameNode::Index(_) => self.convert_null_coerce_index(info),
            name => Err(CompilerInternalError::of(
                format!("Type not filtered by removeIllegal(): {:?}", name),
                name,
            )
            .into()),
        }
    }

    fn convert_null_coerce_var(&self, info: &mut CompilerInfo) -> CompileBytes {
        let variable = <&VariableNode>::try_from(self.node.get_name()).unwrap();
        if info.variable_is_immutable(variable.get_name()) {
            return Err(
                CompilerException::of("Cannot assign to immutable variable", self.node).into(),
            );
        }
        let mut assigned_converter = variable.test_converter(1);
        let mut value_converter = self.node.get_value().test_converter(1);
        let variable_type = first(assigned_converter.return_type(info)?);
        let value_type = first(value_converter.return_type(info)?);
        let variable_type =
            OptionTypeObject::try_from(variable_type).map_err(|x| coerce_error(self.node, &x))?;
        let needs_make_option = self.needs_make_option(&variable_type, &value_type)?;
        let mut bytes = assigned_converter.convert(info)?;
        let label = Label::new();
        bytes.add(Bytecode::JumpNN(label.clone().into()));
        bytes.extend(value_converter.convert(info)?);
        if needs_make_option {
            bytes.add(Bytecode::MakeOption());
        }
        bytes.add(Bytecode::Store(info.var_index(variable)?.into()));
        bytes.add_label(label);
        Ok(bytes)
    }

    fn convert_null_coerce_dot(&self, info: &mut CompilerInfo) -> CompileBytes {
        let name = <&DottedVariableNode>::try_from(self.node.get_name()).unwrap();
        let post_dot = remove_illegal(name.get_last().get_post_dot())?;
        match post_dot {
            NameNode::Variable(_) => self.convert_null_coerce_dot_var(info),
            NameNode::Index(_) => self.convert_null_coerce_dotted_index(info),
            NameNode::Dotted(_) => Err(CompilerInternalError::of(
                "Dotted variables should not be part of a post-dot",
                post_dot,
            )
            .into()),
            _ => Err(CompilerInternalError::of(
                format!(
                    "Post-dot type not filtered by remove_illegal(): {:?}",
                    post_dot
                ),
                post_dot,
            )
            .into()),
        }
    }

    fn convert_null_coerce_index(&self, info: &mut CompilerInfo) -> CompileBytes {
        let name = <&IndexNode>::try_from(self.node.get_name()).unwrap();
        let pre_dot_converter = name.get_var().test_converter(1);
        let post_dot_converters = converters_of(name.get_indices());
        self.convert_null_index(info, pre_dot_converter, post_dot_converters)
    }

    fn convert_null_coerce_dot_var(&self, info: &mut CompilerInfo) -> CompileBytes {
        let name = <&DottedVariableNode>::try_from(self.node.get_name()).unwrap();
        let (mut pre_dot_converter, str_name) = DotConverter::except_last(name, 1)?;
        let ret_type = first(pre_dot_converter.return_type(info)?);
        let mut value_converter = self.node.get_value().test_converter(1);
        let value_type = first(value_converter.return_type(info)?);
        let variable_type = ret_type.try_attr_type(name.get_last(), str_name, info)?;
        let variable_type = OptionTypeObject::try_from(variable_type.into_owned())
            .map_err(|x| coerce_error(self.node, &x))?;
        let needs_make_option = self.needs_make_option(&variable_type, &value_type)?;
        let constant = LangConstant::from(str_name);
        let mut bytes = pre_dot_converter.convert(info)?;
        bytes.add(Bytecode::DupTop());
        bytes.add(Bytecode::LoadDot(constant.clone().into()));
        let label = Label::new();
        bytes.add(Bytecode::JumpNN(label.clone().into()));
        bytes.extend(value_converter.convert(info)?);
        if needs_make_option {
            bytes.add(Bytecode::MakeOption());
        }
        bytes.add(Bytecode::StoreAttr(constant.into()));
        let label_2 = Label::new();
        bytes.add(Bytecode::Jump(label_2.clone().into()));
        bytes.add_label(label);
        bytes.add(Bytecode::PopTop());
        bytes.add_label(label_2);
        Ok(bytes)
    }

    fn convert_null_coerce_dotted_index(&self, info: &mut CompilerInfo) -> CompileBytes {
        let name = <&DottedVariableNode>::try_from(self.node.get_name()).unwrap();
        let (pre_dot, post_dots) = DotConverter::except_last_index(name, 1)?;
        let post_dot_converters = converters_of(post_dots);
        self.convert_null_index(info, pre_dot, post_dot_converters)
    }

    fn convert_null_index(
        &self,
        info: &mut CompilerInfo,
        mut pre_dot_converter: impl ConverterTest,
        mut post_dots: Vec<impl ConverterTest>,
    ) -> CompileBytes {
        let name = self.node.get_name();
        let mut value_converter = self.node.get_value().test_converter(1);
        let pre_dot_type = first(pre_dot_converter.return_type(info)?);
        let variable_type =
            first(pre_dot_type.try_operator_return_type(name, OpSpTypeNode::GetAttr, info)?);
        pre_dot_type.try_operator_info(name, OpSpTypeNode::SetAttr, info)?;
        let value_type = first(value_converter.return_type(info)?);
        let variable_type =
            OptionTypeObject::try_from(variable_type).map_err(|x| coerce_error(self.node, &x))?;
        let needs_make_option = self.needs_make_option(&variable_type, &value_type)?;
        let mut bytes = pre_dot_converter.convert(info)?;
        for post_dot in &mut post_dots {
            bytes.extend(post_dot.convert(info)?);
        }
        if post_dots.len() == 1 {
            bytes.add(Bytecode::DupTop2());
        } else {
            bytes.add(Bytecode::DupTopN(((post_dots.len() + 1) as u16).into()));
        }
        bytes.add(Bytecode::LoadSubscript((post_dots.len() as u16).into()));
        let label = Label::new();
        bytes.add(Bytecode::JumpNN(label.clone().into()));
        bytes.extend(value_converter.convert(info)?);
        if needs_make_option {
            bytes.add(Bytecode::MakeOption());
        }
        bytes.add(Bytecode::StoreSubscript((post_dots.len() as u16).into()));
        let label_2 = Label::new();
        bytes.add(Bytecode::Jump(label_2.clone().into()));
        bytes.add_label(label);
        for _ in &post_dots {
            bytes.add(Bytecode::PopTop());
        }
        bytes.add_label(label_2);
        Ok(bytes)
    }

    fn check_info(
        &self,
        fn_info: Option<&FunctionInfo>,
        assigned_return: &TypeObject,
        value_return: &TypeObject,
    ) -> CompileResult<()> {
        match fn_info {
            Option::None => Err(CompilerException::of(
                format!(
                    "Value of type '{}' does not have an overloaded '{}' operator",
                    assigned_return.name(),
                    self.node.get_operator().get_operator().sequence()
                ),
                self.node,
            )
            .into()),
            Option::Some(fn_info) => {
                let line_info = self.node.get_value().line_info();
                let argument = Argument::new_full(
                    String::new(),
                    value_return.clone(),
                    false,
                    line_info.clone(),
                );
                if !fn_info.matches(&[argument]) {
                    return Err(CompilerException::of(
                        format!(
                            "'{}'.operator {} cannot be called with type '{}'",
                            assigned_return.name(),
                            self.node.get_operator().get_operator().sequence(),
                            value_return.name()
                        ),
                        line_info,
                    )
                    .into());
                }
                let return_type = &fn_info.get_returns()[0];
                if !assigned_return.is_superclass(return_type) {
                    Err(CompilerException::of(
                        // FIXME: Don't use debug representation here
                        format!(
                            "Value of type {} has a return type of '{}', \
                             which is incompatible with the return type of '{:?}'",
                            assigned_return.name(),
                            return_type.name(),
                            self.node.get_name()
                        ),
                        self.node,
                    )
                    .into())
                } else {
                    Ok(())
                }
            }
        }
    }

    fn needs_make_option(
        &self,
        variable_type: &OptionTypeObject,
        value_type: &TypeObject,
    ) -> CompileResult<bool> {
        if variable_type.is_superclass(value_type) {
            Ok(false)
        } else if variable_type.get_option_val().is_superclass(value_type) {
            Ok(true)
        } else {
            Err(CompilerException::of(
                format!(
                    "Cannot assign: Expected instance of '{}', got '{}'",
                    variable_type.get_option_val().name(),
                    value_type.name()
                ),
                self.node,
            )
            .into())
        }
    }
}

fn remove_illegal(name: &NameNode) -> CompileResult<&NameNode> {
    match name {
        NameNode::Dotted(_) | NameNode::Index(_) | NameNode::Variable(_) => Ok(name),
        NameNode::EscapedOp(_) => Err(illegal_error("escaped operators", name).into()),
        NameNode::Function(_) => Err(illegal_error("function calls", name).into()),
        NameNode::SpecialOp(_) => Err(illegal_error("operator names", name).into()),
    }
}

fn illegal_error(value: &str, name: impl Lined) -> CompilerException {
    CompilerException::of(
        format!("Augmented assignment does not work on {}", value),
        name,
    )
}

fn coerce_error(node: impl Lined, value_type: &TypeObject) -> CompilerException {
    CompilerException::of(
        format!(
            "??= only works on an optional variable, not one of type '{}'",
            value_type.name()
        ),
        node,
    )
}

fn converters_of(nodes: &[TestNode]) -> Vec<TestConverter> {
    nodes.iter().map(|x| x.test_converter(1)).collect()
}

base_convertible!(AugmentedAssignmentNode, AugAssignConverter);
