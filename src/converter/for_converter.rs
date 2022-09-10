use std::borrow::Cow;

use derive_new::new;

use crate::parser::for_loop::ForStatementNode;
use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::variable::{VarLikeNode, VariableNode};
use crate::util::{first, levenshtein};

use super::base_converter::BaseConverter;
use super::builtins::FORBIDDEN_NAMES;
use super::bytecode::{ArgcBytecode, Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::error_builder::ErrorBuilder;
use super::loop_converter::LoopConverter;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct ForConverter<'a> {
    node: &'a ForStatementNode,
}

impl<'a> LoopConverter for ForConverter<'a> {
    const HAS_CONTINUE: bool = true;

    fn true_convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.true_convert_with_return(info).map(|x| x.0)
    }

    fn true_convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let var_len = self.node.get_vars().len();
        let iter_len = self.node.get_iterables().len();
        if iter_len > var_len {
            Err(CompilerException::of(
                format!(
                    "For-loops may not have more iterables than variables\n\
                     (got {} iterables, {} variables)",
                    iter_len, var_len
                ),
                self.node,
            )
            .into())
        } else if iter_len == 1 {
            self.convert_single_iter(info)
        } else {
            self.convert_multiple_iter(info)
        }
    }
}

impl<'a> ForConverter<'a> {
    fn convert_single_iter(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let ret_count = self.node.get_vars().len();
        let small_ret_count = ret_count.try_into().expect("Too many variables");
        let mut value_converter = self.node.get_iterables()[0].test_converter(1);
        let mut bytes = BytecodeList::new();
        add_iter(info, &mut bytes, &mut value_converter)?;
        bytes.add_label(info.loop_manager().continue_label().clone());
        let label = Label::new();
        bytes.add(Bytecode::ForIter(
            label.clone().into(),
            ArgcBytecode::new(small_ret_count),
        ));
        for i in (0..ret_count).rev() {
            self.add_variable_storage(info, &mut bytes, i, &mut value_converter, false)?;
        }
        let diverging_info = self.add_cleanup(info, label, &mut bytes)?;
        Ok((bytes, diverging_info))
    }

    fn convert_multiple_iter(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let var_len = self.node.get_vars().len();
        let iter_len = self.node.get_iterables().len();
        let variable_count = var_len.try_into().expect("Too many variables");
        if var_len != iter_len {
            return Err(CompilerException::with_note(
                "For loops with more than one iterable must have an \
                 equal number of variables and iterables",
                "Statements with multiple returns are only usable \
                 in for-loops when there is only one",
                self.node,
            )
            .into());
        }
        // TODO: Make this more functional-style
        let mut value_converters = Vec::with_capacity(var_len);
        let mut bytes = BytecodeList::new();
        for iterator in self.node.get_iterables() {
            let mut value_converter = iterator.test_converter(1);
            add_iter(info, &mut bytes, &mut value_converter)?;
            value_converters.push(value_converter);
        }
        bytes.add_label(info.loop_manager().continue_label().clone());
        let label = Label::new();
        bytes.add(Bytecode::ForParallel(
            label.clone().into(),
            ArgcBytecode::new(variable_count),
        ));
        debug_assert_eq!(value_converters.len(), var_len);
        for (i, converter) in value_converters.iter_mut().enumerate().rev() {
            self.add_variable_storage(info, &mut bytes, i, converter, true)?;
        }
        let diverging_info = self.add_cleanup(info, label, &mut bytes)?;
        Ok((bytes, diverging_info))
    }

    fn add_variable_storage(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        i: usize,
        value_converter: &mut impl ConverterTest,
        first_ret: bool,
    ) -> CompileResult<()> {
        let typed_var = &self.node.get_vars()[i];
        let iterator_type = self.get_iterator_type(info, i, value_converter, first_ret)?;
        let variable = typed_var.get_variable();
        match typed_var {
            VarLikeNode::Typed(var) => {
                let name = var.get_name();
                if FORBIDDEN_NAMES.contains(&name) {
                    return Err(CompilerException::of(
                        format!("Illegal name for variable: {}", var.get_name()),
                        variable,
                    )
                    .into());
                }
                let value_type =
                    self.return_type(info, if first_ret { 0 } else { i }, value_converter)?;
                if !iterator_type.is_superclass(&value_type) {
                    return Err(CompilerException::of(
                        format!(
                            "Object of type '{}' cannot be assigned to object of type '{}'",
                            value_type.name(),
                            iterator_type.name()
                        ),
                        self.node,
                    )
                    .into());
                }
                info.check_definition(name, typed_var)?;
                info.add_variable(
                    name.to_string(),
                    iterator_type,
                    false,
                    typed_var.line_info().clone(),
                );
            }
            VarLikeNode::Variable(var) => {
                let name = var.get_name();
                if info.variable_is_immutable(name) {
                    return Err(CompilerException::of(
                        format!("Cannot assign to immutable variable '{}'", name),
                        var,
                    )
                    .into());
                }
                // FIXME: Check variable type
            }
        }
        bytes.add(Bytecode::Store(info.var_index(variable)?.into()));
        Ok(())
    }

    fn return_type(
        &self,
        info: &mut CompilerInfo,
        i: usize,
        value_converter: &mut impl ConverterTest,
    ) -> CompileResult<TypeObject> {
        let op_types = first(value_converter.return_type(info)?).try_operator_return_type(
            self.node.line_info(),
            OpSpTypeNode::Iter,
            info,
        )?;
        let op_type = info.builtins().de_iterable(&op_types[0])?;
        if op_type.len() <= i {
            Err(CompilerException::of(
                format!(
                    "Expected at least {} returns from iterable, got {}",
                    op_type.len(),
                    i + 1
                ),
                self.node,
            )
            .into())
        } else {
            Ok(op_type.into_iter().nth(i).unwrap())
        }
    }

    fn get_iterator_type(
        &self,
        info: &mut CompilerInfo,
        i: usize,
        value_converter: &mut impl ConverterTest,
        first_ret: bool,
    ) -> CompileResult<TypeObject> {
        let variable = &self.node.get_vars()[i];
        match variable {
            VarLikeNode::Variable(var) => {
                return info
                    .get_type(var.get_name())
                    .map(Cow::into_owned)
                    .ok_or_else(|| variable_exception(var, info.defined_names()).into());
            }
            VarLikeNode::Typed(var) => {
                if let TypeLikeNode::Type(iterator_type) = var.get_type() {
                    info.convert_type(iterator_type)
                } else {
                    self.return_type(info, if first_ret { 0 } else { i }, value_converter)
                }
            }
        }
    }

    fn add_cleanup(
        &self,
        info: &mut CompilerInfo,
        label: Label,
        bytes: &mut BytecodeList,
    ) -> CompileResult<DivergingInfo> {
        let (bytecode, mut diverging_info) =
            BaseConverter::bytes_with_return(self.node.get_body(), info)?;
        bytes.extend(bytecode);
        bytes.add(Bytecode::Jump(
            info.loop_manager().continue_label().clone().into(),
        ));
        bytes.add_label(label);
        if (diverging_info.will_break() || diverging_info.will_return())
            && !diverging_info.may_continue()
        {
            warning::warn(
                "Loop executes no more than once",
                WarningType::Unreachable,
                info,
                self.node,
            )?;
        }
        if self.node.get_nobreak().is_empty() {
            diverging_info.make_uncertain();
        } else {
            let (bytecode, diverge) =
                BaseConverter::bytes_with_return(self.node.get_nobreak(), info)?;
            bytes.extend(bytecode);
            diverging_info.and_with(diverge);
        }
        Ok(diverging_info)
    }
}

pub fn add_iter(
    info: &mut CompilerInfo,
    bytes: &mut BytecodeList,
    value_converter: &mut impl ConverterTest,
) -> CompileResult<()> {
    bytes.add(Bytecode::LoadConst(
        info.builtins().iter_constant().clone().into(),
    ));
    bytes.extend(value_converter.try_convert_slice(info)?);
    bytes.add(Bytecode::CallTos(1.into()));
    Ok(())
}

fn variable_exception<'a>(
    var: &VariableNode,
    names: impl Iterator<Item = &'a str>,
) -> CompilerException {
    let name = var.get_name();
    if let Option::Some(closest) = levenshtein::closest_name(name, names) {
        CompilerException::from_builder(
            ErrorBuilder::new(var)
                .with_message(format!(
                    "Variable {} not defined. Did you mean {}?",
                    name, closest
                ))
                .with_help("If not, consider adding 'var' before the variable"),
        )
    } else {
        CompilerException::from_builder(
            ErrorBuilder::new(var)
                .with_message(format!("Variable {} not defined", name))
                .with_help("Consider adding 'var' before the variable"),
        )
    }
}

base_convertible!(ForStatementNode, ForConverter);
