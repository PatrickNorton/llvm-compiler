use std::iter::zip;

use derive_new::new;

use crate::parser::annotation::AnnotatableRef;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::typed_arg::VarargType;
use crate::parser::typed_var::TypedVariableNode;
use crate::util::first;

use super::annotation::{impl_annotatable, AnnotatableConverter};
use super::builtins::FORBIDDEN_NAMES;
use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::constant::OptionConstant;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::mutable::MutableType;
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct DeclaredAssignConverter<'a> {
    node: &'a DeclaredAssignmentNode,
}

impl<'a> AnnotatableConverter<'a> for DeclaredAssignConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::DeclaredAssignment(self.node)
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        if self.node.is_colon() {
            Err(
                CompilerException::of(":= is only allowed for defining class members", self.node)
                    .into(),
            )
        } else if self.is_single() {
            self.convert_single(info).map(|x| (x, DivergingInfo::new()))
        } else {
            self.convert_multiple(info)
                .map(|x| (x, DivergingInfo::new()))
        }
    }
}

impl_annotatable!(DeclaredAssignConverter<'a>);

impl<'a> DeclaredAssignConverter<'a> {
    fn is_single(&self) -> bool {
        self.node.get_values().len() == 1
            && self.node.get_types().len() > 1
            && self.node.get_values().get_vararg(0).unwrap().is_empty()
    }

    fn convert_single(&self, info: &mut CompilerInfo) -> CompileBytes {
        let values = self.node.get_values();
        let types = self.node.get_types();
        assert_eq!(values.len(), 1);
        let value = &values[0];
        let mut value_converter = value.test_converter(types.len() as u16);
        let value_types = value_converter.return_type(info)?;
        let is_static = self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Static);
        let mut bytes = BytecodeList::new();
        let static_label = add_static(&mut bytes, is_static);
        bytes.extend(value_converter.convert(info)?);
        let mutability = MutableType::from_nullable(self.node.get_mutability());
        let is_const = mutability.is_const_ref();
        // Iterate backward b/c variables are in reversed order on the stack
        // B/c this is *declared* assignment, we know this to be side-effect free, so this is safe
        // We probably don't even *need* to build up the stack here, but I'm slightly wary that might
        // actually cause order-of-execution issues
        for (assigned, value_type) in zip(types, value_types).rev() {
            let assigned_type = get_assigned(info, &value_type, assigned.get_type(), mutability)?;
            let assigned_name = assigned.get_variable().get_name();
            if FORBIDDEN_NAMES.contains(&assigned_name) {
                return Err(CompilerException::of(
                    format!("Illegal name {}", assigned_name),
                    self.node,
                )
                .into());
            }
            let needs_make_option = self.check_types(&assigned_type, &value_type)?;
            if needs_make_option {
                bytes.add(Bytecode::MakeOption());
            }
            self.finish_assignment(
                info,
                &mut bytes,
                is_static,
                assigned_type,
                assigned_name.to_string(),
                is_const,
                assigned,
            )?;
        }
        if is_static {
            bytes.add_label(static_label.unwrap());
        }
        Ok(bytes)
    }

    fn convert_multiple(&self, info: &mut CompilerInfo) -> CompileBytes {
        let types = self.node.get_types();
        let values = self.node.get_values();
        let is_static = self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Static);
        let mut bytes = BytecodeList::new();
        let static_lbl = add_static(&mut bytes, is_static);
        let mutability = MutableType::from_nullable(self.node.get_mutability());
        let mut tuple_count = 0;
        for i in 0..values.len() {
            match values.get_vararg(i).unwrap() {
                VarargType::None => self.convert_no_tuple(
                    info,
                    &mut bytes,
                    &types[i - tuple_count],
                    is_static,
                    &values[i],
                    mutability,
                )?,
                VarargType::Single => self.convert_tuple(
                    info,
                    &mut bytes,
                    is_static,
                    mutability,
                    &mut tuple_count,
                    i,
                )?,
                VarargType::Double => {
                    return Err(CompilerException::of(
                        "Cannot unpack dictionaries in declared assignment",
                        &values[i],
                    )
                    .into())
                }
            }
        }
        if types.len() != values.len() + tuple_count {
            return Err(CompilerException::of(
                format!(
                    "Multiple returns are not supported in = statements \
                     with more than one operand (got {} variables and {} expressions)",
                    types.len(),
                    values.len()
                ),
                self.node,
            )
            .into());
        }
        if is_static {
            bytes.add_label(static_lbl.unwrap());
        }
        Ok(bytes)
    }

    fn convert_no_tuple(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        assigned: &TypedVariableNode,
        is_static: bool,
        value: &TestNode,
        mutability: MutableType,
    ) -> CompileResult<()> {
        let is_const = mutability.is_const_ref();
        let raw_type = assigned.get_type();
        let mut converter = get_converter(info, value, raw_type)?;
        let value_type = first(converter.return_type(info)?);
        let assigned_type = get_assigned(info, &value_type, raw_type, mutability)?;
        let assigned_name = assigned.get_variable().get_name();
        self.check_name(assigned_name)?;
        let needs_make_option = self.check_types(&assigned_type, &value_type)?;
        let const_value = converter.constant_return(info)?;
        if let (true, Option::Some(constant)) = (is_const, const_value) {
            self.add_constant(
                info,
                assigned_type,
                assigned_name.to_string(),
                needs_make_option,
                constant,
            )
        } else {
            bytes.extend(OptionTypeObject::maybe_wrap_bytes(
                converter.convert(info)?,
                needs_make_option,
            ));
            self.finish_assignment(
                info,
                bytes,
                is_static,
                assigned_type,
                assigned_name.to_string(),
                is_const,
                assigned,
            )
        }
    }

    fn convert_tuple(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        is_static: bool,
        mutability: MutableType,
        tuple_count: &mut usize,
        i: usize,
    ) -> CompileResult<()> {
        let is_const = mutability.is_const_ref();
        let types = self.node.get_types();
        let values = self.node.get_values();
        let mut converter = values[i].test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        let argc = check_tuple(info, &ret_type, &values[i])?;
        let value_types = self.expand_zero_tuple(info, &mut converter)?;
        assert_eq!(argc, value_types.len());
        bytes.extend(converter.convert(info)?);
        bytes.add(Bytecode::UnpackTuple());
        for (j, value_type) in value_types.into_iter().enumerate().rev() {
            let assigned = &types[i - *tuple_count + j];
            let raw_type = assigned.get_type();
            let assigned_type = get_assigned(info, &value_type, raw_type, mutability)?;
            let assigned_name = assigned.get_variable().get_name();
            self.check_name(assigned_name)?;
            let needs_make_option = self.check_types(&assigned_type, &value_type)?;
            if needs_make_option {
                bytes.add(Bytecode::MakeOption());
            }
            self.finish_assignment(
                info,
                bytes,
                is_static,
                assigned_type,
                assigned_name.to_string(),
                is_const,
                assigned,
            )?;
        }
        *tuple_count += argc - 1;
        Ok(())
    }

    fn check_types(
        &self,
        assigned_type: &TypeObject,
        value_type: &TypeObject,
    ) -> CompileResult<bool> {
        if !assigned_type.is_superclass(value_type) {
            if OptionTypeObject::needs_and_super(assigned_type, value_type) {
                Ok(true)
            } else {
                Err(CompilerException::of(
                    format!(
                        "Object of type '{}' cannot be assigned to object of type '{}'",
                        value_type.name(),
                        assigned_type.name()
                    ),
                    self.node,
                )
                .into())
            }
        } else {
            Ok(false)
        }
    }

    fn add_constant(
        &self,
        info: &mut CompilerInfo,
        assigned_type: TypeObject,
        assigned_name: String,
        needs_make_option: bool,
        constant: LangConstant,
    ) -> CompileResult<()> {
        info.check_definition(&assigned_name, self.node)?;
        if needs_make_option {
            let constant = OptionConstant::new(constant);
            info.add_constant_variable(
                assigned_name,
                assigned_type,
                constant.into(),
                self.node.line_info().clone(),
            );
        } else {
            info.add_constant_variable(
                assigned_name,
                assigned_type,
                constant,
                self.node.line_info().clone(),
            );
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn finish_assignment(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        is_static: bool,
        assigned_type: TypeObject,
        assigned_name: String,
        is_const: bool,
        line_info: impl Lined,
    ) -> CompileResult<()> {
        info.check_definition(&assigned_name, self.node)?;
        if is_static && !is_const {
            return Err(mut_static_exception(line_info).into());
        }
        let index = if is_static {
            info.add_static_var(
                assigned_name,
                assigned_type,
                true,
                self.node.line_info().clone(),
            )
        } else {
            info.add_variable(
                assigned_name,
                assigned_type,
                is_const,
                self.node.line_info().clone(),
            )
        };
        bytes.add(if is_static {
            Bytecode::StoreStatic(index.into())
        } else {
            Bytecode::Store(index.into())
        });
        Ok(())
    }

    fn check_name(&self, name: &str) -> CompileResult<()> {
        if FORBIDDEN_NAMES.contains(&name) {
            Err(CompilerException::of(format!("Illegal name {}", name), self.node).into())
        } else {
            Ok(())
        }
    }

    fn expand_zero_tuple(
        &self,
        info: &mut CompilerInfo,
        value_converter: &mut impl ConverterTest,
    ) -> CompileTypes {
        let val_t = value_converter.return_type(info)?;
        if self.node.get_values().get_vararg(0).unwrap().is_empty() {
            Ok(val_t)
        } else if let TypeObject::Tuple(tup) = &val_t[0] {
            Ok(tup.get_generics().to_vec())
        } else {
            Err(CompilerException::of(
                format!(
                    "Vararg used on non-tuple argument (returned type '{}')",
                    val_t[0].name()
                ),
                &self.node.get_values()[0],
            )
            .into())
        }
    }
}

fn add_static(bytes: &mut BytecodeList, is_static: bool) -> Option<Label> {
    is_static.then(|| {
        let label = Label::new();
        bytes.add(Bytecode::DoStatic(label.clone().into()));
        label
    })
}

fn get_converter<'a>(
    info: &mut CompilerInfo,
    value: &'a TestNode,
    raw_type: &TypeLikeNode,
) -> CompileResult<TestConverter<'a>> {
    match raw_type {
        TypeLikeNode::Type(ty) => Ok(value.test_conv_expected(1, vec![info.convert_type(ty)?])),
        TypeLikeNode::Var(_) => Ok(value.test_converter(1)),
    }
}

fn get_assigned(
    info: &mut CompilerInfo,
    value_type: &TypeObject,
    raw_type: &TypeLikeNode,
    mutability: MutableType,
) -> CompileResult<TypeObject> {
    let converted; // Keep converted alive until the end of the function
    let non_const = match raw_type {
        TypeLikeNode::Type(ty) => {
            converted = info.convert_type(ty)?;
            &converted
        }
        TypeLikeNode::Var(_) => value_type,
    };
    Ok(if mutability.is_const_type() {
        non_const.make_const()
    } else {
        non_const.make_mut()
    })
}

fn check_tuple(
    info: &mut CompilerInfo,
    ret_type: &TypeObject,
    node: impl Lined,
) -> CompileResult<usize> {
    if let TypeObject::Tuple(ty) = ret_type {
        Ok(ty.get_generics().len())
    } else if ret_type.operator_info(OpSpTypeNode::Iter, info)?.is_some() {
        Err(CompilerException::of(
            "Vararg on declared assignment does not work for iterables, only tuples",
            node,
        )
        .into())
    } else {
        Err(CompilerException::of(
            format!("Vararg expected tuple, got type '{}'", ret_type.name()),
            node,
        )
        .into())
    }
}

fn mut_static_exception(line_info: impl Lined) -> CompilerException {
    CompilerException::of(
        "Local static variable may not be 'mut' or 'mref'",
        line_info,
    )
}

base_convertible!(DeclaredAssignmentNode, DeclaredAssignConverter);
