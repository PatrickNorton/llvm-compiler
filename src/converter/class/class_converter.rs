use derive_new::new;
use itertools::Itertools;

use crate::converter::annotation::{impl_annotatable, AnnotatableConverter};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::base_convertible;
use crate::converter::default_holder::DefaultHolder;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::CompilerException;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::{StdTypeObject, TypeObject, UserType, UserTypeLike};
use crate::converter::{CompileBytes, CompileResult};
use crate::parser::annotation::AnnotatableRef;
use crate::parser::class_def::ClassDefinitionNode;
use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;

use super::converter_holder::ConverterHolder;
use super::{check_contract, convert_supers, ensure_proper_inheritance, ClassConverterBase};

#[derive(Debug, new)]
pub struct ClassConverter<'a> {
    node: &'a ClassDefinitionNode,
}

impl<'a> ClassConverterBase<'a> for ClassConverter<'a> {
    type Converted = ClassDefinitionNode;

    fn get_node(&self) -> &'a Self::Converted {
        self.node
    }
}

impl<'a> AnnotatableConverter<'a> for ClassConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::BaseClass(BaseClassRef::Class(self.node))
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let supers = info.types_of(self.node.get_superclasses())?;
        let mut converter = ConverterHolder::new();
        let descriptors = self.node.get_descriptors();
        let is_final = !descriptors.contains(&DescriptorNode::Nonfinal);
        let has_type = info.get_type(self.node.str_name()).is_some();
        let type_val;
        if !has_type {
            let true_supers = convert_supers(self.node, supers)?;
            let generics = GenericInfo::parse(info, self.node.get_name().get_subtypes())?;
            type_val = StdTypeObject::new(
                self.node.str_name().to_string(),
                // FIXME: Remove unnecessary clone
                Some(true_supers.iter().cloned().map_into().collect()),
                generics,
                is_final,
            );
            let user_type = type_val.clone().into();
            ensure_proper_inheritance(self.node, &user_type, &true_supers)?;
            info.add_type(user_type.into());
            let is_const = self.node.get_descriptors().contains(&DescriptorNode::Const);
            if !is_const {
                self.check_const_supers(&type_val, &true_supers)?;
            }
            self.parse_into_object(info, &mut converter, &type_val, is_const, None)?;
            info.reserve_class(type_val.clone().into());
        } else {
            type_val = info
                .get_type_obj(self.node.str_name())
                .unwrap()
                .clone()
                .try_into()
                .unwrap();
            let type_obj = TypeObject::from(type_val.clone());
            info.access_handler_mut().add_cls(type_obj.clone());
            info.access_handler_mut()
                .add_super(super_type(&type_val).clone());
            info.add_local_types(type_obj, type_val.get_generic_info().get_param_map());
            let result = self.parse_statements(info, &mut converter, None);
            info.access_handler_mut().remove_cls();
            info.access_handler_mut().remove_super();
            info.remove_local_types();
            result?;
        };
        let super_constants = self.get_super_constants(info, type_val.get_supers())?;
        let ty = UserType::from(type_val);
        check_contract(self.node, &ty, ty.get_supers())?;
        self.put_in_info(info, ty, "class", None, super_constants, converter)?;
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

impl_annotatable!(ClassConverter<'a>);

impl<'a> ClassConverter<'a> {
    pub fn complete_type(
        &self,
        info: &mut CompilerInfo,
        obj: &StdTypeObject,
        reserve: bool,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<u16> {
        let mut converter = ConverterHolder::new();
        obj.get_generic_info()
            .re_parse(info, self.node.get_name().get_subtypes())?;
        obj.set_generic_parent();
        let supers = convert_supers(self.node, info.types_of(self.node.get_superclasses())?)?;
        obj.set_supers(supers.into_iter().map_into().collect());
        let is_const = self.node.get_descriptors().contains(&DescriptorNode::Const);
        if !is_const {
            // TODO: Remove cloning
            let supers = obj
                .get_supers()
                .iter()
                .map(|x| UserType::try_from(x.clone()).unwrap())
                .collect_vec();
            self.check_const_supers(obj, &supers)?;
        }
        self.parse_into_object(info, &mut converter, obj, is_const, Some(defaults))?;
        Ok(if reserve {
            info.reserve_class(obj.clone().into())
        } else {
            u16::MAX
        })
    }

    fn check_const_supers(&self, ty: &StdTypeObject, supers: &[UserType]) -> CompileResult<()> {
        for cls in supers {
            if cls.const_semantics() {
                return Err(CompilerException::of(
                    format!(
                        "Union '{}' inherits from the const class '{}', but is not itself const",
                        ty.name(),
                        cls.name()
                    ),
                    self.node,
                )
                .into());
            }
        }
        Ok(())
    }

    fn parse_into_object(
        &self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        obj: &StdTypeObject,
        is_const: bool,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        info.access_handler_mut().add_cls(obj.clone().into());
        info.access_handler_mut().add_super(super_type(obj).clone());
        info.add_local_types(obj.clone().into(), obj.get_generic_info().get_param_map());
        let result = self.parse_inner(info, converter, obj, is_const, defaults);
        info.access_handler_mut().remove_cls();
        info.access_handler_mut().remove_super();
        info.remove_local_types();
        result
    }

    fn parse_inner(
        &self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        obj: &StdTypeObject,
        is_const: bool,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        self.parse_statements(info, converter, defaults)?;
        if is_const {
            if !class_is_constant(converter) {
                return Err(CompilerException::of(
                    format!("Cannot make class '{}' const", obj.name()),
                    self.node,
                )
                .into());
            }
            obj.is_const_class();
        }
        obj.set_operators(converter.ops.get_operator_infos());
        obj.set_static_operators(converter.ops.static_operator_infos());
        converter.check_attributes()?;
        obj.set_attributes(converter.all_attrs());
        obj.set_static_attributes(converter.static_attrs());
        obj.seal();
        Ok(())
    }
}

fn super_type(obj: &StdTypeObject) -> &TypeObject {
    todo!()
}

fn class_is_constant(converter: &ConverterHolder<'_>) -> bool {
    todo!()
}

base_convertible!(ClassDefinitionNode, ClassConverter);
