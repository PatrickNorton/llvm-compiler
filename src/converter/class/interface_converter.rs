use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::converter::annotation::{impl_annotatable, AnnotatableConverter};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::base_convertible;
use crate::converter::default_holder::DefaultHolder;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::CompilerException;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::{
    InterfaceAttrInfo, InterfaceFnInfo, InterfaceType, TypeObject, UserType, UserTypeLike,
};
use crate::converter::{annotation, CompileBytes, CompileResult};
use crate::parser::annotation::{AnnotatableNode, AnnotatableRef};
use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::generic_stmt::GenericDefinitionNode;
use crate::parser::interface::{InterfaceDefinitionNode, InterfaceStatementNode};
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::reborrow_option;

use super::converter_holder::ConverterHolder;
use super::{convert_supers, ensure_proper_inheritance, AttributeInfo, MethodInfo};

use super::ClassConverterBase;

#[derive(Debug)]
pub struct InterfaceConverter<'a> {
    node: &'a InterfaceDefinitionNode,
    generic_ops: HashSet<OpSpTypeNode>,
    generic_attrs: HashSet<&'a str>,
}

impl<'a> InterfaceConverter<'a> {
    pub fn new(node: &'a InterfaceDefinitionNode) -> Self {
        Self {
            node,
            generic_ops: HashSet::new(),
            generic_attrs: HashSet::new(),
        }
    }
}

impl<'a> ClassConverterBase<'a> for InterfaceConverter<'a> {
    type Converted = InterfaceDefinitionNode;

    fn get_node(&self) -> &'a Self::Converted {
        self.node
    }
}

impl<'a> AnnotatableConverter<'a> for InterfaceConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::BaseClass(BaseClassRef::Interface(self.node))
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.generic_ops.clear();
        self.generic_attrs.clear();
        let mut converter = ConverterHolder::new();
        let true_supers = convert_supers(self.node, info.types_of(self.node.get_superclasses())?)?;
        let type_val;
        let has_type = info.has_type(self.node.get_name().str_name());
        if !has_type {
            let generics = GenericInfo::parse(info, self.node.get_name().get_subtypes())?;
            type_val = InterfaceType::new(
                self.node.get_name().str_name().to_string(),
                generics,
                Some(true_supers.iter().cloned().map_into().collect()),
            );
            let user_type = UserType::from(type_val.clone());
            type_val.set_generic_parent();
            ensure_proper_inheritance(self.node, &user_type, &true_supers)?;
            info.add_type(user_type.into());
            self.parse_into_object(info, &mut converter, type_val.clone(), None)?;
            if self.node.get_descriptors().contains(&DescriptorNode::Auto) {
                return Err(CompilerException::of(
                    "Auto interfaces may only be defined at top level",
                    self.node,
                )
                .into());
            }
        } else {
            type_val = info
                .get_type_obj(self.node.get_name().str_name())
                .unwrap()
                .clone()
                .try_into()
                .unwrap();
            let type_obj = TypeObject::from(type_val.clone());
            info.access_handler_mut().add_cls(type_obj.clone());
            info.add_local_types(type_obj, type_val.get_generic_info().get_param_map());
            let result = self.parse_statements(info, &mut converter, None);
            info.access_handler_mut().remove_cls();
            info.remove_local_types();
            result?;
        }
        let super_constants = self.get_super_constants(info, type_val.get_supers())?;
        // TODO: converter.check_attributes()
        if has_type && !self.node.get_descriptors().contains(&DescriptorNode::Auto) {
            self.put_in_info(
                info,
                type_val.into(),
                "interface",
                None,
                super_constants,
                converter,
            )?;
        } else {
            self.add_to_info(
                info,
                type_val.into(),
                "interface",
                None,
                super_constants,
                converter,
            )?;
        }
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

impl_annotatable!(InterfaceConverter<'a>);

impl<'a> InterfaceConverter<'a> {
    pub fn complete_type(
        &mut self,
        info: &mut CompilerInfo,
        obj: &InterfaceType,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<u16> {
        self.complete_without_reserving(info, obj, defaults)?;
        Ok(info.reserve_class(obj.clone().into()))
    }

    pub fn complete_without_reserving(
        &mut self,
        info: &mut CompilerInfo,
        obj: &InterfaceType,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<()> {
        let mut converter = ConverterHolder::new();
        obj.get_generic_info()
            .re_parse(info, self.node.get_name().get_subtypes())?;
        obj.set_generic_parent();
        info.access_handler_mut().add_cls(obj.clone().into());
        info.add_local_types(obj.clone().into(), obj.get_generic_info().get_param_map());
        let result = self.parse_into_object(info, &mut converter, obj.clone(), Some(defaults));
        info.access_handler_mut().remove_cls();
        info.remove_local_types();
        result
    }

    fn parse_into_object(
        &mut self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        obj: InterfaceType,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        self.parse_statements(info, converter, defaults)?;
        converter.check_attributes()?;
        obj.set_operators(operator_infos(
            converter.ops.get_operator_infos(),
            &self.generic_ops,
        ));
        obj.set_static_operators(operator_infos(
            converter.ops.static_operator_infos(),
            &HashSet::new(),
        ));
        obj.set_attributes(attr_infos(converter.all_attrs(), &self.generic_attrs));
        obj.set_static_attributes(attr_infos(converter.static_attrs(), &HashSet::new()));
        obj.seal(Some(info.global_info()), Some(info.builtins()));
        Ok(())
    }

    // TODO: Deduplicate
    fn parse_statements(
        &mut self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        mut defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        let op_converter = &mut converter.ops;
        let operators = annotation::derive_operators(self.get_node().get_annotations())?;
        for op in operators {
            op_converter.parse_derived(info, op, &self.get_node().get_annotations()[0])?;
        }
        for stmt in self.get_node().get_body() {
            if let Option::Some(annotations) = AnnotatableNode::try_interface_annotations(stmt) {
                if annotation::should_compile(stmt, info, annotations)? {
                    self.parse_stmt(info, stmt, converter, reborrow_option(&mut defaults))?
                }
            } else {
                self.parse_stmt(info, stmt, converter, reborrow_option(&mut defaults))?
            }
        }
        Ok(())
    }

    fn parse_stmt(
        &mut self,
        info: &mut CompilerInfo,
        stmt: &'a InterfaceStatementNode,
        converter: &mut ConverterHolder<'a>,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        match stmt {
            InterfaceStatementNode::ClassStmt(stmt) => {
                self.parse_statement(info, stmt, converter, defaults)
            }
            InterfaceStatementNode::Generic(gen) => match gen {
                GenericDefinitionNode::Function(func) => {
                    self.generic_attrs.insert(func.get_name().get_name());
                    converter.methods.parse_generic(info, func, defaults)
                }
                GenericDefinitionNode::Operator(op) => {
                    self.generic_ops.insert(op.get_op_code().get_operator());
                    converter.ops.parse_generic(info, op, defaults)
                }
            },
        }
    }
}

fn operator_infos(
    args: HashMap<OpSpTypeNode, MethodInfo>,
    generics: &HashSet<OpSpTypeNode>,
) -> HashMap<OpSpTypeNode, InterfaceFnInfo> {
    args.into_iter()
        .map(|(x, y)| (x, InterfaceFnInfo::new(y, !generics.contains(&x))))
        .collect()
}

fn attr_infos(
    args: HashMap<String, AttributeInfo>,
    generics: &HashSet<&str>,
) -> HashMap<String, InterfaceAttrInfo> {
    args.into_iter()
        .map(|(x, y)| {
            let is_generic = generics.contains(x.as_str());
            (x, InterfaceAttrInfo::new(y, !is_generic))
        })
        .collect()
}

base_convertible!(InterfaceDefinitionNode, InterfaceConverter);
