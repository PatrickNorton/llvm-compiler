use std::collections::hash_map::Entry;
use std::collections::HashMap;

use itertools::Itertools;

use crate::converter::access_handler::AccessLevel;
use crate::converter::annotation::{self, impl_annotatable, AnnotatableConverter};
use crate::converter::argument::{Argument, ArgumentInfo};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::base_convertible;
use crate::converter::default_holder::DefaultHolder;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::mutable::MutableType;
use crate::converter::type_obj::{TypeObject, UnionTypeObject, UserType, UserTypeLike};
use crate::converter::{CompileBytes, CompileResult};
use crate::parser::annotation::{AnnotatableNode, AnnotatableRef};
use crate::parser::base::IndependentNode;
use crate::parser::class_def::ClassStatementNode;
use crate::parser::declaration::DeclarationNode;
use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::return_stmt::ReturnStatementNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::union_def::UnionDefinitionNode;
use crate::parser::variable::VariableNode;
use crate::parser::variant::VariantCreationNode;
use crate::util::reborrow_option;

use super::attribute::AttributeInfo;
use super::converter_holder::ConverterHolder;
use super::method::RawMethod;
use super::{convert_supers, ensure_proper_inheritance, ClassConverterBase};

#[derive(Debug)]
pub struct UnionConverter<'a> {
    node: &'a UnionDefinitionNode,
    variants: HashMap<String, (u16, AttributeInfo)>,
}

impl<'a> UnionConverter<'a> {
    pub fn new(node: &'a UnionDefinitionNode) -> Self {
        Self {
            node,
            variants: HashMap::new(),
        }
    }
}

impl<'a> ClassConverterBase<'a> for UnionConverter<'a> {
    type Converted = UnionDefinitionNode;

    fn get_node(&self) -> &'a Self::Converted {
        self.node
    }
}

impl<'a> AnnotatableConverter<'a> for UnionConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::BaseClass(BaseClassRef::Union(self.node))
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut converter = ConverterHolder::new();
        let has_type = info.has_type(self.node.get_name().str_name());
        let type_val;
        if !has_type {
            let supers = info.types_of(self.node.get_superclasses())?;
            let true_supers = convert_supers(self.node, supers)?;
            let generics = GenericInfo::parse(info, self.node.get_name().get_subtypes())?;
            type_val = UnionTypeObject::new(
                self.node.get_name().str_name().to_string(),
                // FIXME: Remove unnecessary clone
                Some(true_supers.iter().cloned().map_into().collect()),
                generics,
            );
            let user_type = type_val.clone().into();
            ensure_proper_inheritance(self.node, &user_type, &true_supers)?;
            info.add_type(user_type.into());
            let is_const = self.node.get_descriptors().contains(&DescriptorNode::Const);
            if !is_const {
                self.check_const_supers(&type_val, &true_supers)?;
            }
            self.parse_into_object(info, &mut converter, &type_val, is_const, None)?;
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
            let result = self.parse_stmts(info, &mut converter, None);
            info.remove_local_types();
            info.access_handler_mut().remove_cls();
            result?
        }
        if self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Nonfinal)
        {
            return Err(CompilerException::of("Union may not be nonfinal", self.node).into());
        }
        let super_constants = self.get_super_constants(info, type_val.get_supers())?;
        if has_type {
            self.put_in_info(
                info,
                type_val.into(),
                "union",
                Some(self.convert_variants()),
                super_constants,
                converter,
            )?;
        } else {
            self.add_to_info(
                info,
                type_val.into(),
                "union",
                Some(self.convert_variants()),
                super_constants,
                converter,
            )?;
        }
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

impl_annotatable!(UnionConverter<'a>);

impl<'a> UnionConverter<'a> {
    pub fn complete_type(
        &mut self,
        info: &mut CompilerInfo,
        obj: &UnionTypeObject,
        reserve: bool,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<u16> {
        let mut converter = ConverterHolder::new();
        let supers = convert_supers(self.node, info.types_of(self.node.get_superclasses())?)?;
        let is_const = self.node.get_descriptors().contains(&DescriptorNode::Const);
        if is_const {
            self.check_const_supers(obj, &supers)?;
        }
        obj.set_supers(supers.iter().map(|x| x.clone().into()).collect_vec());
        ensure_proper_inheritance(self.node, &obj.clone().into(), &supers)?;
        info.access_handler_mut().add_cls(obj.clone().into());
        info.add_local_types(obj.clone().into(), obj.get_generic_info().get_param_map());
        let result = self.parse_into_object(info, &mut converter, obj, is_const, Some(defaults));
        info.access_handler_mut().remove_cls();
        info.remove_local_types();
        result?;
        Ok(if reserve {
            info.reserve_class(obj.clone().into())
        } else {
            u16::MAX
        })
    }

    // TODO: Deduplicate
    fn parse_stmts(
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
            if let Option::Some(annotations) = AnnotatableNode::try_get_annotations(stmt) {
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
        stmt: &'a ClassStatementNode,
        converter: &mut ConverterHolder<'a>,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        match stmt {
            ClassStatementNode::Declaration(decl) => {
                if decl.get_descriptors().contains(&DescriptorNode::Static) {
                    converter.attrs.parse(info, decl)
                } else {
                    self.add_variant(info, decl)
                }
            }
            ClassStatementNode::DeclaredAssign(decl) => {
                if decl.get_descriptors().contains(&DescriptorNode::Static) {
                    converter.attrs.parse_assign(info, decl)
                } else {
                    Err(
                        CompilerException::of("Non-static variables not allowed in unions", decl)
                            .into(),
                    )
                }
            }
            _ => self.parse_statement(info, stmt, converter, defaults),
        }
    }

    fn add_variant(&mut self, info: &CompilerInfo, decl: &DeclarationNode) -> CompileResult<()> {
        let name = decl.get_name().get_name();
        let ty = info.convert_type(decl.get_type().as_type())?;
        let variant_count = self.variants.len();
        match self.variants.entry(name.to_string()) {
            Entry::Occupied(o) => Err(CompilerException::double_def(name, &o.get().1, decl).into()),
            Entry::Vacant(v) => {
                v.insert((
                    variant_count as u16,
                    AttributeInfo::new(
                        false,
                        AccessLevel::Public,
                        MutableType::Standard,
                        ty,
                        decl.line_info().clone(),
                    ),
                ));
                Ok(())
            }
        }
    }

    fn parse_into_object(
        &mut self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        obj: &UnionTypeObject,
        is_const: bool,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        info.add_local_types(obj.clone().into(), obj.get_generic_info().get_param_map());
        if is_const && !class_is_constant(converter) {
            return Err(CompilerException::of(
                format!("Cannot make union '{}' const", obj.name()),
                self.node,
            )
            .into());
        }
        obj.is_const_class(is_const);
        self.parse_stmts(info, converter, defaults)?;
        converter
            .methods
            .add_union_methods(self.variant_methods(info, obj))?;
        obj.set_operators(converter.ops.get_operator_infos());
        obj.set_static_operators(converter.ops.static_operator_infos());
        converter.check_attributes()?;
        obj.set_attributes(self.with_variant_infos(converter.all_attrs()));
        obj.set_static_attributes(self.with_static_variants(info, converter.static_attrs(), obj));
        obj.set_variants(self.get_variants());
        obj.seal(Some(info.global_info()), Some(info.builtins()));
        Ok(())
    }

    fn with_variant_infos(
        &self,
        mut vars: HashMap<String, AttributeInfo>,
    ) -> HashMap<String, AttributeInfo> {
        vars.reserve(self.variants.len());
        vars.extend(self.variants.iter().map(|(name, (_, info))| {
            let fn_info = TypeObject::optional(info.get_type().clone());
            (
                name.clone(),
                AttributeInfo::new(
                    false,
                    AccessLevel::Public,
                    MutableType::Standard,
                    fn_info,
                    info.line_info().clone(),
                ),
            )
        }));
        vars
    }

    fn with_static_variants(
        &self,
        info: &CompilerInfo,
        mut vars: HashMap<String, AttributeInfo>,
        self_type: &UnionTypeObject,
    ) -> HashMap<String, AttributeInfo> {
        vars.reserve(self.variants.len());
        vars.extend(self.variants.iter().map(|(name, (_, method_info))| {
            let fn_info =
                variant_info(info, method_info.get_type(), self_type, name.clone()).to_callable();
            (
                name.clone(),
                AttributeInfo::new(
                    false,
                    AccessLevel::Public,
                    MutableType::Standard,
                    fn_info,
                    method_info.line_info().clone(),
                ),
            )
        }));
        vars
    }

    fn get_variants(&self) -> Vec<(String, TypeObject)> {
        let mut result = vec![None; self.variants.len()];
        for (name, (index, attr)) in &self.variants {
            assert!(result[*index as usize].is_none());
            result[*index as usize] = Some((name.clone(), attr.get_type().clone()))
        }
        result.into_iter().map(|x| x.unwrap()).collect()
    }

    fn check_const_supers(
        &self,
        type_val: &UnionTypeObject,
        supers: &[UserType],
    ) -> CompileResult<()> {
        for cls in supers {
            if cls.const_semantics() {
                return Err(CompilerException::of(
                    format!(
                        "Union '{}' inherits from the const class '{}', but is not itself const",
                        type_val.name(),
                        cls.name()
                    ),
                    self.node,
                )
                .into());
            }
        }
        Ok(())
    }

    fn convert_variants(&self) -> Vec<String> {
        let mut result = vec![None; self.variants.len()];
        for (name, (variant_no, _)) in &self.variants {
            assert!(result[*variant_no as usize].is_none());
            result[*variant_no as usize] = Some(name.clone());
        }
        result.into_iter().map(Option::unwrap).collect()
    }

    fn variant_methods(
        &self,
        info: &CompilerInfo,
        self_type: &UnionTypeObject,
    ) -> HashMap<String, RawMethod<'a>> {
        self.variants
            .iter()
            .map(|(name, (variant_no, attr_info))| {
                let fn_info = variant_info(info, attr_info.get_type(), self_type, name.clone());
                let self_var = VariableNode::new(LineInfo::empty(), self_type.name().to_string());
                let variant = VariableNode::new(LineInfo::empty(), VARIANT_NAME.to_string());
                let stmt = VariantCreationNode::new(
                    self.node.line_info().clone(),
                    TestNode::Name(NameNode::Variable(self_var)),
                    name.clone(),
                    *variant_no,
                    TestNode::Name(NameNode::Variable(variant)),
                );
                let list = TestListNode::from_one(TestNode::Variant(Box::new(stmt)));
                let ret_stmt = ReturnStatementNode::new(
                    self.node.line_info().clone(),
                    list,
                    TestNode::empty(),
                );
                let body = StatementBodyNode::new(
                    LineInfo::empty(),
                    vec![IndependentNode::Return(ret_stmt)],
                );
                (
                    name.clone(),
                    RawMethod::new(
                        AccessLevel::Public,
                        false,
                        fn_info,
                        body.into(),
                        self.node.line_info().clone(),
                    ),
                )
            })
            .collect()
    }
}

fn class_is_constant(converter: &ConverterHolder) -> bool {
    converter
        .attrs
        .get_vars()
        .iter()
        .all(|(_, info)| info.get_mut_type() != MutableType::Standard)
}

const VARIANT_NAME: &str = "val";

fn variant_info(
    info: &CompilerInfo,
    val: &TypeObject,
    ty: &UnionTypeObject,
    name: String,
) -> FunctionInfo {
    if val.same_base_type(info.builtins().null_type()) {
        FunctionInfo::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            ArgumentInfo::empty(),
            vec![ty.make_mut().into()],
        )
    } else {
        let arg = Argument::new(VARIANT_NAME.to_string(), val.clone());
        FunctionInfo::new(
            LineInfo::empty(),
            name,
            false,
            GenericInfo::empty(),
            ArgumentInfo::of_args([arg]),
            vec![ty.make_mut().into()],
        )
    }
}

base_convertible!(UnionDefinitionNode, UnionConverter);
