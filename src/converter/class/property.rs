use std::collections::HashMap;

use derive_new::new;

use crate::converter::access_handler::AccessLevel;
use crate::converter::annotation::{self, AnnotatableConverter};
use crate::converter::argument::{Argument, ArgumentInfo};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::mutable::MutableType;
use crate::converter::CompileResult;
use crate::parser::annotation::AnnotatableRef;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::property::PropertyDefinitionNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::util::tee_map::TeeMap;

use super::attribute::AttributeInfo;
use super::method::RawMethod;

#[derive(Debug)]
pub struct PropertyConverter<'a> {
    properties: HashMap<&'a str, (AttributeInfo, PropertyInfo<'a>)>,
}

#[derive(Debug, new)]
struct PropertyConvInner<'a, 'b> {
    inner: &'a mut PropertyConverter<'b>,
    node: &'b PropertyDefinitionNode,
}

#[derive(Debug, new)]
pub(super) struct PropertyInfo<'a> {
    getter: &'a StatementBodyNode,
    setter: &'a StatementBodyNode,
    setter_args: ArgumentInfo,
    line_info: LineInfo,
}

impl<'a> PropertyConverter<'a> {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
        }
    }

    pub(super) fn get_properties(&self) -> &HashMap<&'a str, (AttributeInfo, PropertyInfo<'a>)> {
        &self.properties
    }

    pub fn parse(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a PropertyDefinitionNode,
    ) -> CompileResult<()> {
        annotation::convert_annotatable(&mut PropertyConvInner::new(self, node), info).map(|_| ())
    }

    pub fn parse_inner(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a PropertyDefinitionNode,
    ) -> CompileResult<()> {
        let name = node.get_name().get_name();
        let ty = info.convert_type(node.get_type().as_type())?;
        if let Option::Some(prop) = self.properties.get(name) {
            return Err(CompilerException::double_def(name, &prop.0, node).into());
        }
        let access_level = AccessLevel::from_descriptors(node.get_descriptors());
        assert_eq!(
            MutableType::from_descriptors(node.get_descriptors()),
            MutableType::Standard,
            "Properties should never be mut"
        );
        // FIXME: Deal with double-creation of ArgumentInfo here
        let arg_info = ArgumentInfo::of(node.get_set_args(), info, None)?;
        let has_args = !arg_info.is_empty();
        let fn_info = FunctionInfo::from(arg_info);
        if has_args && !fn_info.matches(&[Argument::new(String::new(), ty.clone())]) {
            return Err(CompilerException::of(
                "Invalid argument info for setter",
                node.get_set_args(),
            )
            .into());
        }
        let attr_info = AttributeInfo::new(
            false,
            access_level,
            if node.get_set().is_empty() {
                MutableType::Standard
            } else {
                MutableType::Mut
            },
            ty,
            node.line_info().clone(),
        );
        let prop_info = PropertyInfo::new(
            node.get_get(),
            node.get_set(),
            ArgumentInfo::of(node.get_set_args(), info, None)?,
            node.line_info().clone(),
        );
        self.properties.insert(name, (attr_info, prop_info));
        Ok(())
    }

    pub fn split(
        self,
    ) -> (
        impl Iterator<Item = (&'a str, RawMethod<'a>)>,
        impl Iterator<Item = (&'a str, RawMethod<'a>)>,
        impl Iterator<Item = (&'a str, RawMethod<'a>)>,
        impl Iterator<Item = (&'a str, RawMethod<'a>)>,
    ) {
        let (a, b) = Self::split_map(self.properties);
        let (c, d) = Self::split_map(HashMap::new());
        (a, b, c, d)
    }

    fn split_map(
        properties: HashMap<&'a str, (AttributeInfo, PropertyInfo<'a>)>,
    ) -> (
        impl Iterator<Item = (&'a str, RawMethod<'a>)>,
        impl Iterator<Item = (&'a str, RawMethod<'a>)>,
    ) {
        properties
            .into_iter()
            .map(|(k, (property, info))| {
                let getter_info = FunctionInfo::from_returns(vec![property.get_type().clone()]);
                let setter_info = FunctionInfo::with_args(info.setter_args, vec![]);
                let getter_method = RawMethod::new(
                    property.get_access_level(),
                    false,
                    getter_info,
                    info.getter.into(),
                    info.line_info.clone(),
                );
                let setter_method = RawMethod::new(
                    property.get_access_level(),
                    false,
                    setter_info,
                    info.setter.into(),
                    info.line_info.clone(),
                );
                (k, (getter_method, setter_method))
            })
            .tee_map(|(k, (v1, v2))| ((k, v1), (k, v2)))
    }
}

impl<'a, 'b> AnnotatableConverter<'b> for PropertyConvInner<'a, 'b> {
    fn get_annotatable(&self) -> AnnotatableRef<'b> {
        AnnotatableRef::Property(self.node)
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.inner.parse_inner(info, self.node)?;
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}
