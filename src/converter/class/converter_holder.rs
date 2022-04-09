use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use crate::converter::error::CompilerException;
use crate::converter::mutable::MutableType;
use crate::converter::CompileResult;
use crate::parser::line_info::Lined;

use super::attribute::AttributeConverter;
use super::method::{MethodConverter, RawMethod};
use super::operator_def::OperatorDefConverter;
use super::property::{PropertyConverter, PropertyInfo};
use super::AttributeInfo;

#[derive(Debug)]
pub struct ConverterHolder<'a> {
    pub attrs: AttributeConverter<'a>,
    pub methods: MethodConverter<'a>,
    pub ops: OperatorDefConverter<'a>,
    pub props: PropertyConverter<'a>,
}

impl<'a> ConverterHolder<'a> {
    pub fn new() -> Self {
        Self {
            attrs: AttributeConverter::new(),
            methods: MethodConverter::new(),
            ops: OperatorDefConverter::new(),
            props: PropertyConverter::new(),
        }
    }

    pub fn check_attributes(&self) -> CompileResult<()> {
        check_maps(
            self.attrs.get_vars(),
            self.methods.get_methods(),
            self.methods.get_static_methods(),
        )?;
        check_maps(
            self.attrs.get_static_vars(),
            self.methods.get_methods(),
            self.methods.get_static_methods(),
        )?;
        check_maps(
            self.methods.get_methods(),
            self.attrs.get_vars(),
            self.attrs.get_static_vars(),
        )?;
        check_maps(
            self.methods.get_static_methods(),
            self.attrs.get_vars(),
            self.attrs.get_static_vars(),
        )?;
        Ok(())
    }

    pub fn all_attrs(&self) -> HashMap<String, AttributeInfo> {
        merge_attrs(
            self.attrs.get_vars(),
            self.attrs.get_colons(),
            self.methods.get_methods(),
            self.props.get_properties(),
        )
    }

    pub fn static_attrs(&self) -> HashMap<String, AttributeInfo> {
        merge_attrs(
            self.attrs.get_static_vars(),
            self.attrs.get_static_colons(),
            self.methods.get_static_methods(),
            &HashMap::new(),
        )
    }
}

fn check_maps(
    vars: &HashMap<impl Borrow<str> + Eq + Hash, impl Lined>,
    methods: &HashMap<impl Borrow<str> + Eq + Hash, impl Lined>,
    static_methods: &HashMap<impl Borrow<str> + Eq + Hash, impl Lined>,
) -> CompileResult<()> {
    for (key, val) in vars {
        let key = key.borrow();
        if let Option::Some(method) = methods.get(key) {
            return Err(CompilerException::double_def(key, val, method).into());
        } else if let Option::Some(static_method) = static_methods.get(key) {
            return Err(CompilerException::double_def(key, val, static_method).into());
        }
    }
    Ok(())
}

fn merge_attrs(
    attrs: &HashMap<String, AttributeInfo>,
    colons: &HashMap<String, RawMethod<'_>>,
    methods: &HashMap<impl Borrow<str>, RawMethod<'_>>,
    properties: &HashMap<&str, (AttributeInfo, PropertyInfo<'_>)>,
) -> HashMap<String, AttributeInfo> {
    attrs
        .iter()
        .map(|(x, y)| (x.clone(), y.clone()))
        .chain(colons.iter().map(|(x, y)| (x.clone(), attr_info(y, false))))
        .chain(
            methods
                .iter()
                .map(|(x, y)| (x.borrow().to_string(), attr_info(y, true))),
        )
        .chain(
            properties
                .iter()
                .map(|(x, (y, _))| (x.to_string(), y.clone())),
        )
        .collect()
}

fn attr_info(method: &RawMethod<'_>, is_method: bool) -> AttributeInfo {
    let mut_type = if method.is_mut {
        MutableType::MutMethod
    } else {
        MutableType::Standard
    };
    AttributeInfo::new(
        is_method,
        method.access_level,
        mut_type,
        method.info.to_callable(),
        method.line_info.clone(),
    )
}

impl<'a> Default for ConverterHolder<'a> {
    fn default() -> Self {
        Self::new()
    }
}
