use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::converter::access_handler::AccessLevel;
use crate::converter::builtins::OBJECT;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::CompileResult;
use crate::parser::type_node::TypeNode;

use super::macros::{arc_eq_hash, try_from_type_obj, type_obj_from};
use super::{ListTypeObject, TypeObject};

#[derive(Debug, Clone)]
pub struct TemplateParam {
    value: Arc<TemplateInner>,
}

#[derive(Debug)]
struct TemplateInner {
    name: String,
    index: usize,
    bound: TypeObject,
    is_vararg: bool,
    typedef_name: Option<String>,
    parent: OnceCell<TypeObject>,
}

impl TemplateParam {
    pub fn new(name: String, index: usize, bound: TypeObject) -> Self {
        Self {
            value: Arc::new(TemplateInner {
                name,
                index,
                bound,
                is_vararg: false,
                typedef_name: None,
                parent: OnceCell::new(),
            }),
        }
    }

    pub fn new_vararg(name: String, index: usize) -> Self {
        Self {
            value: Arc::new(TemplateInner {
                name,
                index,
                bound: TypeObject::list([]),
                is_vararg: true,
                typedef_name: None,
                parent: OnceCell::new(),
            }),
        }
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.value
            .typedef_name
            .as_deref()
            .unwrap_or(&self.value.name)
            .into()
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        self.value.name.as_str().into()
    }

    pub fn is_vararg(&self) -> bool {
        self.value.is_vararg
    }

    pub fn get_index(&self) -> usize {
        self.value.index
    }

    pub fn get_bound(&self) -> &TypeObject {
        &self.value.bound
    }

    pub fn get_parent(&self) -> &TypeObject {
        self.value
            .parent
            .get()
            .expect("Should have given parent before acquiring it")
    }

    pub fn set_parent(&self, parent: TypeObject) {
        self.value
            .parent
            .set(parent)
            .expect("Parent should only be written to once")
    }

    pub fn parse_generics(
        info: &mut CompilerInfo,
        generics: &[TypeNode],
    ) -> CompileResult<HashMap<String, TemplateParam>> {
        generics
            .iter()
            .enumerate()
            .map(|(i, generic)| {
                if generic.get_subtypes().is_empty() {
                    Ok((
                        generic.str_name().to_string(),
                        TemplateParam::new(generic.str_name().to_string(), i, OBJECT.into()),
                    ))
                } else {
                    let bounds = TypeObject::union_of(info, info.types_of(generic.get_subtypes())?);
                    Ok((
                        generic.str_name().to_string(),
                        TemplateParam::new(generic.str_name().to_string(), i, bounds),
                    ))
                }
            })
            .collect()
    }

    pub fn attr_type(
        &self,
        value: &str,
        access: AccessLevel,
    ) -> CompileResult<Option<Cow<'_, TypeObject>>> {
        self.value.bound.attr_type_access(value, access)
    }

    pub fn static_attr_type(
        &self,
        value: &str,
        access: AccessLevel,
    ) -> Option<Cow<'_, TypeObject>> {
        self.value.bound.static_attr_type(value, access)
    }

    pub fn generify_with(&self, parent: &TypeObject, values: Vec<TypeObject>) -> TypeObject {
        let self_parent = self.value.parent.get().unwrap();
        let index = self.value.index;
        if self_parent.same_base_type(parent) {
            if self.value.is_vararg && <&ListTypeObject>::try_from(&values[index]).is_err() {
                TypeObject::list([values[index].clone()])
            } else {
                values[index].clone()
            }
        } else {
            self.clone().into()
        }
    }

    pub fn get_defined(&self) -> Option<Box<dyn Iterator<Item = Cow<'_, str>> + '_>> {
        self.value.bound.get_defined()
    }

    pub fn static_defined(&self) -> Option<Box<dyn Iterator<Item = &'_ str> + '_>> {
        self.value.bound.static_defined()
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(TemplateInner {
                name: self.value.name.clone(),
                index: self.value.index,
                bound: self.value.bound.clone(),
                is_vararg: self.value.is_vararg,
                typedef_name: Some(name),
                parent: self.value.parent.clone(),
            }),
        }
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.hash(state)
    }
}

arc_eq_hash!(TemplateParam);

type_obj_from!(TemplateParam, Template);
try_from_type_obj!(TemplateParam, Template);
