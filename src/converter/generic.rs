use std::collections::HashMap;

use derive_new::new;

use crate::parser::type_node::TypeNode;

use super::builtins::OBJECT;
use super::compiler_info::CompilerInfo;
use super::type_obj::{TemplateParam, TypeObject};
use super::CompileResult;

#[derive(Debug, new)]
pub struct GenericInfo {
    params: Vec<TemplateParam>,
    is_registered: bool,
}

impl GenericInfo {
    pub const fn empty() -> Self {
        Self {
            params: vec![],
            is_registered: false,
        }
    }

    pub fn parse(info: &mut CompilerInfo, generics: &[TypeNode]) -> CompileResult<GenericInfo> {
        if generics.is_empty() {
            return Ok(Self::empty());
        }
        let mut params = Vec::with_capacity(generics.len());
        for (i, generic) in generics.iter().enumerate() {
            let param = if generic.is_vararg() {
                TemplateParam::new_vararg(generic.str_name().to_string(), i)
            } else if generic.get_name().is_empty() {
                // [*T]
                assert!(generic.get_subtypes().len() == 1 && generic.get_subtypes()[0].is_vararg());
                let subtype = &generic.get_subtypes()[0];
                TemplateParam::new(subtype.str_name().to_string(), i, TypeObject::list([]))
            } else {
                let bound = if generic.get_subtypes().len() == 1 {
                    info.convert_type(&generic.get_subtypes()[0])?
                } else {
                    OBJECT.into()
                };
                TemplateParam::new(generic.str_name().to_string(), i, bound)
            };
            params.push(param)
        }
        Ok(GenericInfo::new(params, true))
    }

    pub fn parse_no_types(generics: &[TypeNode]) -> CompileResult<GenericInfo> {
        if generics.is_empty() {
            return Ok(Self::empty());
        }
        let mut params = Vec::with_capacity(generics.len());
        for (i, generic) in generics.iter().enumerate() {
            let param = if generic.is_vararg() {
                TemplateParam::new_vararg(generic.str_name().to_string(), i)
            } else if generic.get_name().is_empty() {
                // [*T]
                assert!(generic.get_subtypes().len() == 1 && generic.get_subtypes()[0].is_vararg());
                let subtype = &generic.get_subtypes()[0];
                TemplateParam::new(subtype.str_name().to_string(), i, TypeObject::list([]))
            } else if generic.get_subtypes().is_empty() {
                TemplateParam::new(generic.str_name().to_string(), i, OBJECT.into())
            } else {
                todo!("Default interfaces are done before types are registered, so this doesn't work yet")
            };
            params.push(param)
        }
        Ok(GenericInfo::new(params, false))
    }

    pub fn re_parse(&self, info: &mut CompilerInfo, generics: &[TypeNode]) -> CompileResult<()> {
        todo!("Re-parsing GenericInfo needs interior mutability")
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    pub fn get_param_map(&self) -> HashMap<String, TypeObject> {
        todo!()
    }

    pub fn set_parent(&self, parent: TypeObject) {
        for param in &self.params {
            param.set_parent(parent.clone())
        }
    }
}
