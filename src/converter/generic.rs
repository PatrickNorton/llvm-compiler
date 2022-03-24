use std::cmp::Ordering;
use std::collections::HashMap;
use std::iter::zip;

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

    pub fn generify(&self, args: Vec<TypeObject>) -> Option<Vec<TypeObject>> {
        if !self.params.iter().any(|x| x.is_vararg()) {
            self.generify_no_varargs(args)
        } else {
            self.generify_vararg(args)
        }
    }

    fn generify_no_varargs(&self, args: Vec<TypeObject>) -> Option<Vec<TypeObject>> {
        if args.len() != self.len() {
            return None;
        }
        zip(args, &self.params)
            .map(|(arg, param)| {
                if is_invalid(&arg, param) {
                    None
                } else {
                    Some(arg)
                }
            })
            .collect()
    }

    fn generify_vararg(&self, args: Vec<TypeObject>) -> Option<Vec<TypeObject>> {
        if args.len() < self.len() - 1 {
            return None;
        }
        let mut result = Vec::with_capacity(self.len());
        let mut i = 0;
        while i < args.len() && !self.params[i].is_vararg() {
            if is_invalid(&args[i], &self.params[i]) {
                return None;
            }
            // TODO: Remove clones
            result.push(args[i].clone());
            i += 1;
        }
        let remaining_args = args.len() - i;
        let remaining_params = self.len() - i;
        match remaining_args.cmp(&remaining_params) {
            Ordering::Less => {
                // Number of types resulted in empty vararg, proceed accordingly
                result.push(TypeObject::list([]));
                while i < args.len() {
                    if is_invalid(&args[i], &self.params[i]) {
                        return None;
                    }
                    // TODO: Remove clones
                    result.push(args[i].clone());
                    i += 1;
                }
            }
            Ordering::Equal => {
                // Exactly one parameter goes into the vararg
                result.push(TypeObject::list([args[i].clone()]));
                i += 1;
                while i < args.len() {
                    if is_invalid(&args[i], &self.params[i]) {
                        return None;
                    }
                    // TODO: Remove clones
                    result.push(args[i].clone());
                    i += 1;
                }
            }
            Ordering::Greater => {
                let diff = remaining_args - remaining_params;
                result.push(TypeObject::list_of(args[i..diff + i + 1].to_vec()));
                i += 1;
                while i < self.len() {
                    if is_invalid(&args[i + diff], &self.params[i]) {
                        return None;
                    }
                    // TODO: Remove clones
                    result.push(args[i + diff].clone());
                    i += 1;
                }
            }
        }
        Some(result)
    }
}

fn is_invalid(arg: &TypeObject, param: &TemplateParam) -> bool {
    let is_list = matches!(param.get_bound(), TypeObject::List(_));
    if is_list != matches!(arg, TypeObject::List(_)) {
        true
    } else if !is_list {
        // FIXME? param.get_bound() != null
        return !param.get_bound().is_superclass(arg);
    } else {
        false
    }
}

impl Default for GenericInfo {
    fn default() -> Self {
        Self::empty()
    }
}
