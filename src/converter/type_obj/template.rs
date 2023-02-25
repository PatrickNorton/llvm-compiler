use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::converter::access_handler::AccessLevel;
use crate::converter::builtins::{BuiltinRef, OBJECT};
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::CompileResult;
use crate::macros::hash_map;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::type_node::TypeNode;

use super::error::AccessErrorType;
use super::macros::{arc_eq_hash, arc_partial_eq, try_from_type_obj, type_obj_from};
use super::{ListTypeObject, TypeObject};

#[derive(Debug, Clone)]
pub struct TemplateParam {
    value: Arc<TemplateInner>,
}

struct TemplateInner {
    name: String,
    index: usize,
    bound: TemplateBound,
    is_vararg: bool,
    typedef_name: Option<String>,
    parent: OnceCell<TypeObject>,
}

#[derive(Debug, Clone)]
enum TemplateBound {
    Known(TypeObject),
    Redefined(Arc<OnceCell<TypeObject>>),
}

impl TemplateParam {
    pub fn new(name: String, index: usize, bound: TypeObject) -> Self {
        Self {
            value: Arc::new(TemplateInner {
                name,
                index,
                bound: TemplateBound::Known(bound),
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
                bound: TemplateBound::Known(TypeObject::list([])),
                is_vararg: true,
                typedef_name: None,
                parent: OnceCell::new(),
            }),
        }
    }

    pub fn new_unbounded(name: String, index: usize) -> Self {
        Self {
            value: Arc::new(TemplateInner {
                name,
                index,
                bound: TemplateBound::Known(OBJECT.into()),
                is_vararg: false,
                typedef_name: None,
                parent: OnceCell::new(),
            }),
        }
    }

    pub fn new_unknown_bound(name: String, index: usize) -> Self {
        Self {
            value: Arc::new(TemplateInner {
                name,
                index,
                bound: TemplateBound::Redefined(Arc::new(OnceCell::new())),
                is_vararg: false,
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

    pub fn is_superclass(&self, other: &TypeObject) -> bool {
        other.is_superclass(&self.clone().into())
    }

    pub fn is_subclass(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::Template(tp) => {
                tp.get_parent().same_base_type(self.get_parent())
                    && tp.get_bound().is_superclass(self.get_bound())
            }
            _ => other.is_superclass(self.get_bound()),
        }
    }

    pub fn get_bound(&self) -> &TypeObject {
        static OBJECT_TYPE: TypeObject = TypeObject::Object(OBJECT);
        match &self.value.bound {
            TemplateBound::Known(bound) => bound,
            TemplateBound::Redefined(bound) => bound.get().unwrap_or(&OBJECT_TYPE),
        }
    }

    pub fn set_bound(&self, bound: TypeObject) {
        match &self.value.bound {
            TemplateBound::Known(_) => panic!("Cannot set bound of already-bounded object"),
            TemplateBound::Redefined(b) => b
                .set(bound)
                .expect("Cannot set bound of already-bounded object"),
        }
    }

    pub fn is_bounded(&self) -> bool {
        self.value.bound.is_known()
    }

    pub fn get_parent(&self) -> &TypeObject {
        self.value
            .parent
            .get()
            .expect("Should have given parent before acquiring it")
    }

    pub fn set_parent(&self, parent: TypeObject) {
        match self.value.parent.try_insert(parent) {
            Result::Ok(_) => {}
            Result::Err((ty, parent)) => {
                if !ty.same_base_type(&parent) {
                    panic!("Parent should only be written to once")
                }
            }
        }
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
    ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
        self.get_bound().attr_type_access(value, access)
    }

    pub fn static_attr_type(
        &self,
        value: &str,
        access: AccessLevel,
    ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
        self.get_bound().static_attr_type(value, access)
    }

    pub fn operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> Option<Cow<'_, FunctionInfo>> {
        self.get_bound().op_info_access(o, access, builtins)
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

    pub fn generify_as(
        &self,
        parent: &TypeObject,
        other: &TypeObject,
    ) -> Option<HashMap<u16, TypeObject>> {
        if self == other {
            Some(HashMap::new())
        } else if self.get_parent().same_base_type(parent) {
            if self.is_vararg() || self.get_bound().is_superclass(other) {
                Some(hash_map!(self.value.index.try_into().unwrap() => other.clone()))
            } else {
                None
            }
        } else if self.is_superclass(other) {
            Some(HashMap::new())
        } else {
            None
        }
    }

    pub fn get_defined(&self) -> Option<Box<dyn Iterator<Item = Cow<'_, str>> + '_>> {
        self.get_bound().get_defined()
    }

    pub fn static_defined(&self) -> Option<Box<dyn Iterator<Item = &'_ str> + '_>> {
        self.get_bound().static_defined()
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

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::Template(t) => self == t,
            _ => false,
        }
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.hash(state)
    }
}

impl TemplateBound {
    pub fn is_known(&self) -> bool {
        matches!(self, TemplateBound::Known(_))
    }
}

impl Debug for TemplateInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TemplateInner")
            .field("name", &self.name)
            .field("index", &self.index)
            .field("bound", &"...")
            .field("is_vararg", &self.is_vararg)
            .field("typedef_name", &self.typedef_name)
            .field("parent", &"...")
            .finish()
    }
}

arc_eq_hash!(TemplateParam);
arc_partial_eq!(TemplateParam, Template);

type_obj_from!(TemplateParam, Template);
try_from_type_obj!(TemplateParam, Template);

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::converter::builtins::OBJECT;
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{StdTypeObject, TemplateParam, UserTypeLike};
    use crate::macros::hash_map;
    use crate::parser::line_info::LineInfo;

    #[test]
    fn eq_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new(
            "parent".into(),
            Some(Vec::new()),
            generic_info,
            true,
            LineInfo::empty(),
        );
        parent.set_generic_parent();
        assert_eq!(
            param.clone().generify_as(&parent.into(), &param.into()),
            Some(HashMap::new()),
        );
    }

    #[test]
    fn template_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new(
            "parent".into(),
            Some(Vec::new()),
            generic_info,
            true,
            LineInfo::empty(),
        );
        parent.set_generic_parent();
        assert_eq!(
            param.generify_as(&parent.into(), &OBJECT.into()),
            Some(hash_map!(0 => OBJECT.into())),
        );
    }
}
