use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::converter::builtins::NULL_TYPE;
use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::fn_info::FunctionInfo;

use super::macros::{arc_eq_hash, try_from_type_obj, type_obj_from};
use super::TypeObject;

#[derive(Debug, Clone)]
pub struct OptionTypeObject {
    value: Arc<InnerOption>,
}

#[derive(Debug)]
struct InnerOption {
    typedef_name: Option<String>,
    option_val: TypeObject,
    map_fn: FunctionInfo,
    flat_map_fn: FunctionInfo,
}

impl OptionTypeObject {
    pub fn new(option_val: TypeObject) -> Self {
        Self {
            value: Arc::new(InnerOption {
                typedef_name: None,
                map_fn: Self::get_map(option_val.clone()),
                flat_map_fn: Self::get_flat_map(option_val.clone()),
                option_val,
            }),
        }
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.value.typedef_name.as_deref().map_or_else(
            || (self.value.option_val.name().into_owned() + "?").into(),
            Cow::Borrowed,
        )
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        "".into()
    }

    pub fn get_option_val(&self) -> &TypeObject {
        &self.value.option_val
    }

    pub fn strip_null(&self) -> &TypeObject {
        &self.value.option_val
    }

    pub fn needs_and_super(a: &TypeObject, b: &TypeObject) -> bool {
        Self::needs_make_option(a, b) && Self::super_with_option(a, b)
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(InnerOption {
                typedef_name: Some(name),
                map_fn: self.value.map_fn.clone(),
                flat_map_fn: self.value.flat_map_fn.clone(),
                option_val: self.value.option_val.clone(),
            }),
        }
    }

    pub fn needs_make_option(maybe_option: &TypeObject, other: &TypeObject) -> bool {
        if let TypeObject::Option(option) = maybe_option {
            // TODO: Refer to Builtins
            NULL_TYPE.is_superclass(other) || option.get_option_val().is_superclass(other)
        } else {
            false
        }
    }

    pub fn super_with_option(maybe_option: &TypeObject, other: &TypeObject) -> bool {
        debug_assert!(Self::needs_make_option(maybe_option, other));
        // TODO: Refer to Builtins
        other.same_base_type(&*NULL_TYPE)
            || maybe_option.is_superclass(&TypeObject::optional(other.clone()))
    }

    pub fn is_superclass(&self, other: &TypeObject) -> bool {
        // TODO: Do without cloning
        other.is_subclass(&self.clone().into())
    }

    pub fn generify_with(&self, parent: &TypeObject, args: Vec<TypeObject>) -> TypeObject {
        TypeObject::optional(self.value.option_val.generify_with(parent, args))
    }

    pub fn attr_type(&self, value: &str) -> Option<TypeObject> {
        match value {
            "map" => Some(self.value.map_fn.clone().into()),
            "flatMap" => Some(self.value.flat_map_fn.clone().into()),
            _ => None,
        }
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        matches!(other, TypeObject::Object(_))
    }

    pub fn make_const(&self) -> Self {
        Self::new(self.value.option_val.make_const())
    }

    pub fn make_mut(&self) -> Self {
        Self::new(self.value.option_val.make_mut())
    }

    #[inline]
    pub fn maybe_wrap_bytes(bytes: BytecodeList, wrap: bool) -> BytecodeList {
        if wrap {
            Self::wrap_bytes(bytes)
        } else {
            bytes
        }
    }

    #[inline]
    pub fn wrap_bytes(mut bytes: BytecodeList) -> BytecodeList {
        bytes.add(Bytecode::MakeOption());
        bytes
    }

    pub fn get_defined(&self) -> impl Iterator<Item = &'_ str> {
        ["map", "flat_map"].into_iter()
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        "object".hash(state)
    }

    fn get_map(ty: TypeObject) -> FunctionInfo {
        todo!()
    }

    fn get_flat_map(ty: TypeObject) -> FunctionInfo {
        todo!()
    }
}

arc_eq_hash!(OptionTypeObject);

type_obj_from!(OptionTypeObject, Option);
try_from_type_obj!(OptionTypeObject, Option);
