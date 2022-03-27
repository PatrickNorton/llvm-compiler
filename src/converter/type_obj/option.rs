use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::converter::argument::ArgumentInfo;
use crate::converter::builtins::{NULL_TYPE, OBJECT};
use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::TemplateParam;
use crate::parser::line_info::LineInfo;

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
    map_fn: OnceCell<FunctionInfo>,
    flat_map_fn: OnceCell<FunctionInfo>,
}

impl OptionTypeObject {
    pub fn new(option_val: TypeObject) -> Self {
        Self {
            value: Arc::new(InnerOption {
                typedef_name: None,
                map_fn: OnceCell::new(),
                flat_map_fn: OnceCell::new(),
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
        // FIXME: Passing `object` into `other` does not work properly
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

    pub fn is_subclass(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::Option(o) => {
                self == o || o.get_option_val().is_superclass(self.get_option_val())
            }
            _ => false,
        }
    }

    pub fn generify_with(&self, parent: &TypeObject, args: Vec<TypeObject>) -> TypeObject {
        TypeObject::optional(self.value.option_val.generify_with(parent, args))
    }

    pub fn attr_type(&self, value: &str) -> Option<TypeObject> {
        match value {
            "map" => Some(
                self.value
                    .map_fn
                    .get_or_init(|| Self::get_map(self.get_option_val().clone()))
                    .clone()
                    .into(),
            ),
            "flatMap" => Some(
                self.value
                    .flat_map_fn
                    .get_or_init(|| Self::get_flat_map(self.get_option_val().clone()))
                    .clone()
                    .into(),
            ),
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
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let inner_fn =
            FunctionInfo::with_args(ArgumentInfo::of_types([ty]), vec![param.clone().into()])
                .to_callable();
        let fn_info = FunctionInfo::new(
            LineInfo::empty(),
            "map".to_string(),
            false,
            GenericInfo::new(vec![param.clone()]),
            ArgumentInfo::of_types([inner_fn]),
            vec![TypeObject::optional(param.into())],
        );
        fn_info.set_generic_parent();
        fn_info
    }

    fn get_flat_map(ty: TypeObject) -> FunctionInfo {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let option_param = TypeObject::optional(param.clone().into());
        let inner_fn =
            FunctionInfo::with_args(ArgumentInfo::of_types([ty]), vec![option_param.clone()])
                .to_callable();
        let fn_info = FunctionInfo::new(
            LineInfo::empty(),
            "flat_map".to_string(),
            false,
            GenericInfo::new(vec![param]),
            ArgumentInfo::of_types([inner_fn]),
            vec![TypeObject::optional(option_param)],
        );
        fn_info.set_generic_parent();
        fn_info
    }
}

arc_eq_hash!(OptionTypeObject);

type_obj_from!(OptionTypeObject, Option);
try_from_type_obj!(OptionTypeObject, Option);
