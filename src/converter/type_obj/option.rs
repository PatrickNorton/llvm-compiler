use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::converter::access_handler::AccessLevel;
use crate::converter::argument::ArgumentInfo;
use crate::converter::builtins::{BuiltinRef, NULL_TYPE, OBJECT};
use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::TemplateParam;
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;

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

    pub fn generify_as(
        &self,
        parent: &TypeObject,
        other: &TypeObject,
    ) -> Option<HashMap<u16, TypeObject>> {
        match other {
            TypeObject::Option(o) => self
                .get_option_val()
                .generify_as(parent, o.get_option_val()),
            TypeObject::Object(_) => Some(HashMap::new()),
            _ => None,
        }
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

    pub fn operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> Option<Cow<'_, FunctionInfo>> {
        match o {
            OpSpTypeNode::Hash => self.value.option_val.op_info_access(o, access, builtins),
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;

    use crate::converter::builtins::{NULL_TYPE, OBJECT};
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{
        StdTypeObject, TemplateParam, TupleType, TypeObject, UserTypeLike,
    };
    use crate::macros::hash_map;
    use crate::parser::line_info::LineInfo;

    use super::OptionTypeObject;

    #[test]
    fn option_name() {
        let opt = OptionTypeObject::new(OBJECT.into());
        assert_eq!(opt.name(), "object?");
        let typedefed = opt.typedef_as("test".to_string());
        assert_eq!(typedefed.name(), "test");
    }

    #[test]
    fn strip_null() {
        let opt = OptionTypeObject::new(TupleType::default().into());
        assert_eq!(opt.strip_null(), &TupleType::default());
        assert_eq!(opt.get_option_val(), &TupleType::default());
    }

    #[test]
    fn needs_make_option() {
        let tup = TypeObject::from(TupleType::default());
        let opt_object = OptionTypeObject::new(tup.clone()).into();
        assert!(OptionTypeObject::needs_make_option(&opt_object, &tup));
        assert!(!OptionTypeObject::needs_make_option(
            &opt_object,
            &opt_object,
        ));
        assert!(OptionTypeObject::needs_make_option(&opt_object, &NULL_TYPE));
        assert!(!OptionTypeObject::needs_make_option(&tup, &opt_object))
    }

    #[test]
    fn super_with_option() {
        let tup = TypeObject::from(TupleType::default());
        let opt_object = OptionTypeObject::new(tup.clone()).into();
        assert!(OptionTypeObject::super_with_option(&opt_object, &tup));
        assert!(OptionTypeObject::super_with_option(&opt_object, &NULL_TYPE));
    }

    #[test]
    fn super_defined() {
        let opt = OptionTypeObject::new(OBJECT.into());
        let mut opt_defined = opt.get_defined().collect_vec();
        opt_defined.sort_unstable();
        assert_eq!(opt_defined, &["flat_map", "map"]);
    }

    #[test]
    fn option_generify_as() {
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
        let value = OptionTypeObject::new(param.into());
        let result = OptionTypeObject::new(OBJECT.into());
        assert_eq!(
            value.generify_as(&parent.into(), &result.into()),
            Some(hash_map!(0 => OBJECT.into()))
        );
    }

    #[test]
    fn identity_generify_as() {
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
        let value = OptionTypeObject::new(param.clone().into());
        let result = OptionTypeObject::new(param.into());
        assert_eq!(
            value.generify_as(&parent.into(), &result.into()),
            Some(HashMap::new())
        );
    }

    #[test]
    fn invalid_generify_as() {
        let param = TemplateParam::new("T".into(), 0, TupleType::default().into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new(
            "parent".into(),
            Some(Vec::new()),
            generic_info,
            true,
            LineInfo::empty(),
        );
        parent.set_generic_parent();
        let value = OptionTypeObject::new(param.into());
        let result = OptionTypeObject::new(OBJECT.into());
        assert_eq!(value.generify_as(&parent.into(), &result.into()), None);
    }
}
