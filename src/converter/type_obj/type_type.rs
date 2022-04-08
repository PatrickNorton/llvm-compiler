use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::slice;
use std::sync::Arc;

use crate::converter::access_handler::AccessLevel;
use crate::converter::builtins::Builtins;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::CompileResult;
use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::first;

use super::macros::{arc_partial_eq, try_from_type_obj, type_obj_from};
use super::TypeObject;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeTypeObject {
    value: Arc<InnerType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InnerType {
    typedef_name: Option<String>,
    generic: Option<TypeObject>,
}

impl TypeTypeObject {
    pub fn new(generic: TypeObject) -> Self {
        Self {
            value: Arc::new(InnerType {
                typedef_name: None,
                generic: Some(generic),
            }),
        }
    }

    pub fn new_empty() -> Self {
        Self {
            value: Arc::new(InnerType {
                typedef_name: None,
                generic: None,
            }),
        }
    }

    pub fn represented_type(&self) -> &TypeObject {
        self.value.generic.as_ref().unwrap()
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.value
            .typedef_name
            .as_deref()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| {
                self.value
                    .generic
                    .as_ref()
                    .map_or_else(|| "type".into(), |x| format!("type[{}]", x.name()).into())
            })
    }

    pub fn get_generics(&self) -> &[TypeObject] {
        if let Option::Some(generic) = &self.value.generic {
            slice::from_ref(generic)
        } else {
            &[]
        }
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        "type".into()
    }

    pub fn is_superclass(&self, other: &TypeObject) -> bool {
        matches!(other, TypeObject::Type(_) | TypeObject::Object(_))
    }

    pub fn generify(
        &self,
        line_info: &dyn Lined,
        args: Vec<TypeObject>,
    ) -> CompileResult<TypeObject> {
        if args.len() != 1 {
            Err(CompilerException::of(
                "Cannot generify 'type' with more than one argument",
                line_info,
            )
            .into())
        } else if self.value.generic.is_some() {
            Err(
                CompilerException::of(format!("Cannot generify '{}'", self.name()), line_info)
                    .into(),
            )
        } else {
            Ok(TypeTypeObject::new(first(args)).into())
        }
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(InnerType {
                typedef_name: Some(name),
                generic: self.value.generic.clone(),
            }),
        }
    }

    pub fn attr_type(&self, name: &str, access: AccessLevel) -> Option<Cow<'_, TypeObject>> {
        match &self.value.generic {
            Option::None => None,
            Option::Some(generic) => generic.static_attr_type(name, access),
        }
    }

    pub fn try_attr_type(
        &self,
        line_info: impl Lined,
        name: &str,
        access: AccessLevel,
    ) -> CompileResult<Cow<'_, TypeObject>> {
        match &self.value.generic {
            Option::None => Err(CompilerException::of(
                format!("Cannot get attribute '{}' from type 'type'", name),
                line_info,
            )
            .into()),
            Option::Some(generic) => generic.try_static_attr_type(&line_info, name, access),
        }
    }

    pub fn operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: &Builtins,
    ) -> Option<FunctionInfo> {
        self.value.generic.as_ref().and_then(|gen| {
            if o == OpSpTypeNode::Call {
                let mut_gen = gen.make_mut();
                let op_info = mut_gen
                    .op_info_access(OpSpTypeNode::New, access, builtins)
                    .map(Cow::into_owned);
                op_info.map(|fi| FunctionInfo::with_args(fi.get_args().clone(), vec![mut_gen]))
            } else {
                None
            }
        })
    }

    pub fn get_defined(&self) -> Option<Box<dyn Iterator<Item = Cow<'_, str>> + '_>> {
        self.value
            .generic
            .as_ref()
            .and_then(|x| x.static_defined())
            .map(|x| -> Box<dyn Iterator<Item = Cow<'_, str>>> { Box::new(x.map(Cow::Borrowed)) })
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        matches!(other, TypeObject::Tuple(_))
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.base_name().hash(state)
    }
}

impl Default for TypeTypeObject {
    fn default() -> Self {
        Self::new_empty()
    }
}

arc_partial_eq!(TypeTypeObject, Type);

type_obj_from!(TypeTypeObject, Type);
try_from_type_obj!(TypeTypeObject, Type);

#[cfg(test)]
mod tests {
    use crate::converter::builtins::OBJECT;
    use crate::converter::type_obj::{TupleType, TypeObject};
    use crate::parser::line_info::LineInfo;

    use super::TypeTypeObject;

    #[test]
    fn type_name() {
        let ty = TypeTypeObject::new(OBJECT.into());
        assert_eq!(ty.name(), "type[object]");
        assert_eq!(ty.base_name(), "type");
        let typedefed = ty.typedef_as("test".into());
        assert_eq!(typedefed.name(), "test");
        assert_eq!(typedefed.base_name(), "type");
    }

    #[test]
    fn empty_type_name() {
        let ty = TypeTypeObject::new_empty();
        assert_eq!(ty.name(), "type");
        assert_eq!(ty.base_name(), "type");
        let typedefed = ty.typedef_as("test".into());
        assert_eq!(typedefed.name(), "test");
        assert_eq!(typedefed.base_name(), "type");
    }

    #[test]
    fn type_type_equals() {
        let ty = TypeTypeObject::new_empty();
        assert_eq!(ty, ty);
        assert_eq!(TypeTypeObject::new_empty(), TypeTypeObject::new_empty());
        let obj = TypeTypeObject::new(OBJECT.into());
        assert_eq!(obj, obj);
        assert_eq!(
            TypeTypeObject::new(OBJECT.into()),
            TypeTypeObject::new(OBJECT.into()),
        );
    }

    #[test]
    fn generify_type() {
        let ty = TypeTypeObject::new_empty();
        assert_eq!(
            ty.generify(&LineInfo::empty(), vec![OBJECT.into()])
                .unwrap(),
            TypeObject::Type(TypeTypeObject::new(OBJECT.into())),
        );
    }

    #[test]
    fn generify_type_error() {
        let ty = TypeTypeObject::new_empty();
        assert!(ty.generify(&LineInfo::empty(), Vec::new()).is_err());
        assert!(ty
            .generify(&LineInfo::empty(), vec![OBJECT.into(), OBJECT.into()])
            .is_err());
        let full = TypeTypeObject::new(OBJECT.into());
        assert!(full
            .generify(&LineInfo::empty(), vec![TupleType::default().into()])
            .is_err());
        assert!(full
            .generify(&LineInfo::empty(), vec![OBJECT.into()])
            .is_err());
    }
}
