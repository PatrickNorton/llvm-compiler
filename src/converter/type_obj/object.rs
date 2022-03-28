use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::converter::access_handler::AccessLevel;
use crate::converter::fn_info::FunctionInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::macros::type_obj_from;
use super::TypeObject;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectType {
    typedef_name: Option<Arc<str>>,
}

impl ObjectType {
    pub const fn new() -> Self {
        Self { typedef_name: None }
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.typedef_name.as_deref().unwrap_or("object").into()
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        "object".into()
    }

    pub fn operator_info(&self, o: OpSpTypeNode, access: AccessLevel) -> Option<&FunctionInfo> {
        match o {
            OpSpTypeNode::Equals => todo!(),
            OpSpTypeNode::Str | OpSpTypeNode::Repr => todo!(),
            OpSpTypeNode::Bool => todo!(),
            _ => None,
        }
    }

    pub fn is_superclass(&self, _other: &TypeObject) -> bool {
        true
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        matches!(other, TypeObject::Object(_))
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            typedef_name: Some(name.into()),
        }
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.base_name().hash(state)
    }
}

arc_partial_eq!(ObjectType, Object);

type_obj_from!(ObjectType, Object);

impl Default for ObjectType {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::type_obj::{ObjectType, TupleType};

    #[test]
    fn const_constructable() {
        const _: ObjectType = ObjectType::new();
    }

    #[test]
    fn object_name() {
        let obj = ObjectType::new();
        assert_eq!(obj.name(), "object");
        assert_eq!(obj.base_name(), "object");
        let typedefed = obj.typedef_as("test".to_string());
        assert_eq!(typedefed.name(), "test");
        assert_eq!(typedefed.base_name(), "object");
    }

    #[test]
    fn obj_superclass() {
        let obj = ObjectType::new();
        assert!(obj.is_superclass(&ObjectType::new().into()));
        assert!(obj.is_superclass(&TupleType::default().into()));
    }

    #[test]
    fn obj_subclass() {
        let obj = ObjectType::new();
        assert!(obj.is_subclass(&ObjectType::new().into()));
        assert!(!obj.is_subclass(&TupleType::default().into()));
    }
}
