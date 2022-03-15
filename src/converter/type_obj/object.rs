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

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            typedef_name: Some(name.into()),
        }
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.base_name().hash(state)
    }
}

type_obj_from!(ObjectType, Object);
