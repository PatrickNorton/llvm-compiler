use std::hash::{Hash, Hasher};
use std::ptr;
use std::sync::Arc;

use super::constant::LangConstant;
use super::type_obj::TypeObject;

// TODO? Remove double-indirection
#[derive(Debug, Clone)]
pub struct LangInstance {
    value: Arc<InnerInstance>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LangObject {
    Constant(LangConstant),
    Instance(LangInstance),
    Type(TypeObject),
}

#[derive(Debug)]
struct InnerInstance {
    cls: TypeObject,
}

impl LangInstance {
    pub fn new(obj: TypeObject) -> Self {
        Self {
            value: Arc::new(InnerInstance { cls: obj }),
        }
    }
}

impl LangObject {
    pub fn get_type(&self) -> &TypeObject {
        match self {
            LangObject::Constant(c) => c.get_type(),
            LangObject::Instance(i) => &i.value.cls,
            LangObject::Type(t) => t,
        }
    }
}

impl PartialEq for LangInstance {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}

impl Eq for LangInstance {}

impl Hash for LangInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(Arc::as_ptr(&self.value), state)
    }
}

impl PartialEq<TypeObject> for LangObject {
    fn eq(&self, other: &TypeObject) -> bool {
        match self {
            LangObject::Constant(_) => false,
            LangObject::Instance(_) => false,
            LangObject::Type(this) => this == other,
        }
    }
}
