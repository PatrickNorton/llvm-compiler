use std::fmt::Display;
use std::hash::Hash;
use std::ptr;
use std::sync::Arc;

use crate::converter::type_obj::{TypeObject, TypeTypeObject};
use crate::util::U32_BYTES;

use super::{ConstantBytes, LangConstant, UserType};

#[derive(Debug, Clone)]
pub struct ClassConstant {
    value: Arc<ClassConstInner>, // TODO: Fields
}

#[derive(Debug)]
struct ClassConstInner {
    name: String,
    index: u16,
    type_val: UserType,
}

impl ClassConstant {
    pub fn new(name: &str, index: u16, type_val: UserType) -> Self {
        Self {
            value: Arc::new(ClassConstInner {
                name: name.to_string(),
                index,
                type_val,
            }),
        }
    }

    pub fn str_value(&self) -> String {
        self.value.name.clone()
    }

    pub fn repr_value(&self) -> String {
        self.value.name.clone()
    }

    pub fn get_type(&self) -> TypeObject {
        TypeTypeObject::new(self.value.type_val.clone().into()).into()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Class as u8);
        bytes.extend((self.value.index as u32).to_be_bytes());
        bytes
    }
}

impl From<ClassConstant> for LangConstant {
    fn from(x: ClassConstant) -> Self {
        Self::Class(x)
    }
}

impl Display for ClassConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.name.fmt(f)
    }
}

impl PartialEq for ClassConstant {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}

impl Eq for ClassConstant {}

impl Hash for ClassConstant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(Arc::as_ptr(&self.value), state)
    }
}
