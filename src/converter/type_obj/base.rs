use std::hash::{Hash, Hasher};

use super::{TypeObject, UserType};

#[derive(Debug, Clone)]
pub struct BaseType {
    value: TypeObject,
}

impl BaseType {
    pub fn new(value: TypeObject) -> Self {
        Self { value }
    }
}

impl PartialEq for BaseType {
    fn eq(&self, other: &Self) -> bool {
        self.value.same_base_type(&other.value)
    }
}

impl Eq for BaseType {}

impl Hash for BaseType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.base_hash(state)
    }
}

impl From<TypeObject> for BaseType {
    fn from(x: TypeObject) -> Self {
        Self::new(x)
    }
}

impl From<UserType> for BaseType {
    fn from(x: UserType) -> Self {
        Self::new(x.into())
    }
}
