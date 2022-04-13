use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::macros::{arc_eq_hash, try_from_type_obj, type_obj_from};
use super::TypeObject;

#[derive(Debug, Clone)]
pub struct ModuleType {
    value: Arc<ModuleInner>,
}

#[derive(Debug)]
struct ModuleInner {
    name: String,
    values: HashMap<String, TypeObject>,
}

impl ModuleType {
    pub fn new(name: String, values: HashMap<String, TypeObject>) -> Self {
        Self {
            value: Arc::new(ModuleInner { name, values }),
        }
    }

    pub fn name(&self) -> Cow<'_, str> {
        format!("[anonymous module type '{}']", self.value.name).into()
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        "[anonymous module type]".into()
    }

    pub fn attr_type(&self, name: &str) -> Option<&TypeObject> {
        self.value.values.get(name)
    }

    pub fn get_defined(&self) -> impl Iterator<Item = &'_ str> {
        self.value.values.keys().map(|x| x.as_str())
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        matches!(other, TypeObject::Module(_))
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.base_name().hash(state)
    }

    pub fn typedef_as(&self, _name: String) -> Self {
        panic!("Cannot typedef a module")
    }
}

arc_eq_hash!(ModuleType);

try_from_type_obj!(ModuleType, Module);
type_obj_from!(ModuleType, Module);
