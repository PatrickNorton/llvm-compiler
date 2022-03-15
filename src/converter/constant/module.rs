use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::ptr;
use std::sync::Arc;

use crate::converter::file_writer::ConstantSet;
use crate::util::usize_to_bytes;

use super::{ConstantBytes, LangConstant, StringConstant};

#[derive(Debug, Clone)]
pub struct ModuleConstant {
    value: Arc<ModuleInner>,
}

#[derive(Debug)]
struct ModuleInner {
    name: String,
    values: HashMap<String, LangConstant>,
}

impl ModuleConstant {
    pub fn new(name: String, values: HashMap<String, LangConstant>) -> Self {
        Self {
            value: Arc::new(ModuleInner { name, values }),
        }
    }

    pub fn to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        let mut bytes = vec![ConstantBytes::Module as u8];
        bytes.extend(usize_to_bytes(self.value.values.len()));
        for (name, constant) in &self.value.values {
            bytes.extend(StringConstant::str_bytes(name));
            bytes.extend(usize_to_bytes(constants.get_index_of(constant).unwrap()));
        }
        bytes
    }
}

impl Display for ModuleConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.name.fmt(f)
    }
}

impl From<ModuleConstant> for LangConstant {
    fn from(x: ModuleConstant) -> Self {
        Self::Module(x)
    }
}

impl PartialEq for ModuleConstant {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}

impl Eq for ModuleConstant {}

impl Hash for ModuleConstant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(Arc::as_ptr(&self.value), state)
    }
}
