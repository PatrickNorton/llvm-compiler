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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::converter::constant::{ConstantBytes, ModuleConstant};
    use crate::converter::file_writer::ConstantSet;
    use crate::macros::hash_map;

    const MODULE_BYTE: u8 = ConstantBytes::Module as u8;

    #[test]
    fn empty_bytes() {
        let constants = ConstantSet::default();
        assert_eq!(
            ModuleConstant::new(String::new(), HashMap::new()).to_bytes(&constants),
            vec![MODULE_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            ModuleConstant::new("test".to_string(), HashMap::new()).to_bytes(&constants),
            vec![MODULE_BYTE, 0, 0, 0, 0]
        );
    }

    #[test]
    fn module_bytes() {
        let constants = ConstantSet::new(vec![true.into(), false.into()].into_iter().collect());
        #[rustfmt::skip]
        let result = vec![
            MODULE_BYTE,
            0, 0, 0, 1,
            0, 0, 0, 4,
            0x74, 0x72, 0x75, 0x65,
            0, 0, 0, 0
        ];
        assert_eq!(
            ModuleConstant::new(String::new(), hash_map!("true".into() => true.into()))
                .to_bytes(&constants),
            result
        );
    }
}
