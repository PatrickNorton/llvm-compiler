use std::fmt::Display;
use std::sync::Arc;

use crate::converter::type_obj::TypeObject;
use crate::util::U32_BYTES;

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportConstant {
    name: Arc<str>,
    index: u16,
}

impl ImportConstant {
    pub fn new(index: u16, name: String) -> Self {
        Self {
            index,
            name: name.into(),
        }
    }

    pub fn get_type(&self) -> &TypeObject {
        todo!("Cannot figure out type of this yet")
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Import as u8);
        bytes.extend((self.index as u32).to_be_bytes());
        bytes
    }
}

impl Display for ImportConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl From<ImportConstant> for LangConstant {
    fn from(x: ImportConstant) -> Self {
        LangConstant::Import(x)
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::constant::{ConstantBytes, ImportConstant};

    #[test]
    fn import_bytes() {
        const IMPORT_BYTE: u8 = ConstantBytes::Import as u8;
        assert_eq!(
            ImportConstant::new(0, String::new()).to_bytes(),
            vec![IMPORT_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            ImportConstant::new(1000, String::new()).to_bytes(),
            vec![IMPORT_BYTE, 0, 0, 0x3, 0xe8]
        );
    }
}
