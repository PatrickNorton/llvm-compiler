use std::fmt::Display;
use std::sync::Arc;

use crate::converter::builtins::Builtins;
use crate::converter::file_writer::ConstantSet;
use crate::converter::type_obj::TypeObject;
use crate::util::{usize_to_bytes, usize_to_short_bytes, U16_BYTES};

use super::{ConstantBytes, LangConstant};

// FIXME: Empty OptionConstant
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OptionConstant {
    value: Box<LangConstant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OptionTypeConstant {
    value: Arc<OptionTypeInner>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct OptionTypeInner {
    inner: LangConstant,
    ty: TypeObject,
}

impl OptionConstant {
    pub fn new(value: LangConstant) -> Self {
        Self {
            value: Box::new(value),
        }
    }

    pub fn fmt_name(&self, builtins: &Builtins, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Option[{}]", self.value.display(builtins))
    }

    pub fn to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(U16_BYTES + 1);
        bytes.push(ConstantBytes::Option as u8);
        let const_index = constants.get_index_of(&*self.value).unwrap();
        bytes.extend(usize_to_short_bytes(const_index));
        bytes
    }
}

impl OptionTypeConstant {
    pub fn new(inner: LangConstant, ty: TypeObject) -> Self {
        Self {
            value: Arc::new(OptionTypeInner { inner, ty }),
        }
    }

    pub fn to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(U16_BYTES + 1);
        bytes.push(ConstantBytes::OptionType as u8);
        let const_index = constants.get_index_of(&self.value.inner).unwrap();
        bytes.extend(usize_to_bytes(const_index));
        bytes
    }
}

impl Display for OptionTypeConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}?", self.value.ty.name())
    }
}

impl From<OptionConstant> for LangConstant {
    fn from(x: OptionConstant) -> Self {
        LangConstant::Option(x)
    }
}

impl From<OptionTypeConstant> for LangConstant {
    fn from(x: OptionTypeConstant) -> Self {
        LangConstant::OptionType(x)
    }
}
