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

    pub fn str_value(&self) -> String {
        format!("{}?", self.value.ty.name())
    }

    pub fn repr_value(&self) -> String {
        format!("{}?", self.value.ty.name())
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

#[cfg(test)]
mod tests {
    use crate::converter::builtins::OBJECT;
    use crate::converter::constant::{
        BuiltinConstant, ConstantBytes, OptionConstant, OptionTypeConstant,
    };
    use crate::converter::file_writer::ConstantSet;

    const OPTION_BYTE: u8 = ConstantBytes::Option as u8;
    const OPTION_TYPE_BYTE: u8 = ConstantBytes::OptionType as u8;

    #[test]
    fn option_bytes() {
        let constants = ConstantSet::new(vec![true.into(), false.into()].into_iter().collect());
        assert_eq!(
            OptionConstant::new(true.into()).to_bytes(&constants),
            vec![OPTION_BYTE, 0, 0]
        );
        assert_eq!(
            OptionConstant::new(false.into()).to_bytes(&constants),
            vec![OPTION_BYTE, 0, 1]
        );
    }

    #[test]
    fn option_type_bytes() {
        let object_builtin = BuiltinConstant::new(21);
        let constants = ConstantSet::new(vec![object_builtin.clone().into()].into_iter().collect());
        assert_eq!(
            OptionTypeConstant::new(object_builtin.into(), OBJECT.into()).to_bytes(&constants),
            vec![OPTION_TYPE_BYTE, 0, 0, 0, 0]
        )
    }

    #[test]
    fn option_type_str() {
        let object_builtin = BuiltinConstant::new(21);
        assert_eq!(
            OptionTypeConstant::new(object_builtin.into(), OBJECT.into()).str_value(),
            "object?"
        );
    }

    #[test]
    fn option_type_repr() {
        let object_builtin = BuiltinConstant::new(21);
        assert_eq!(
            OptionTypeConstant::new(object_builtin.into(), OBJECT.into()).repr_value(),
            "object?"
        );
    }
}
