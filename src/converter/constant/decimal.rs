use std::fmt::Display;
use std::sync::Arc;

use crate::converter::builtins::Builtins;
use crate::converter::type_obj::TypeObject;
use crate::util::decimal::BigDecimal;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DecimalConstant {
    pub(super) value: Arc<BigDecimal>,
}

impl DecimalConstant {
    pub fn new(value: BigDecimal) -> Self {
        Self {
            value: Arc::new(value),
        }
    }

    pub fn get_value(&self) -> &BigDecimal {
        &self.value
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.dec_type()
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        todo!()
    }
}

impl Display for DecimalConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
