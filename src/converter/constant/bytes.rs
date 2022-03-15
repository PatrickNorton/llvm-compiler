use std::fmt::Display;
use std::sync::Arc;

use itertools::Itertools;

use crate::util::{string_escape, usize_to_bytes, U32_BYTES};

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BytesConstant {
    pub(super) value: Arc<[u8]>,
}

impl BytesConstant {
    pub fn new(value: Vec<u8>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn get_value(&self) -> &[u8] {
        &self.value
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES + self.value.len());
        bytes.push(ConstantBytes::Bytes as u8);
        bytes.extend(usize_to_bytes(self.value.len()));
        bytes.extend(&*self.value);
        bytes
    }
}

impl From<BytesConstant> for LangConstant {
    fn from(x: BytesConstant) -> Self {
        LangConstant::Bytes(x)
    }
}

impl Display for BytesConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"b"{}""#,
            self.value
                .iter()
                .map(|&x| string_escape::escaped(x as char))
                .format("")
        )
    }
}
