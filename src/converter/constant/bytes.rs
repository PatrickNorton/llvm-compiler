use std::fmt::Display;
use std::sync::Arc;

use itertools::Itertools;

use crate::converter::builtins::BuiltinRef;
use crate::converter::type_obj::TypeObject;
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

    pub fn repr_value(&self) -> String {
        self.value
            .iter()
            .map(|&x| {
                if x < 0x80 {
                    string_escape::escaped(x as char)
                } else {
                    format!(r"\x{:02x}", x).into()
                }
            })
            .join("")
    }

    pub fn get_type<'a>(&self, builtins: BuiltinRef<'a>) -> &'a TypeObject {
        builtins.bytes_type()
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
                .map(|&x| if x < 0x80 {
                    string_escape::escaped(x as char)
                } else {
                    format!(r"\x{:02x}", x).into()
                })
                .format("")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::constant::{BytesConstant, ConstantBytes};

    #[test]
    fn to_bytes() {
        const BYTES_BYTE: u8 = ConstantBytes::Bytes as u8;
        assert_eq!(
            BytesConstant::new(vec![]).to_bytes(),
            vec![BYTES_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            BytesConstant::new(vec![0, 1, 2, 3]).to_bytes(),
            vec![BYTES_BYTE, 0, 0, 0, 4, 0, 1, 2, 3]
        );
    }

    #[test]
    fn display() {
        assert_eq!(format!("{}", BytesConstant::new(vec![])), r#"b"""#);
        assert_eq!(format!("{}", BytesConstant::new(vec![0x61])), r#"b"a""#);
        assert_eq!(format!("{}", BytesConstant::new(vec![0x5c])), r#"b"\\""#);
        assert_eq!(
            format!("{}", BytesConstant::new(vec![0xe5, 0x80, 0x80])),
            r#"b"\xe5\x80\x80""#
        );
    }
}
