use std::borrow::Cow;
use std::sync::Arc;

use itertools::Itertools;

use crate::converter::builtins::Builtins;
use crate::converter::file_writer::ConstantSet;
use crate::converter::type_obj::{TupleType, TypeObject};
use crate::util::{usize_to_bytes, usize_to_short_bytes, U16_BYTES, U32_BYTES};

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleConstant {
    pub(super) value: Arc<[LangConstant]>,
}

impl TupleConstant {
    pub fn new(value: Vec<LangConstant>) -> Self {
        Self {
            value: value.into(),
        }
    }

    pub fn get_values(&self) -> &[LangConstant] {
        &self.value
    }

    pub fn get_type(&self, builtins: &Builtins) -> TypeObject {
        TupleType::new(
            self.value
                .iter()
                .map(|x| Cow::into_owned(x.get_type(builtins)))
                .collect(),
        )
        .into()
    }

    pub fn fmt_name(
        &self,
        builtins: &Builtins,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.value.iter().map(|x| x.display(builtins)).format(", ")
        )
    }

    pub fn to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES + self.value.len() * U16_BYTES);
        bytes.push(ConstantBytes::Tuple as u8);
        bytes.extend(usize_to_bytes(self.value.len()));
        for constant in &*self.value {
            let const_index = constants.get_index_of(constant).unwrap();
            bytes.extend(usize_to_short_bytes(const_index));
        }
        bytes
    }
}

impl From<TupleConstant> for LangConstant {
    fn from(x: TupleConstant) -> Self {
        Self::Tuple(x)
    }
}

impl TryFrom<LangConstant> for TupleConstant {
    type Error = LangConstant;

    fn try_from(value: LangConstant) -> Result<Self, Self::Error> {
        match value {
            LangConstant::Tuple(t) => Ok(t),
            x => Err(x),
        }
    }
}

impl Default for TupleConstant {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::constant::{ConstantBytes, TupleConstant};
    use crate::converter::file_writer::ConstantSet;

    const TUPLE_BYTE: u8 = ConstantBytes::Tuple as u8;

    #[test]
    fn tuple_bytes() {
        let constants = ConstantSet::new(
            vec![0.into(), 1.into(), 2.into(), true.into(), 3.into()]
                .into_iter()
                .collect(),
        );
        assert_eq!(
            TupleConstant::default().to_bytes(&constants),
            vec![TUPLE_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            TupleConstant::new(vec![1.into()]).to_bytes(&constants),
            vec![TUPLE_BYTE, 0, 0, 0, 1, 0, 1]
        );
        assert_eq!(
            TupleConstant::new(vec![1.into(), true.into(), 3.into()]).to_bytes(&constants),
            vec![TUPLE_BYTE, 0, 0, 0, 3, 0, 1, 0, 3, 0, 4]
        );
    }
}
