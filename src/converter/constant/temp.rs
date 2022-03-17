use std::fmt::Debug;
use std::hash::Hash;
use std::ptr;
use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::converter::builtins::Builtins;
use crate::converter::type_obj::TypeObject;

use super::LangConstant;

#[derive(Debug, Clone)]
pub struct TempConstant {
    pub(super) value: Arc<TempInner>,
}

#[derive(Debug)]
pub(super) struct TempInner {
    pub(super) value: OnceCell<LangConstant>,
    pub(super) ty: TypeObject,
}

impl TempConstant {
    pub fn new(ty: TypeObject) -> Self {
        Self {
            value: Arc::new(TempInner {
                value: OnceCell::new(),
                ty,
            }),
        }
    }

    pub fn get_inner(&self) -> Option<&LangConstant> {
        self.value.value.get()
    }

    pub fn set_reserved(&self, val: LangConstant) {
        self.value
            .value
            .set(val)
            .expect("Cannot set TempConstant more than once")
    }

    pub fn str_value(&self) -> Option<String> {
        self.value.value.get().and_then(|x| x.str_value())
    }

    pub fn repr_value(&self) -> Option<String> {
        self.value.value.get().and_then(|x| x.str_value())
    }

    pub fn fmt_name(
        &self,
        builtins: &Builtins,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self.value.value.get() {
            Option::Some(val) => val.fmt_name(builtins, f),
            Option::None => f.write_str("[temporary constant]"),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        panic!("All temporary constants should be written out during instantiation")
    }
}

impl From<TempConstant> for LangConstant {
    fn from(x: TempConstant) -> Self {
        Self::Temp(x)
    }
}

impl PartialEq for TempConstant {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}

impl Eq for TempConstant {}

impl Hash for TempConstant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(Arc::as_ptr(&self.value), state)
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::builtins::OBJECT;

    use super::TempConstant;

    #[test]
    fn get_inner() {
        let constant = TempConstant::new(OBJECT.into());
        assert_eq!(constant.get_inner(), None);
        constant.set_reserved(true.into());
        assert_eq!(constant.get_inner(), Some(&true.into()));
    }

    #[test]
    #[should_panic]
    fn double_set_inner() {
        let constant = TempConstant::new(OBJECT.into());
        constant.set_reserved(true.into());
        constant.set_reserved(false.into());
    }

    #[test]
    fn temp_str() {
        let constant = TempConstant::new(OBJECT.into());
        assert_eq!(constant.str_value(), None);
        constant.set_reserved(true.into());
        assert_eq!(constant.str_value().as_deref(), Some("true"));
    }

    #[test]
    fn temp_repr() {
        let constant = TempConstant::new(OBJECT.into());
        assert_eq!(constant.repr_value(), None);
        constant.set_reserved(true.into());
        assert_eq!(constant.repr_value().as_deref(), Some("true"));
    }
}
