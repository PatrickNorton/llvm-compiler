use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::iter::zip;
use std::sync::Arc;

use itertools::Itertools;

use crate::converter::error::CompilerException;
use crate::converter::CompileResult;
use crate::parser::line_info::Lined;

use super::macros::{arc_partial_eq, type_obj_from};
use super::TypeObject;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleType {
    value: Arc<TupleInner>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct TupleInner {
    generics: Vec<TypeObject>,
    typedef_name: Option<String>,
}

impl TupleType {
    pub fn new(args: Vec<TypeObject>) -> Self {
        Self {
            value: Arc::new(TupleInner {
                generics: args,
                typedef_name: None,
            }),
        }
    }

    pub fn get_generics(&self) -> &[TypeObject] {
        &self.value.generics
    }

    pub fn name(&self) -> Cow<'_, str> {
        if let Option::Some(typedef) = &self.value.typedef_name {
            typedef.as_str().into()
        } else if self.value.generics.is_empty() {
            "tuple".into()
        } else {
            format!(
                "tuple[{}]",
                self.value.generics.iter().map(|x| x.name()).format(", ")
            )
            .into()
        }
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        "tuple".into()
    }

    pub fn is_subclass(&self, other: &TypeObject) -> bool {
        if let TypeObject::Tuple(tuple) = other {
            if self == tuple {
                return true;
            } else if self.get_generics().len() != tuple.get_generics().len() {
                return false;
            }
            zip(self.get_generics(), tuple.get_generics()).all(|(x, y)| x.is_superclass(y))
        // FIXME: Self is hashable
        } else if other.will_super_recurse() {
            false
        } else {
            other.is_superclass(&self.clone().into())
        }
    }

    pub fn generify(&self, lined: &dyn Lined, args: Vec<TypeObject>) -> CompileResult<TypeObject> {
        if self.value.generics.is_empty() {
            Ok(TupleType::new(args).into())
        } else {
            Err(CompilerException::of("Cannot generify object", lined).into())
        }
    }

    pub fn typedef_as(&self, name: String) -> Self {
        TupleType {
            value: Arc::new(TupleInner {
                generics: self.value.generics.clone(),
                typedef_name: Some(name),
            }),
        }
    }

    pub fn attr_type(&self, value: &str) -> Option<&TypeObject> {
        value
            .parse::<usize>()
            .ok()
            .and_then(|x| self.value.generics.get(x))
    }

    pub fn get_defined(&self) -> impl Iterator<Item = String> {
        (0..self.value.generics.len()).map(|x| x.to_string())
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        matches!(other, TypeObject::Tuple(_))
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.base_name().hash(state)
    }
}

impl Default for TupleType {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

arc_partial_eq!(TupleType, Tuple);

type_obj_from!(TupleType, Tuple);

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use itertools::Itertools;

    use crate::converter::builtins::OBJECT;
    use crate::converter::type_obj::TupleType;

    #[test]
    fn empty_tuple_name() {
        let empty = TupleType::default();
        assert_eq!(empty.name(), "tuple");
        assert_eq!(empty.base_name(), "tuple");
        assert_eq!(empty.typedef_as("test".to_string()).name(), "test");
        assert_eq!(empty.typedef_as("test".to_string()).base_name(), "tuple");
    }

    #[test]
    fn tuple_name() {
        let nonempty = TupleType::new(vec![OBJECT.into()]);
        assert_eq!(nonempty.name(), "tuple[object]");
        assert_eq!(nonempty.base_name(), "tuple");
        assert_eq!(nonempty.typedef_as("test2".to_string()).name(), "test2");
        assert_eq!(
            nonempty.typedef_as("test2".to_string()).base_name(),
            "tuple"
        );
    }

    fn assert_defined<T: Debug + Ord>(ty: &TupleType, result: &mut [T])
    where
        String: PartialEq<T>,
    {
        result.sort();
        assert_eq!(ty.get_defined().sorted().collect_vec(), result);
    }

    #[test]
    fn tuple_defined() {
        for i in 0..100 {
            let ty = TupleType::new(vec![OBJECT.into(); i]);
            let mut result = (0..i).map(|x| x.to_string()).collect_vec();
            assert_defined(&ty, &mut result);
        }
    }
}
