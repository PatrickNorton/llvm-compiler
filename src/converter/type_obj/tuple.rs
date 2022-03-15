use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use itertools::Itertools;

use crate::converter::error::CompilerException;
use crate::converter::CompileResult;
use crate::parser::line_info::Lined;

use super::macros::{arc_eq_hash, type_obj_from};
use super::TypeObject;

#[derive(Debug, Clone)]
pub struct TupleType {
    value: Arc<TupleInner>,
}

#[derive(Debug)]
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

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.base_name().hash(state)
    }
}

arc_eq_hash!(TupleType);

type_obj_from!(TupleType, Tuple);
