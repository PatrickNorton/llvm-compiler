use std::borrow::Cow;
use std::ops::Index;
use std::sync::Arc;

use itertools::Itertools;

use super::macros::{arc_eq_hash, try_from_type_obj, type_obj_from};
use super::{TemplateParam, TypeObject};

#[derive(Debug, Clone)]
pub struct ListTypeObject {
    value: Arc<ListTypeInner>,
}

#[derive(Debug)]
struct ListTypeInner {
    values: Vec<TypeObject>,
    typedef_name: Option<String>,
}

impl ListTypeObject {
    pub fn new(values: Vec<TypeObject>) -> Self {
        Self {
            value: Arc::new(ListTypeInner {
                values,
                typedef_name: None,
            }),
        }
    }

    pub fn get_values(&self) -> &[TypeObject] {
        &self.value.values
    }

    pub fn name(&self) -> Cow<'_, str> {
        if let Option::Some(typedef) = &self.value.typedef_name {
            Cow::Borrowed(typedef)
        } else {
            format!(
                "[{}]",
                self.value.values.iter().map(|x| x.name()).format(", ")
            )
            .into()
        }
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        "".into()
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(ListTypeInner {
                values: self.value.values.clone(),
                typedef_name: Some(name),
            }),
        }
    }

    pub fn generify_with(&self, parent: &TypeObject, args: Vec<TypeObject>) -> TypeObject {
        if self.value.values.len() == 1
            && <&TemplateParam>::try_from(&self.value.values[0])
                .map_or_else(|_| false, |x| x.is_vararg())
        {
            self.value.values[0].generify_with(parent, args)
        } else {
            ListTypeObject::new(
                self.value
                    .values
                    .iter()
                    .map(|x| x.generify_with(parent, args.clone()))
                    .collect(),
            )
            .into()
        }
    }
}

arc_eq_hash!(ListTypeObject);

type_obj_from!(ListTypeObject, List);
try_from_type_obj!(ListTypeObject, List);

impl Index<usize> for ListTypeObject {
    type Output = TypeObject;

    fn index(&self, index: usize) -> &Self::Output {
        &self.value.values[0]
    }
}
