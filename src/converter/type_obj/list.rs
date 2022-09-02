use std::borrow::Cow;
use std::collections::HashMap;
use std::iter::zip;
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

    pub fn is_superclass(&self, other: &TypeObject) -> bool {
        if let TypeObject::List(list) = other {
            if self.get_values().len() != list.get_values().len() {
                false
            } else {
                zip(self.get_values(), list.get_values()).all(|(x, y)| x.is_superclass(y))
            }
        } else {
            panic!(
                "Should not be instancing list types: {} and {}",
                self.name(),
                other.name()
            )
        }
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

    pub fn generify_as(
        &self,
        parent: &TypeObject,
        other: &TypeObject,
    ) -> Option<HashMap<u16, TypeObject>> {
        let other = match other {
            TypeObject::List(l) => l,
            _ => return None,
        };
        let mut result = HashMap::new();
        for (val, other_val) in zip(self, other) {
            let map = val.generify_as(parent, other_val)?;
            if !TypeObject::add_generics_to_map(map, &mut result) {
                return None;
            }
        }
        Some(result)
    }
}

arc_eq_hash!(ListTypeObject);

type_obj_from!(ListTypeObject, List);
try_from_type_obj!(ListTypeObject, List);

impl Index<usize> for ListTypeObject {
    type Output = TypeObject;

    fn index(&self, index: usize) -> &Self::Output {
        &self.value.values[index]
    }
}

impl<'a> IntoIterator for &'a ListTypeObject {
    type Item = &'a TypeObject;

    type IntoIter = std::slice::Iter<'a, TypeObject>;

    fn into_iter(self) -> Self::IntoIter {
        self.value.values.iter()
    }
}

impl Default for ListTypeObject {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::converter::builtins::OBJECT;
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{
        ListTypeObject, StdTypeObject, TemplateParam, TupleType, TypeObject, TypeTypeObject,
        UserTypeLike,
    };
    use crate::macros::hash_map;
    use crate::parser::line_info::LineInfo;

    fn list_name_types() -> Vec<(ListTypeObject, &'static str)> {
        vec![
            (ListTypeObject::new(Vec::new()), "[]"),
            (ListTypeObject::new(vec![OBJECT.into()]), "[object]"),
            (
                ListTypeObject::new(vec![OBJECT.into(), TupleType::default().into()]),
                "[object, tuple]",
            ),
            (
                ListTypeObject::new(vec![
                    OBJECT.into(),
                    TupleType::default().into(),
                    TypeTypeObject::new(OBJECT.into()).into(),
                ]),
                "[object, tuple, type[object]]",
            ),
        ]
    }

    #[test]
    fn list_name() {
        for (ty, name) in list_name_types() {
            assert_eq!(ty.name(), name);
        }
    }

    #[test]
    fn list_typedef() {
        for (i, (ty, _)) in list_name_types().into_iter().enumerate() {
            assert_eq!(
                ty.typedef_as(format!("test_{}", i)).name(),
                format!("test_{}", i)
            )
        }
    }

    fn assert_super(x: &ListTypeObject, y: &TypeObject) {
        assert!(
            x.is_superclass(y),
            "{} is not a superclass of {}",
            x.name(),
            y.name(),
        );
    }

    fn assert_not_super(x: &ListTypeObject, y: &TypeObject) {
        assert!(
            !x.is_superclass(y),
            "{} is a superclass of {}",
            x.name(),
            y.name(),
        );
    }

    #[test]
    fn list_super() {
        assert_super(
            &ListTypeObject::new(Vec::new()),
            &ListTypeObject::new(Vec::new()).into(),
        );
        assert_super(
            &ListTypeObject::new(vec![OBJECT.into()]),
            &ListTypeObject::new(vec![OBJECT.into()]).into(),
        );
        assert_super(
            &ListTypeObject::new(vec![OBJECT.into()]),
            &ListTypeObject::new(vec![TupleType::default().into()]).into(),
        );
    }

    #[test]
    fn list_not_super() {
        assert_not_super(
            &ListTypeObject::new(vec![TupleType::default().into()]),
            &ListTypeObject::new(vec![OBJECT.into()]).into(),
        );
        assert_not_super(
            &ListTypeObject::new(vec![]),
            &ListTypeObject::new(vec![OBJECT.into()]).into(),
        );
        assert_not_super(
            &ListTypeObject::new(vec![OBJECT.into()]),
            &ListTypeObject::new(vec![]).into(),
        );
    }

    #[test]
    fn empty_generify_as() {
        let list = ListTypeObject::default();
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param]);
        let parent = StdTypeObject::new(
            "parent".into(),
            Some(Vec::new()),
            generic_info,
            true,
            LineInfo::empty(),
        );
        parent.set_generic_parent();
        assert_eq!(
            list.generify_as(&parent.into(), &list.clone().into()),
            Some(HashMap::new())
        );
        assert_eq!(
            list.generify_as(&list.clone().into(), &list.clone().into()),
            Some(HashMap::new())
        );
    }

    #[test]
    fn single_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new(
            "parent".into(),
            Some(Vec::new()),
            generic_info,
            true,
            LineInfo::empty(),
        );
        parent.set_generic_parent();
        let list = ListTypeObject::new(vec![param.into()]);
        let result = ListTypeObject::new(vec![OBJECT.into()]);
        assert_eq!(
            list.generify_as(&parent.into(), &result.into()),
            Some(hash_map!(0 => OBJECT.into()))
        );
    }

    #[test]
    fn non_list_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new(
            "parent".into(),
            Some(Vec::new()),
            generic_info,
            true,
            LineInfo::empty(),
        );
        parent.set_generic_parent();
        let list = ListTypeObject::new(vec![param.into()]);
        let result = TupleType::new(vec![OBJECT.into()]);
        assert_eq!(list.generify_as(&parent.into(), &result.into()), None);
    }
}
