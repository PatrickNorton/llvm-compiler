use std::borrow::Cow;
use std::collections::HashSet;
use std::sync::Arc;

use once_cell::sync::{Lazy, OnceCell};

use crate::converter::class::{AttributeInfo, MethodInfo};
use crate::converter::generic::GenericInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::macros::{
    arc_eq_hash, arc_partial_eq, try_from_type_obj, try_from_user_type, type_obj_from,
    user_type_from,
};
use super::user::{UserInfo, UserTypeInner, UserTypeLike};
use super::{TypeObject, UserType};

#[derive(Debug, Clone)]
pub struct UnionTypeObject {
    value: Arc<UnionTypeInner>,
}

#[derive(Debug)]
struct UnionTypeInner {
    info: Arc<UnionInfo>,
    typedef_name: Option<String>,
    generics: Vec<TypeObject>,
    is_const: bool,
}

#[derive(Debug)]
struct UnionInfo {
    info: UserInfo<MethodInfo, AttributeInfo>,
    variants: OnceCell<Vec<(String, TypeObject)>>,
    is_const_class: bool,
}

impl UnionTypeObject {
    pub fn new(name: String, supers: Option<Vec<TypeObject>>, generics: GenericInfo) -> Self {
        Self {
            value: Arc::new(UnionTypeInner {
                info: Arc::new(UnionInfo {
                    info: UserInfo::new(name, supers, generics),
                    variants: OnceCell::new(),
                    is_const_class: false,
                }),
                typedef_name: None,
                generics: Vec::new(),
                is_const: false,
            }),
        }
    }

    pub fn new_predefined(name: String, generics: GenericInfo) -> Self {
        Self::new(name, None, generics)
    }

    fn clone_with_const(&self, is_const: bool) -> Self {
        Self {
            value: Arc::new(UnionTypeInner {
                info: self.value.info.clone(),
                typedef_name: None,
                generics: self.get_generics().to_vec(),
                is_const,
            }),
        }
    }

    pub fn get_generic_info(&self) -> &GenericInfo {
        &self.get_info().info
    }

    pub fn get_generics(&self) -> &[TypeObject] {
        &self.value.generics
    }

    pub fn set_supers(&self, supers: Vec<TypeObject>) {
        self.get_info()
            .supers
            .set(supers)
            .expect("Supers should only be set once")
    }

    pub fn is_final(&self) -> bool {
        true
    }

    pub fn name(&self) -> Cow<'_, str> {
        let info = self.get_info();
        UserType::std_name(
            &info.name,
            &self.value.generics,
            self.value.is_const,
            &self.value.typedef_name,
            self.value.info.is_const_class,
        )
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(UnionTypeInner {
                info: self.value.info.clone(),
                typedef_name: Some(name),
                generics: self.value.generics.clone(),
                is_const: self.value.is_const,
            }),
        }
    }

    pub fn is_const_class(&self) {
        todo!()
    }

    pub fn variant_count(&self) -> u16 {
        self.value
            .info
            .variants
            .get()
            .unwrap()
            .len()
            .try_into()
            .expect("Too many variants")
    }

    pub fn variant_name(&self, index: u16) -> Option<&str> {
        self.value
            .info
            .variants
            .get()
            .unwrap()
            .get(index as usize)
            .map(|(x, _)| &**x)
    }

    pub fn variant_info(&self, index: &str) -> Option<(u16, &TypeObject)> {
        for (i, (name, ty)) in self.value.info.variants.get().unwrap().iter().enumerate() {
            if name == index {
                // FIXME: Generics
                return Some((i.try_into().unwrap(), ty));
            }
        }
        None
    }

    pub fn variant_number(&self, index: &str) -> Option<u16> {
        self.variant_info(index).map(|(x, _)| x)
    }

    pub fn variant_type(&self, index: &str) -> Option<&TypeObject> {
        self.variant_info(index).map(|(_, x)| x)
    }

    pub(super) fn get_info(&self) -> &UserInfo<MethodInfo, AttributeInfo> {
        &self.value.info.info
    }

    pub fn set_variants(&self, variants: Vec<(String, TypeObject)>) {
        self.value
            .info
            .variants
            .set(variants)
            .expect("Cannot set variants more than once")
    }

    pub fn contract(&self) -> &(HashSet<String>, HashSet<OpSpTypeNode>) {
        static EMPTY: Lazy<(HashSet<String>, HashSet<OpSpTypeNode>)> =
            Lazy::new(|| (HashSet::new(), HashSet::new()));
        &EMPTY
    }
}

impl UserTypeLike for UnionTypeObject {
    fn same_base_type(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::Union(u) => Arc::ptr_eq(&self.value.info, &u.value.info),
            _ => false,
        }
    }

    fn const_semantics(&self) -> bool {
        self.value.info.is_const_class
    }

    fn make_const(&self) -> Self {
        if !self.value.is_const {
            self.clone_with_const(true)
        } else {
            self.clone()
        }
    }

    fn make_mut(&self) -> Self {
        if self.value.is_const {
            self.clone_with_const(false)
        } else {
            self.clone()
        }
    }

    fn get_supers(&self) -> &[TypeObject] {
        self.get_info().supers.get().unwrap()
    }
}

impl UserTypeInner for UnionTypeObject {
    type Operator = MethodInfo;

    type Attribute = AttributeInfo;

    fn get_info(&self) -> &UserInfo<Self::Operator, Self::Attribute> {
        &self.value.info.info
    }

    fn typedef_name(&self) -> &Option<String> {
        &self.value.typedef_name
    }

    fn generics(&self) -> &[TypeObject] {
        &self.value.generics
    }

    fn is_const(&self) -> bool {
        self.value.is_const
    }
}

arc_eq_hash!(UnionTypeObject);
arc_partial_eq!(UnionTypeObject, Union);

user_type_from!(UnionTypeObject, Union);
try_from_user_type!(UnionTypeObject, Union);

type_obj_from!(UnionTypeObject, Union);
try_from_type_obj!(UnionTypeObject, Union);
