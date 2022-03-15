use std::borrow::Cow;
use std::collections::HashSet;
use std::sync::Arc;

use once_cell::sync::Lazy;

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
pub struct StdTypeObject {
    value: Arc<TypeObjInner>,
}

#[derive(Debug)]
struct TypeObjInner {
    info: Arc<StdInfo>,
    typedef_name: Option<String>,
    generics: Vec<TypeObject>,
    is_const: bool,
}

#[derive(Debug)]
struct StdInfo {
    info: UserInfo<MethodInfo, AttributeInfo>,
    is_const_class: bool,
    is_final: bool,
}

impl StdTypeObject {
    pub fn new(
        name: String,
        supers: Option<Vec<TypeObject>>,
        info: GenericInfo,
        is_final: bool,
    ) -> Self {
        Self {
            value: Arc::new(TypeObjInner {
                info: Arc::new(StdInfo {
                    info: UserInfo::new(name, supers, info),
                    is_const_class: false,
                    is_final,
                }),
                typedef_name: None,
                generics: Vec::new(),
                is_const: true,
            }),
        }
    }

    pub fn new_predefined(name: String, info: GenericInfo) -> Self {
        Self::new(name, None, info, true)
    }

    fn clone_with_const(&self, is_const: bool) -> Self {
        Self {
            value: Arc::new(TypeObjInner {
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

    pub fn is_final(&self) -> bool {
        self.value.info.is_final
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

    pub fn is_const_class(&self) {
        todo!()
    }

    pub fn set_supers(&self, supers: Vec<TypeObject>) {
        self.get_info()
            .supers
            .set(supers)
            .expect("Supers should not be set more than once")
    }

    pub(super) fn get_info(&self) -> &UserInfo<MethodInfo, AttributeInfo> {
        &self.value.info.info
    }

    pub fn contract(&self) -> &(HashSet<String>, HashSet<OpSpTypeNode>) {
        static EMPTY: Lazy<(HashSet<String>, HashSet<OpSpTypeNode>)> =
            Lazy::new(|| (HashSet::new(), HashSet::new()));
        &EMPTY
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(TypeObjInner {
                info: self.value.info.clone(),
                typedef_name: Some(name),
                generics: self.value.generics.clone(),
                is_const: self.value.is_const,
            }),
        }
    }
}

impl UserTypeLike for StdTypeObject {
    fn same_base_type(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::Std(s) => Arc::ptr_eq(&self.value.info, &s.value.info),
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

impl UserTypeInner for StdTypeObject {
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

arc_eq_hash!(StdTypeObject);
arc_partial_eq!(StdTypeObject, Std);

user_type_from!(StdTypeObject, Std);
try_from_user_type!(StdTypeObject, Std);

type_obj_from!(StdTypeObject, Std);
try_from_type_obj!(StdTypeObject, Std);
