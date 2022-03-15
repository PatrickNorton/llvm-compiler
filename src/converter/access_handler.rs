use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::parser::descriptor::DescriptorNode;

use super::type_obj::{BaseType, TypeObject, UserType};

#[derive(Debug)]
pub struct AccessHandler {
    classes_with_access: HashMap<BaseType, usize>,
    classes_with_protected: HashMap<BaseType, usize>,
    cls_types: Vec<TypeObject>,
    super_types: Vec<TypeObject>,
    constructors: Vec<TypeObject>,
    defined_in_file: HashSet<BaseType>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AccessLevel {
    Public,
    Private,
    Protected,
    Pubget,
    File,
}

impl AccessHandler {
    pub fn new() -> Self {
        Self {
            classes_with_access: HashMap::new(),
            classes_with_protected: HashMap::new(),
            cls_types: Vec::new(),
            super_types: Vec::new(),
            constructors: Vec::new(),
            defined_in_file: HashSet::new(),
        }
    }

    pub fn get_cls(&self) -> Option<&TypeObject> {
        self.cls_types.last()
    }

    pub fn get_super(&self) -> Option<&TypeObject> {
        self.super_types.last()
    }

    pub fn access_level(&self, obj: &TypeObject) -> AccessLevel {
        // FIXME: Do this without cloning
        let base = if let TypeObject::Type(ty) = obj {
            BaseType::new(ty.represented_type().clone())
        } else {
            BaseType::new(obj.clone())
        };
        self.access_lvl_inner(&base)
    }

    pub fn user_access_level(&self, obj: &UserType) -> AccessLevel {
        // FIXME: Do this without cloning
        self.access_lvl_inner(&BaseType::new(obj.clone().into()))
    }

    fn access_lvl_inner(&self, base: &BaseType) -> AccessLevel {
        if self.defined_in_file.contains(base) {
            if self.classes_with_access.contains_key(base) {
                AccessLevel::Private
            } else {
                AccessLevel::Public
            }
        } else if self.classes_with_access.contains_key(base) {
            AccessLevel::Private
        } else if self.classes_with_protected.contains_key(base) {
            AccessLevel::Protected
        } else {
            AccessLevel::Public
        }
    }

    pub fn allow_private_access(&mut self, obj: TypeObject) {
        increment(BaseType::new(obj), &mut self.classes_with_access)
    }

    pub fn remove_private_access(&mut self, obj: TypeObject) {
        decrement(BaseType::new(obj), &mut self.classes_with_access)
    }

    pub fn allow_protected_access(&mut self, obj: TypeObject) {
        increment(BaseType::new(obj), &mut self.classes_with_protected)
    }

    pub fn remove_protected_access(&mut self, obj: TypeObject) {
        decrement(BaseType::new(obj), &mut self.classes_with_protected)
    }

    pub fn add_cls(&mut self, obj: TypeObject) {
        self.cls_types.push(obj)
    }

    pub fn remove_cls(&mut self) {
        self.cls_types.pop();
    }

    pub fn add_super(&mut self, obj: TypeObject) {
        self.super_types.push(obj);
    }

    pub fn remove_super(&mut self) {
        self.super_types.pop();
    }

    pub fn enter_constructor(&mut self, ty: TypeObject) {
        self.constructors.push(ty)
    }

    pub fn exit_constructor(&mut self) {
        self.constructors.pop();
    }

    pub fn is_in_constructor(&self, ty: &TypeObject) -> bool {
        self.constructors.iter().any(|x| x.same_base_type(ty))
    }

    pub fn set_defined_in_file(&mut self, defined: HashSet<BaseType>) {
        assert!(self.defined_in_file.is_empty());
        self.defined_in_file = defined;
    }
}

const DESCRIPTOR_PAIRS: &[(DescriptorNode, AccessLevel)] = &[
    (DescriptorNode::Public, AccessLevel::Public),
    (DescriptorNode::Private, AccessLevel::Private),
    (DescriptorNode::Protected, AccessLevel::Protected),
    (DescriptorNode::Pubget, AccessLevel::Pubget),
];

impl AccessLevel {
    pub fn from_descriptors(descriptors: &HashSet<DescriptorNode>) -> AccessLevel {
        DESCRIPTOR_PAIRS
            .iter()
            .find(|x| descriptors.contains(&x.0))
            .map(|x| x.1)
            .unwrap_or(AccessLevel::File)
    }

    pub fn can_access(value_level: AccessLevel, access_level: AccessLevel) -> bool {
        match value_level {
            AccessLevel::Public | AccessLevel::Pubget => true,
            AccessLevel::Private => access_level == AccessLevel::Private,
            AccessLevel::Protected => {
                access_level == AccessLevel::Private || access_level == AccessLevel::Protected
            }
            AccessLevel::File => {
                access_level == AccessLevel::Private || access_level == AccessLevel::File
            }
        }
    }
}

fn increment<T: Hash + Eq>(value: T, map: &mut HashMap<T, usize>) {
    match map.entry(value) {
        Entry::Occupied(mut e) => *e.get_mut() += 1,
        Entry::Vacant(e) => {
            e.insert(1);
        }
    };
}

fn decrement<T: Hash + Eq>(value: T, map: &mut HashMap<T, usize>) {
    *map.get_mut(&value).unwrap() -= 1;
}

impl Default for AccessHandler {
    fn default() -> Self {
        Self::new()
    }
}
