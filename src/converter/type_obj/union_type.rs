use std::borrow::Cow;
use std::collections::HashSet;
use std::sync::Arc;

use itertools::Itertools;
use once_cell::race::OnceBool;
use once_cell::sync::{Lazy, OnceCell};

use crate::converter::class::{AttributeInfo, MethodInfo};
use crate::converter::error::CompilerException;
use crate::converter::generic::GenericInfo;
use crate::converter::CompileResult;
use crate::parser::line_info::Lined;
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
    is_const_class: OnceBool,
}

impl UnionTypeObject {
    pub fn new(name: String, supers: Option<Vec<TypeObject>>, generics: GenericInfo) -> Self {
        Self {
            value: Arc::new(UnionTypeInner {
                info: Arc::new(UnionInfo {
                    info: UserInfo::new(name, supers, generics),
                    variants: OnceCell::new(),
                    is_const_class: OnceBool::new(),
                }),
                typedef_name: None,
                generics: Vec::new(),
                is_const: true,
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
            self.value.info.is_const_class.get().unwrap(),
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
        self.value.info.is_const_class.set(true).unwrap();
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

    pub fn generify(
        &self,
        line_info: impl Lined,
        args: Vec<TypeObject>,
    ) -> CompileResult<TypeObject> {
        let generic_info = self.get_generic_info();
        // TODO: Remove clone
        let true_args = generic_info.generify(args.clone());
        if let Option::Some(true_args) = true_args {
            if true_args.len() != generic_info.len() {
                Err(CompilerException::of(
                    format!(
                        "Cannot generify object in this manner: type {} by types [{}]",
                        self.name(),
                        args.iter().map(|x| x.name()).format(", "),
                    ),
                    line_info,
                )
                .into())
            } else {
                Ok(UnionTypeObject {
                    value: Arc::new(UnionTypeInner {
                        info: self.value.info.clone(),
                        typedef_name: self.typedef_name().clone(),
                        generics: true_args,
                        is_const: self.is_const(),
                    }),
                }
                .into())
            }
        } else {
            Err(CompilerException::of(
                format!(
                    "Cannot generify object in this manner: type {} by types [{}]",
                    self.name(),
                    args.iter().map(|x| x.name()).format(", "),
                ),
                line_info,
            )
            .into())
        }
    }

    pub fn generify_with(&self, parent: &TypeObject, values: Vec<TypeObject>) -> TypeObject {
        UnionTypeObject {
            value: Arc::new(UnionTypeInner {
                info: self.value.info.clone(),
                typedef_name: self.value.typedef_name.clone(),
                generics: self.generify_with_inner(parent, values),
                is_const: self.value.is_const,
            }),
        }
        .into()
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
        self.value.info.is_const_class.get().unwrap()
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

    fn seal(&self) {
        // FIXME: addFulfilledInterfaces
        self.value.info.is_const_class.get_or_init(|| true);
        self.get_info().seal();
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

#[cfg(test)]
mod tests {
    use crate::converter::builtins::OBJECT;
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{
        TemplateParam, TupleType, TypeObject, TypeTypeObject, UnionTypeObject, UserTypeLike,
    };
    use crate::parser::line_info::LineInfo;

    #[test]
    fn simple_name() {
        let ty = UnionTypeObject::new("test".to_string(), Some(Vec::new()), GenericInfo::empty());
        ty.seal();
        assert_eq!(ty.name(), "test");
        let typedefed = ty.typedef_as("test2".to_string());
        assert_eq!(typedefed.name(), "test2");
    }

    #[test]
    fn generic_name() {
        let generic = GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = UnionTypeObject::new("test".to_string(), Some(Vec::new()), generic);
        ty.seal();
        assert_eq!(ty.name(), "test");
        let typedefed = ty.typedef_as("test2".to_string());
        assert_eq!(typedefed.name(), "test2");
        let generified = ty.generify(LineInfo::empty(), vec![OBJECT.into()]).unwrap();
        assert_eq!(generified.name(), "test[object]");
        let gen_ty = generified.typedef_as("test3".to_string());
        assert_eq!(gen_ty.name(), "test3");
    }

    #[test]
    fn generify() {
        let generic = GenericInfo::new(vec![TemplateParam::new(
            "T".to_string(),
            0,
            TypeTypeObject::new_empty().into(),
        )]);
        let ty = UnionTypeObject::new("test".to_string(), Some(Vec::new()), generic);
        ty.seal();
        assert!(ty
            .generify(
                LineInfo::empty(),
                vec![TypeTypeObject::new(OBJECT.into()).into()]
            )
            .is_ok());
        assert!(ty.generify(LineInfo::empty(), vec![]).is_err());
        assert!(ty
            .generify(LineInfo::empty(), vec![TupleType::new(Vec::new()).into()])
            .is_err());
        assert!(ty
            .generify(
                LineInfo::empty(),
                vec![
                    TypeTypeObject::new(OBJECT.into()).into(),
                    TypeTypeObject::new(OBJECT.into()).into(),
                ]
            )
            .is_err())
    }

    #[test]
    fn generics() {
        let generics =
            GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = UnionTypeObject::new("test".to_string(), Some(Vec::new()), generics);
        ty.seal();
        assert_eq!(ty.get_generics(), Vec::<TypeObject>::new());
        let generified = ty.generify(LineInfo::empty(), vec![OBJECT.into()]).unwrap();
        assert_eq!(generified.get_generics(), &[OBJECT]);
    }

    #[test]
    fn same_base_type() {
        let generic = GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = UnionTypeObject::new("test".to_string(), Some(Vec::new()), generic);
        ty.seal();
        let ty_obj = ty.clone().into();
        assert!(ty.same_base_type(&ty_obj));
        let typedefed = ty.typedef_as("test2".to_string());
        assert!(typedefed.same_base_type(&ty_obj));
        assert!(ty.same_base_type(&typedefed.into()));
        let generified = ty.generify(LineInfo::empty(), vec![OBJECT.into()]).unwrap();
        assert!(generified.same_base_type(&ty_obj));
        assert!(ty.same_base_type(&generified));
        let gen_ty = generified.typedef_as("test3".to_string());
        assert!(gen_ty.same_base_type(&ty_obj));
        assert!(ty.same_base_type(&gen_ty));
    }
}
