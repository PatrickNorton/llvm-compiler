use std::borrow::Cow;
use std::collections::HashSet;
use std::sync::Arc;

use itertools::Itertools;
use once_cell::race::OnceBool;
use once_cell::sync::Lazy;

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
    is_const_class: OnceBool,
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
                    is_const_class: OnceBool::new(),
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
            self.value.info.is_const_class.get().unwrap_or(false),
        )
    }

    pub fn is_const_class(&self) {
        self.value
            .info
            .is_const_class
            .set(true)
            .expect("Can only set value once");
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
                Ok(StdTypeObject {
                    value: Arc::new(TypeObjInner {
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
        StdTypeObject {
            value: Arc::new(TypeObjInner {
                info: self.value.info.clone(),
                typedef_name: self.value.typedef_name.clone(),
                generics: self.generify_with_inner(parent, values),
                is_const: self.value.is_const,
            }),
        }
        .into()
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
        self.get_info().seal();
        self.value.info.is_const_class.get_or_init(|| false);
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

#[cfg(test)]
mod tests {
    use crate::converter::builtins::OBJECT;
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{
        StdTypeObject, TemplateParam, TupleType, TypeObject, TypeTypeObject, UserTypeLike,
    };
    use crate::parser::line_info::LineInfo;

    #[test]
    fn simple_name() {
        let ty = StdTypeObject::new(
            "test".to_string(),
            Some(Vec::new()),
            GenericInfo::empty(),
            false,
        );
        ty.seal();
        assert_eq!(ty.name(), "test");
        let typedefed = ty.typedef_as("test2".to_string());
        assert_eq!(typedefed.name(), "test2");
    }

    #[test]
    fn generic_name() {
        let generic = GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = StdTypeObject::new("test".to_string(), Some(Vec::new()), generic, false);
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
        let ty = StdTypeObject::new("test".to_string(), Some(Vec::new()), generic, false);
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
        let ty = StdTypeObject::new("test".to_string(), Some(Vec::new()), generics, false);
        ty.seal();
        assert_eq!(ty.get_generics(), Vec::<TypeObject>::new());
        let generified = ty.generify(LineInfo::empty(), vec![OBJECT.into()]).unwrap();
        assert_eq!(generified.get_generics(), &[OBJECT]);
    }

    #[test]
    fn same_base_type() {
        let generic = GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = StdTypeObject::new("test".to_string(), Some(Vec::new()), generic, false);
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
