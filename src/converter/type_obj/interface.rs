use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use derive_new::new;
use itertools::Itertools;
use once_cell::sync::OnceCell;

use crate::converter::class::{AttributeInfo, MethodInfo};
use crate::converter::error::CompilerException;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::UserType;
use crate::converter::CompileResult;
use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;

use super::macros::{
    arc_eq_hash, arc_partial_eq, try_from_type_obj, try_from_user_type, type_obj_from,
    user_type_from,
};
use super::user::{UserInfo, UserTypeInner, UserTypeLike};
use super::TypeObject;

#[derive(Debug, Clone)]
pub struct InterfaceType {
    value: Arc<InterfaceTypeInner>,
}

#[derive(Debug, new)]
pub struct InterfaceFnInfo {
    info: MethodInfo,
    has_impl: bool,
}

#[derive(Debug, new)]
pub struct InterfaceAttrInfo {
    info: AttributeInfo,
    has_impl: bool,
}

#[derive(Debug)]
struct InterfaceTypeInner {
    info: Arc<InterfaceInfo>,
    typedef_name: Option<String>,
    generics: Vec<TypeObject>,
    is_const: bool,
}

#[derive(Debug)]
struct InterfaceInfo {
    info: UserInfo<InterfaceFnInfo, InterfaceAttrInfo>,
    cached_contract: OnceCell<(HashSet<String>, HashSet<OpSpTypeNode>)>,
}

impl InterfaceType {
    pub fn new(name: String, generics: GenericInfo, supers: Option<Vec<TypeObject>>) -> Self {
        Self {
            value: Arc::new(InterfaceTypeInner {
                info: Arc::new(InterfaceInfo {
                    info: UserInfo::new(name, supers, generics),
                    cached_contract: OnceCell::new(),
                }),
                typedef_name: None,
                generics: Vec::new(),
                is_const: true,
            }),
        }
    }

    pub fn new_operators(
        name: String,
        generics: GenericInfo,
        operators: HashMap<OpSpTypeNode, MethodInfo>,
    ) -> Self {
        let this = Self::new(name, generics, Some(Vec::new()));
        this.set_operators(
            operators
                .into_iter()
                .map(|(op, x)| (op, InterfaceFnInfo::new(x, false)))
                .collect(),
        );
        this.seal();
        this
    }

    pub fn new_predefined(name: String, generics: GenericInfo) -> Self {
        Self::new(name, generics, None)
    }

    fn clone_with_const(&self, is_const: bool) -> Self {
        Self {
            value: Arc::new(InterfaceTypeInner {
                info: self.value.info.clone(),
                typedef_name: None,
                generics: self.get_generics().to_vec(),
                is_const,
            }),
        }
    }

    pub fn get_generics(&self) -> &[TypeObject] {
        &self.value.generics
    }

    pub fn is_final(&self) -> bool {
        false
    }

    pub fn get_generic_info(&self) -> &GenericInfo {
        &self.get_info().info
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.value.typedef_name.as_deref().map_or_else(
            || {
                if self.value.generics.is_empty() {
                    Cow::Borrowed(&self.get_info().name)
                } else {
                    Cow::Owned(format!(
                        "{}[{}]",
                        self.get_info().name,
                        self.value.generics.iter().map(|x| x.name()).format(", ")
                    ))
                }
            },
            Cow::Borrowed,
        )
    }

    pub(super) fn get_info(&self) -> &UserInfo<InterfaceFnInfo, InterfaceAttrInfo> {
        &self.value.info.info
    }

    pub fn generify(
        &self,
        line_info: impl Lined,
        args: Vec<TypeObject>,
    ) -> CompileResult<TypeObject> {
        let generic_info = self.get_generic_info();
        let true_args = generic_info.generify(args);
        if let Option::Some(true_args) = true_args {
            if true_args.len() != generic_info.len() {
                Err(
                    CompilerException::of("Cannot generify object in this manner", line_info)
                        .into(),
                )
            } else {
                Ok(InterfaceType {
                    value: Arc::new(InterfaceTypeInner {
                        info: self.value.info.clone(),
                        typedef_name: self.typedef_name().clone(),
                        generics: true_args,
                        is_const: self.is_const(),
                    }),
                }
                .into())
            }
        } else {
            Err(CompilerException::of("Cannot generify object in this manner", line_info).into())
        }
    }

    pub fn contract(&self) -> &(HashSet<String>, HashSet<OpSpTypeNode>) {
        self.value
            .info
            .cached_contract
            .get_or_init(|| self.get_contract())
    }

    fn get_contract(&self) -> (HashSet<String>, HashSet<OpSpTypeNode>) {
        let info = self.get_info();
        let attributes = info
            .attributes
            .get()
            .expect("Attributes should be set before contract");
        let static_attributes = info
            .static_attributes
            .get()
            .expect("Attributes should be set before contract");
        let mut methods = HashSet::with_capacity(attributes.len() + static_attributes.len());
        for (name, attr_info) in attributes {
            if !attr_info.has_impl {
                methods.insert(name.clone());
            }
        }
        for (name, attr_info) in static_attributes {
            if !attr_info.has_impl {
                methods.insert(name.clone());
            }
        }
        let operators = info
            .operators
            .get()
            .expect("Operators should be set before contract");
        let mut ops = operators
            .iter()
            .filter(|(_, x)| !x.has_impl)
            .map(|(&x, _)| x)
            .collect::<HashSet<_>>();
        for sup in info.supers.get().unwrap() {
            // Interfaces are the only classes with contracts
            if let TypeObject::Interface(i) = sup {
                let (contract_methods, contract_ops) = i.contract();
                for method in contract_methods {
                    if !attributes.contains_key(method) && !static_attributes.contains_key(method) {
                        methods.insert(method.clone());
                    }
                }
                for &operator in contract_ops {
                    if !operators.contains_key(&operator) {
                        ops.insert(operator);
                    }
                }
            }
        }
        (methods, ops)
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(InterfaceTypeInner {
                info: self.value.info.clone(),
                typedef_name: Some(name),
                generics: self.value.generics.clone(),
                is_const: self.value.is_const,
            }),
        }
    }
}

impl UserTypeLike for InterfaceType {
    fn same_base_type(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::Interface(i) => Arc::ptr_eq(&self.value.info, &i.value.info),
            _ => false,
        }
    }

    fn const_semantics(&self) -> bool {
        false
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

impl UserTypeInner for InterfaceType {
    type Operator = InterfaceFnInfo;

    type Attribute = InterfaceAttrInfo;

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

impl AsRef<MethodInfo> for InterfaceFnInfo {
    fn as_ref(&self) -> &MethodInfo {
        &self.info
    }
}

impl AsRef<AttributeInfo> for InterfaceAttrInfo {
    fn as_ref(&self) -> &AttributeInfo {
        &self.info
    }
}

arc_eq_hash!(InterfaceType);
arc_partial_eq!(InterfaceType, Interface);

user_type_from!(InterfaceType, Interface);
try_from_user_type!(InterfaceType, Interface);

type_obj_from!(InterfaceType, Interface);
try_from_type_obj!(InterfaceType, Interface);

#[cfg(test)]
mod tests {
    use crate::converter::access_handler::AccessLevel;
    use crate::converter::argument::ArgumentInfo;
    use crate::converter::builtins::OBJECT;
    use crate::converter::class::{AttributeInfo, MethodInfo};
    use crate::converter::fn_info::FunctionInfo;
    use crate::converter::generic::GenericInfo;
    use crate::converter::mutable::MutableType;
    use crate::converter::type_obj::{
        InterfaceAttrInfo, InterfaceFnInfo, InterfaceType, TemplateParam, TupleType, TypeObject,
        TypeTypeObject, UserTypeLike,
    };
    use crate::macros::{hash_map, hash_set};
    use crate::parser::line_info::LineInfo;
    use crate::parser::operator_sp::OpSpTypeNode;

    #[test]
    fn simple_name() {
        let ty = InterfaceType::new("test".to_string(), GenericInfo::empty(), Some(Vec::new()));
        assert_eq!(ty.name(), "test");
        let typedefed = ty.typedef_as("test2".to_string());
        assert_eq!(typedefed.name(), "test2");
    }

    #[test]
    fn generic_name() {
        let generic = GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = InterfaceType::new("test".to_string(), generic, Some(Vec::new()));
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
        let ty = InterfaceType::new("test".to_string(), generic, Some(Vec::new()));
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
        let ty = InterfaceType::new("test".to_string(), generics, Some(Vec::new()));
        assert_eq!(ty.get_generics(), Vec::<TypeObject>::new());
        let generified = ty.generify(LineInfo::empty(), vec![OBJECT.into()]).unwrap();
        assert_eq!(generified.get_generics(), &[OBJECT]);
    }

    #[test]
    fn same_base_type() {
        let generic = GenericInfo::new(vec![TemplateParam::new("T".to_string(), 0, OBJECT.into())]);
        let ty = InterfaceType::new("test".to_string(), generic, Some(Vec::new()));
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

    fn sample_attr_info() -> AttributeInfo {
        AttributeInfo::new(
            false,
            AccessLevel::Public,
            MutableType::Standard,
            OBJECT.into(),
            LineInfo::empty(),
        )
    }

    fn sample_method_info() -> MethodInfo {
        MethodInfo::new(
            LineInfo::empty(),
            AccessLevel::Public,
            false,
            FunctionInfo::new(
                LineInfo::empty(),
                "test_method".into(),
                false,
                GenericInfo::empty(),
                ArgumentInfo::empty(),
                Vec::new(),
            ),
        )
    }

    #[test]
    fn simple_contract() {
        let ty = InterfaceType::new("test".to_string(), GenericInfo::empty(), Some(Vec::new()));
        ty.seal();
        let (methods, ops) = ty.get_contract();
        assert!(
            methods.is_empty(),
            "methods should be empty, got: {:?}",
            methods
        );
        assert!(ops.is_empty(), "ops should be empty, got: {:?}", ops);
    }

    #[test]
    fn empty_contract() {
        let ty = InterfaceType::new("test".to_string(), GenericInfo::empty(), Some(Vec::new()));
        let attr_info = InterfaceAttrInfo::new(sample_attr_info(), true);
        ty.set_attributes(hash_map!("foo".to_string() => attr_info));
        let op_info = InterfaceFnInfo::new(sample_method_info(), true);
        ty.set_operators(hash_map!(OpSpTypeNode::New => op_info));
        ty.seal();
        let (methods, ops) = ty.get_contract();
        assert!(
            methods.is_empty(),
            "methods should be empty, got: {:?}",
            methods
        );
        assert!(ops.is_empty(), "ops should be empty, got: {:?}", ops);
    }

    #[test]
    fn nonempty_contract() {
        let ty = InterfaceType::new("test".to_string(), GenericInfo::empty(), Some(Vec::new()));
        let attr_info = InterfaceAttrInfo::new(sample_attr_info(), false);
        ty.set_attributes(hash_map!("foo".to_string() => attr_info));
        let op_info = InterfaceFnInfo::new(sample_method_info(), false);
        ty.set_operators(hash_map!(OpSpTypeNode::New => op_info));
        ty.seal();
        let (methods, ops) = ty.get_contract();
        assert_eq!(methods, hash_set!("foo".to_string()));
        assert_eq!(ops, hash_set!(OpSpTypeNode::New));
    }
}
