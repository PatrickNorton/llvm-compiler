use std::borrow::Cow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::zip;
use std::sync::atomic::{AtomicBool, Ordering};

use itertools::Itertools;
use once_cell::sync::OnceCell;

use crate::converter::access_handler::AccessLevel;
use crate::converter::builtins::THROWS_TYPE;
use crate::converter::class::{AttributeInfo, MethodInfo};
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::CompileResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator_sp::OpSpTypeNode;

use super::{InterfaceType, StdTypeObject, TypeObject, UnionTypeObject};

pub(super) use self::private::UserTypeInner;

#[derive(Debug, Clone)]
pub enum UserType {
    Interface(InterfaceType),
    Std(StdTypeObject),
    Union(UnionTypeObject),
}

#[derive(Debug)]
pub struct UserInfo<O, A> {
    pub(super) name: String,
    pub(super) supers: OnceCell<Vec<TypeObject>>,
    pub(super) operators: OnceCell<HashMap<OpSpTypeNode, O>>,
    pub(super) static_operators: OnceCell<HashMap<OpSpTypeNode, O>>,
    pub(super) info: GenericInfo,
    pub(super) attributes: OnceCell<HashMap<String, A>>,
    pub(super) static_attributes: OnceCell<HashMap<String, A>>,
    is_sealed: AtomicBool,
}

pub trait UserTypeLike: UserTypeInner + PartialEq<TypeObject> {
    fn same_base_type(&self, other: &TypeObject) -> bool;
    fn const_semantics(&self) -> bool;
    fn make_const(&self) -> Self;
    fn make_mut(&self) -> Self;
    fn get_supers(&self) -> &[TypeObject];

    fn is_subclass(&self, other: &TypeObject) -> bool {
        if self == other || self.same_base_type(&*THROWS_TYPE) {
            true
        } else if other.is_user_type() && self.same_base_type(other) {
            if !self.const_semantics() && user_is_const(other) && self.is_const() {
                false
            } else if user_generics(other).is_empty() {
                self.const_semantics() || user_is_const(other) || !self.is_const()
            } else if self.generics().is_empty() {
                false
            } else {
                zip(user_generics(other), self.generics()).all(|(x, y)| x.is_superclass(y))
            }
        } else if !self.is_const() && self.make_const() == *other {
            true
        } else {
            self.get_supers().iter().any(|x| other.is_superclass(x))
        }
    }

    fn is_superclass(&self, other: &TypeObject) -> bool {
        // TODO: Do without cloning
        other.is_subclass(&self.clone().into())
    }

    fn op_ret_with_generics(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        self.get_info().op_ret_with_generics(o, access)
    }

    fn true_operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<Cow<'_, FunctionInfo>>> {
        let info = self.get_info();
        let operators = info.operators.get();
        let op_info = match operators {
            Option::None => return Ok(None),
            Option::Some(ops) => match ops.get(&o) {
                Option::Some(op) => op.as_ref(),
                Option::None => return Ok(None),
            },
        };
        if self.is_const() && op_info.is_mut && o != OpSpTypeNode::New {
            Ok(None)
        } else if AccessLevel::can_access(op_info.access_level, access) {
            Ok(Some(Cow::Borrowed(&op_info.function_info)))
        } else {
            self.super_operator_info(o, access)
        }
    }

    fn operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<FunctionInfo>> {
        let true_info = self.true_operator_info(o, access)?;
        Ok(if self.generics().is_empty() {
            true_info.map(|x| x.boundify())
        } else {
            true_info.map(|x| x.generify(&self.clone().into(), self.generics().to_vec()))
        })
    }

    fn can_set_attr(&self, name: &str, access: AccessLevel) -> bool {
        if self.is_const() {
            return false;
        }
        if let Option::Some(attr) = self.get_info().attributes.get().unwrap().get(name) {
            let attr_info = attr.as_ref();
            let access_level = attr_info.get_access_level();
            if !AccessLevel::can_access(access_level, access) {
                false
            } else if access_level == AccessLevel::Pubget {
                AccessLevel::can_access(AccessLevel::Private, access)
            } else {
                !attr_info.get_mut_type().is_const_ref()
            }
        } else {
            false
        }
    }

    fn attr_type(
        &self,
        value: &str,
        access: AccessLevel,
    ) -> CompileResult<Option<Cow<'_, TypeObject>>> {
        self.attr_type_with_generics(value, access)
            .map(|x| x.map(|y| self.generify_attr_type(y)))
    }

    fn static_attr_type(&self, value: &str, access: AccessLevel) -> Option<Cow<'_, TypeObject>> {
        self.static_attr_type_with_generics(value, access)
            .map(|y| self.generify_attr_type(y))
    }

    fn set_generic_parent(&self) {
        self.get_info().info.set_parent(self.clone().into())
    }

    fn seal(&self) {
        self.get_info().seal();
    }

    fn set_operators(&self, operators: HashMap<OpSpTypeNode, Self::Operator>) {
        self.get_info()
            .operators
            .set(operators)
            .expect("Should only set operators once")
    }

    fn set_static_operators(&self, operators: HashMap<OpSpTypeNode, Self::Operator>) {
        self.get_info()
            .static_operators
            .set(operators)
            .expect("Should only set static operators once")
    }

    fn set_attributes(&self, attributes: HashMap<String, Self::Attribute>) {
        self.get_info()
            .attributes
            .set(attributes)
            .expect("Should only set attributes once")
    }

    fn set_static_attributes(&self, attributes: HashMap<String, Self::Attribute>) {
        self.get_info()
            .static_attributes
            .set(attributes)
            .expect("Should only set static attributes once")
    }
}

macro_rules! user_match_all {
    ($self:ident: $x:ident => $result:expr) => {
        match $self {
            UserType::Interface($x) => $result,
            UserType::Std($x) => $result,
            UserType::Union($x) => $result,
        }
    };
}

impl UserType {
    pub fn get_generics(&self) -> &[TypeObject] {
        user_match_all!(self: x => x.get_generics())
    }

    pub fn attr_ty_access(
        &self,
        attr: &str,
        access: AccessLevel,
    ) -> CompileResult<Option<Cow<'_, TypeObject>>> {
        user_match_all!(self: x => x.attr_type(attr, access))
    }

    pub fn operator_info(
        &self,
        o: OpSpTypeNode,
        info: &mut CompilerInfo,
    ) -> CompileResult<Option<FunctionInfo>> {
        self.op_info_access(o, info.user_access_level(self))
    }

    pub fn op_info_access(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<FunctionInfo>> {
        user_match_all!(self: x => x.operator_info(o, access))
    }

    pub fn try_operator_info(
        &self,
        line_info: &LineInfo,
        o: OpSpTypeNode,
        info: &mut CompilerInfo,
    ) -> CompileResult<FunctionInfo> {
        self.operator_info(o, info)?.ok_or_else(|| {
            match self.op_info_exception(line_info, o, info.user_access_level(self)) {
                Result::Ok(x) => x.into(),
                Result::Err(x) => x,
            }
        })
    }

    fn op_info_exception(
        &self,
        line_info: impl Lined,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<CompilerException> {
        if access != AccessLevel::Private && self.op_info_access(o, AccessLevel::Private)?.is_some()
        {
            Ok(CompilerException::of(
                format!(
                    "Cannot get '{}' from type '{}' operator has too strict of an access level",
                    o,
                    self.name()
                ),
                line_info,
            ))
        } else if self.make_mut().op_info_access(o, access)?.is_some() {
            Ok(CompilerException::of(
                format!("'{}' requires a mut variable for type '{}'", o, self.name()),
                line_info,
            ))
        } else {
            Ok(CompilerException::of(
                format!("'{}' does not exist in type '{}'", o, self.name()),
                line_info,
            ))
        }
    }

    pub fn operator_return_type(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        let types = self.op_ret_with_generics(o, access)?;
        Ok(types.map(|x| self.generify_all(&x)))
    }

    pub fn op_ret_with_generics(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        user_match_all!(self: x => x.get_info().op_ret_with_generics(o, access))
    }

    pub fn make_const(&self) -> UserType {
        user_match_all!(self: x => x.make_const().into())
    }

    pub fn make_mut(&self) -> UserType {
        user_match_all!(self: x => x.make_mut().into())
    }

    pub fn get_generic_info(&self) -> &GenericInfo {
        user_match_all!(self: x => x.get_generic_info())
    }

    pub fn is_final(&self) -> bool {
        user_match_all!(self: x => x.is_final())
    }

    pub fn get_supers(&self) -> &[TypeObject] {
        user_match_all!(self: x => x.get_supers())
    }

    pub fn name(&self) -> Cow<'_, str> {
        user_match_all!(self: x => x.name())
    }

    pub fn contract(&self) -> &(HashSet<String>, HashSet<OpSpTypeNode>) {
        user_match_all!(self: x => x.contract())
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        user_match_all!(self: x => x.same_base_type(other))
    }

    pub fn const_semantics(&self) -> bool {
        match self {
            UserType::Interface(_) => false,
            UserType::Std(s) => s.const_semantics(),
            UserType::Union(u) => u.const_semantics(),
        }
    }

    pub fn set_generic_parent(&self) {
        user_match_all!(self: x => x.set_generic_parent())
    }

    pub fn recursive_supers(&self) -> impl Iterator<Item = &TypeObject> {
        RecursiveSuperIter::new(self)
    }

    pub(super) fn std_name<'a>(
        base_name: &'a str,
        generics: &[TypeObject],
        is_const: bool,
        typedef_name: &'a Option<String>,
        is_const_class: bool,
    ) -> Cow<'a, str> {
        if let Option::Some(ty) = typedef_name {
            return Cow::Borrowed(ty);
        }
        if generics.is_empty() {
            let name = typedef_name.as_deref().unwrap_or(base_name);
            if is_const || is_const_class {
                name.into()
            } else {
                format!("mut {}", name).into()
            }
        } else {
            let values = generics.iter().map(|x| x.name()).format(", ");
            if is_const || is_const_class {
                format!("{}[{}]", base_name, values).into()
            } else {
                format!("mut {}[{}]", base_name, values).into()
            }
        }
    }

    fn generify_all(&self, values: &[TypeObject]) -> Vec<TypeObject> {
        let generics = self.get_generics();
        if generics.is_empty() {
            values.to_vec()
        } else {
            let full_self = self.clone().into();
            values
                .iter()
                .map(|x| x.generify_with(&full_self, generics.to_vec()))
                .collect_vec()
        }
    }
}

mod private {
    use std::borrow::Cow;
    use std::fmt::Debug;
    use std::hash::Hasher;
    use std::ptr;

    use crate::converter::access_handler::AccessLevel;
    use crate::converter::class::{AttributeInfo, MethodInfo};
    use crate::converter::fn_info::FunctionInfo;
    use crate::converter::mutable::MutableType;
    use crate::converter::type_obj::{ObjectType, TypeObject};
    use crate::converter::CompileResult;
    use crate::parser::operator_sp::OpSpTypeNode;

    use super::UserInfo;

    pub trait UserTypeInner: Into<TypeObject> + Clone {
        type Operator: AsRef<MethodInfo> + Debug;
        type Attribute: AsRef<AttributeInfo> + Debug;

        fn get_info(&self) -> &UserInfo<Self::Operator, Self::Attribute>;
        fn typedef_name(&self) -> &Option<String>;
        fn generics(&self) -> &[TypeObject];
        fn is_const(&self) -> bool;

        fn super_operator_info(
            &self,
            o: OpSpTypeNode,
            access: AccessLevel,
        ) -> CompileResult<Option<Cow<'_, FunctionInfo>>> {
            let new_access = if access == AccessLevel::Private {
                AccessLevel::Protected
            } else {
                access
            };
            for super_cls in self.get_info().supers.get().unwrap() {
                if let Option::Some(sup_attr) = super_cls.true_operator_info(o, new_access)? {
                    if op_has_impl(o, super_cls) {
                        return Ok(Some(sup_attr));
                    }
                }
            }
            static OBJECT: ObjectType = ObjectType::new();
            Ok(OBJECT.operator_info(o, access).map(Cow::Borrowed))
        }

        fn base_hash<H: Hasher>(&self, state: &mut H) {
            ptr::hash(self.get_info(), state)
        }

        fn attr_type_with_generics(
            &self,
            value: &str,
            access: AccessLevel,
        ) -> CompileResult<Option<Cow<'_, TypeObject>>> {
            let info = self.get_info();
            // Early return should only be taken during auto-interface check of superclass.
            // Given that, the auto interface will be applied to this type instead
            // of the superclass and thus still work.
            let attr = match info.attributes.get() {
                Option::None => return Ok(None),
                Option::Some(x) => x.get(value),
            };
            match attr {
                Option::None => {
                    let new_access = if access == AccessLevel::Private {
                        AccessLevel::Protected
                    } else {
                        access
                    };
                    for super_cls in info.supers.get().unwrap() {
                        let sup_attr = super_cls.attr_type_with_generics(value, new_access)?;
                        if let Option::Some(sup_attr) = sup_attr {
                            if attr_has_impl(value, super_cls) {
                                return Ok(Some(sup_attr));
                            }
                        }
                    }
                    Ok(None)
                }
                Option::Some(attr) => Ok(type_from_attr(self.is_const(), attr.as_ref(), access)),
            }
        }

        fn static_attr_type_with_generics(
            &self,
            value: &str,
            access: AccessLevel,
        ) -> Option<Cow<'_, TypeObject>> {
            let info = self.get_info();
            match info.static_attributes.get().unwrap().get(value) {
                Option::None => info
                    .info
                    .get_param_map()
                    .get(value)
                    .map(|x| Cow::Owned(x.get_type())),
                Option::Some(attr) => type_from_attr(self.is_const(), attr.as_ref(), access),
            }
        }

        fn generify_attr_type<'a>(&self, ty: Cow<'a, TypeObject>) -> Cow<'a, TypeObject> {
            if self.generics().is_empty() {
                ty
            } else {
                // TODO: Remove clone
                Cow::Owned(ty.generify_with(&self.clone().into(), self.generics().to_vec()))
            }
        }
    }

    fn type_from_attr(
        is_const: bool,
        attr: &AttributeInfo,
        access: AccessLevel,
    ) -> Option<Cow<'_, TypeObject>> {
        // TODO: Reduce unnecessary clones (make_mut and make_const)
        if attr.get_mut_type() == MutableType::MutMethod {
            if is_const {
                None
            } else {
                Some(Cow::Borrowed(attr.get_type()))
            }
        } else if is_const {
            if AccessLevel::can_access(attr.get_access_level(), access) {
                Some(Cow::Owned(attr.get_type().make_const()))
            } else {
                None
            }
        } else if attr.get_access_level() == AccessLevel::Pubget {
            if AccessLevel::can_access(AccessLevel::Private, access) {
                Some(Cow::Owned(attr.get_type().make_mut()))
            } else {
                Some(Cow::Owned(attr.get_type().make_const()))
            }
        } else if AccessLevel::can_access(attr.get_access_level(), access) {
            if attr.get_mut_type().is_const_type() {
                Some(Cow::Owned(attr.get_type().make_const()))
            } else {
                Some(Cow::Owned(attr.get_type().make_mut()))
            }
        } else {
            None
        }
    }

    fn attr_has_impl(name: &str, super_cls: &TypeObject) -> bool {
        match super_cls {
            TypeObject::Interface(i) => i.contract().0.contains(name),
            TypeObject::Std(s) => s.contract().0.contains(name),
            TypeObject::Union(u) => u.contract().0.contains(name),
            _ => true,
        }
    }

    fn op_has_impl(o: OpSpTypeNode, super_cls: &TypeObject) -> bool {
        match super_cls {
            TypeObject::Interface(i) => i.contract().1.contains(&o),
            TypeObject::Std(s) => s.contract().1.contains(&o),
            TypeObject::Union(u) => u.contract().1.contains(&o),
            _ => true,
        }
    }
}

impl<O: AsRef<MethodInfo>, A: AsRef<AttributeInfo>> UserInfo<O, A> {
    pub fn new(name: String, supers: Option<Vec<TypeObject>>, info: GenericInfo) -> Self {
        Self {
            name,
            supers: supers.map_or_else(OnceCell::new, |x| x.into()),
            operators: OnceCell::new(),
            static_operators: OnceCell::new(),
            info,
            attributes: OnceCell::new(),
            static_attributes: OnceCell::new(),
            is_sealed: AtomicBool::new(false),
        }
    }

    pub fn op_ret_with_generics(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        let operators = self
            .operators
            .get()
            .expect("Operators should have been initialized at this point");
        if let Option::Some(op) = operators.get(&o) {
            let op = op.as_ref();
            return Ok(AccessLevel::can_access(op.access_level, access)
                .then(|| op.function_info.get_returns().to_vec()));
        }
        for sup in self.supers.get().unwrap() {
            if let Option::Some(op_ret) = sup.op_ret_access(o, access)? {
                return Ok(Some(op_ret));
            }
        }
        Ok(None)
    }

    pub fn seal(&self) {
        self.is_sealed
            .compare_exchange(false, true, Ordering::Relaxed, Ordering::Relaxed)
            .unwrap_or_else(|_| panic!("Class {} sealed twice", self.name));
        self.supers.get_or_init(Vec::new);
        self.operators.get_or_init(|| HashMap::new());
        self.static_operators.get_or_init(|| HashMap::new());
        self.attributes.get_or_init(|| HashMap::new());
        self.static_attributes.get_or_init(|| HashMap::new());
    }
}

fn user_is_const(ty: &TypeObject) -> bool {
    match ty {
        TypeObject::Interface(i) => i.is_const(),
        TypeObject::Std(s) => s.is_const(),
        TypeObject::Union(u) => u.is_const(),
        _ => panic!("Expected user type here"),
    }
}

fn user_generics(ty: &TypeObject) -> &[TypeObject] {
    match ty {
        TypeObject::Interface(i) => i.generics(),
        TypeObject::Std(s) => s.generics(),
        TypeObject::Union(u) => u.generics(),
        _ => panic!("Expected user type here"),
    }
}

impl TryFrom<TypeObject> for UserType {
    type Error = TypeObject;

    fn try_from(value: TypeObject) -> Result<Self, Self::Error> {
        match value {
            TypeObject::Interface(i) => Ok(Self::Interface(i)),
            TypeObject::Std(s) => Ok(Self::Std(s)),
            TypeObject::Union(u) => Ok(Self::Union(u)),
            x => Err(x),
        }
    }
}

impl From<UserType> for TypeObject {
    fn from(x: UserType) -> Self {
        match x {
            UserType::Interface(i) => Self::Interface(i),
            UserType::Std(s) => Self::Std(s),
            UserType::Union(u) => Self::Union(u),
        }
    }
}

#[derive(Debug)]
struct RecursiveSuperIter<'a> {
    values: VecDeque<&'a TypeObject>,
}

impl<'a> RecursiveSuperIter<'a> {
    pub fn new(val: &'a UserType) -> Self {
        Self {
            values: val.get_supers().iter().collect(),
        }
    }
}

impl<'a> Iterator for RecursiveSuperIter<'a> {
    type Item = &'a TypeObject;

    fn next(&mut self) -> Option<Self::Item> {
        match self.values.pop_front()? {
            x @ TypeObject::Interface(i) => {
                self.values.extend(i.get_supers());
                Some(x)
            }
            x @ TypeObject::Std(s) => {
                self.values.extend(s.get_supers());
                Some(x)
            }
            x @ TypeObject::Union(u) => {
                self.values.extend(u.get_supers());
                Some(x)
            }
            x => Some(x),
        }
    }
}
