use std::borrow::Cow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::{zip, FusedIterator};
use std::sync::atomic::{AtomicBool, Ordering};

use itertools::Itertools;
use once_cell::sync::OnceCell;

use crate::converter::access_handler::AccessLevel;
use crate::converter::builtins::{BuiltinRef, THROWS_TYPE};
use crate::converter::class::{AttributeInfo, MethodInfo};
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::global_info::GlobalCompilerInfo;
use crate::converter::CompileResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator_sp::OpSpTypeNode;

use super::error::AccessErrorType;
use super::supers::{SuperHolder, SuperRef};
use super::{InterfaceType, StdTypeObject, TypeObject, UnionTypeObject};

pub(super) use self::private::UserTypeInner;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UserType {
    Interface(InterfaceType),
    Std(StdTypeObject),
    Union(UnionTypeObject),
}

#[derive(Debug)]
pub struct UserInfo<O, A> {
    pub(super) name: String,
    pub(super) supers: SuperHolder,
    pub(super) operators: OnceCell<HashMap<OpSpTypeNode, O>>,
    pub(super) static_operators: OnceCell<HashMap<OpSpTypeNode, O>>,
    pub(super) info: GenericInfo,
    pub(super) attributes: OnceCell<HashMap<String, A>>,
    pub(super) static_attributes: OnceCell<HashMap<String, A>>,
    pub(super) def_info: LineInfo,
    is_sealed: AtomicBool,
}

pub trait UserTypeLike: UserTypeInner + PartialEq<TypeObject> {
    fn same_base_type(&self, other: &TypeObject) -> bool;
    fn const_semantics(&self) -> bool;
    fn make_const(&self) -> Self;
    fn make_mut(&self) -> Self;
    fn get_supers(&self) -> SuperRef<'_>;

    fn is_subclass(&self, other: &TypeObject) -> bool {
        if self == other || self.same_base_type(&THROWS_TYPE) {
            true
        } else if other.is_user_type() && self.same_base_type(other) {
            if !self.const_semantics() && !user_is_const(other) && self.is_const() {
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
            self.get_supers()
                .into_iter()
                .any(|x| other.is_superclass(x))
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
        builtins: BuiltinRef<'_>,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        self.get_info().op_ret_with_generics(o, access, builtins)
    }

    fn true_operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> Option<Cow<'_, FunctionInfo>> {
        let info = self.get_info();
        let operators = info.operators.get();
        let op_info = match operators {
            Option::None => return None,
            Option::Some(ops) => match ops.get(&o) {
                Option::Some(op) => op.as_ref(),
                Option::None => return None,
            },
        };
        if self.is_const() && op_info.is_mut && o != OpSpTypeNode::New {
            None
        } else if AccessLevel::can_access(op_info.access_level, access) {
            Some(Cow::Borrowed(&op_info.function_info))
        } else {
            self.super_operator_info(o, access, builtins)
        }
    }

    fn operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> Option<FunctionInfo> {
        let true_info = self.true_operator_info(o, access, builtins);
        if self.generics().is_empty() {
            true_info.map(|x| x.boundify())
        } else {
            true_info.map(|x| x.generify(&self.clone().into(), self.generics().to_vec()))
        }
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
    ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
        self.attr_type_with_generics(value, access)
            .map(|x| self.generify_attr_type(x))
    }

    fn static_attr_type(
        &self,
        value: &str,
        access: AccessLevel,
    ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
        self.static_attr_type_with_generics(value, access)
            .map(|y| self.generify_attr_type(y))
    }

    fn generify_as(
        &self,
        parent: &TypeObject,
        other: &TypeObject,
    ) -> Option<HashMap<u16, TypeObject>> {
        if self.is_superclass(other) {
            Some(HashMap::new())
        } else if self.same_base_type(other) {
            make_match(parent, self.generics(), other.get_generics())
        } else if let Result::Ok(user) = UserType::try_from(other.clone()) {
            for sup in user.recursive_supers() {
                if self.same_base_type(sup) {
                    let sup_generics = self.generics();
                    let obj_generics = sup.get_generics();
                    return make_match(parent, sup_generics, obj_generics);
                }
            }
            None
        } else {
            // FIXME: Add generification of function types that implement Callable
            None
        }
    }

    fn set_generic_parent(&self) {
        self.get_info().info.set_parent(self.clone().into())
    }

    fn seal(&self, global_info: Option<&GlobalCompilerInfo>, builtins: Option<BuiltinRef<'_>>) {
        if let (Option::Some(global_info), Option::Some(builtins)) = (global_info, builtins) {
            self.add_fulfilled_interfaces(global_info, builtins);
        }
        self.get_info().seal();
    }

    fn seal_without_interfaces(&self) {
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

    fn add_fulfilled_interfaces(&self, global_info: &GlobalCompilerInfo, builtins: BuiltinRef<'_>) {
        assert!(!self.get_info().is_sealed.load(Ordering::Relaxed));
        let fulfilled = private::fulfilled_interfaces(self, global_info, builtins).unwrap();
        if !fulfilled.is_empty() {
            self.get_info()
                .supers
                .fulfilled_interfaces
                .set(fulfilled)
                .expect("Fulfilled interfaces should only be set once")
        }
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
    ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
        user_match_all!(self: x => x.attr_type(attr, access))
    }

    pub fn operator_info(&self, o: OpSpTypeNode, info: &mut CompilerInfo) -> Option<FunctionInfo> {
        self.op_info_access(o, info.user_access_level(self), info.builtins())
    }

    pub fn op_info_access(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> Option<FunctionInfo> {
        user_match_all!(self: x => x.operator_info(o, access, builtins))
    }

    pub fn try_operator_info(
        &self,
        line_info: &LineInfo,
        o: OpSpTypeNode,
        info: &mut CompilerInfo,
    ) -> CompileResult<FunctionInfo> {
        self.operator_info(o, info).ok_or_else(|| {
            self.op_info_exception(line_info, o, info.user_access_level(self), info.builtins())
                .into()
        })
    }

    fn op_info_exception(
        &self,
        line_info: impl Lined,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> CompilerException {
        if access != AccessLevel::Private
            && self
                .op_info_access(o, AccessLevel::Private, builtins)
                .is_some()
        {
            CompilerException::of(
                format!(
                    "Cannot get '{}' from type '{}' operator has too strict of an access level",
                    o,
                    self.name()
                ),
                line_info,
            )
        } else if self
            .make_mut()
            .op_info_access(o, access, builtins)
            .is_some()
        {
            CompilerException::of(
                format!("'{}' requires a mut variable for type '{}'", o, self.name()),
                line_info,
            )
        } else {
            CompilerException::of(
                format!("'{}' does not exist in type '{}'", o, self.name()),
                line_info,
            )
        }
    }

    pub fn operator_return_type(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        let types = self.op_ret_with_generics(o, access, builtins)?;
        Ok(types.map(|x| self.generify_all(&x)))
    }

    pub fn op_ret_with_generics(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
    ) -> CompileResult<Option<Vec<TypeObject>>> {
        user_match_all!(self: x => x.get_info().op_ret_with_generics(o, access, builtins))
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

    pub fn get_supers(&self) -> SuperRef<'_> {
        user_match_all!(self: x => x.get_supers())
    }

    pub fn name(&self) -> Cow<'_, str> {
        user_match_all!(self: x => x.name())
    }

    pub fn base_name(&self) -> &str {
        user_match_all!(self: x => &x.get_info().name)
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

    pub fn get_fields(&self) -> Box<dyn Iterator<Item = &str> + '_> {
        user_match_all!(self: x => Box::new(
            x.get_info()
                .attributes
                .get()
                .unwrap()
                .iter()
                .filter(|(_, x)| !x.as_ref().is_method())
                .map(|(x, _)| x.as_str()),
        ))
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
                format!("mut {name}").into()
            }
        } else {
            let values = generics.iter().map(|x| x.name()).format(", ");
            if is_const || is_const_class {
                format!("{base_name}[{values}]").into()
            } else {
                format!("mut {base_name}[{values}]").into()
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
    use crate::converter::builtins::{self, BuiltinRef};
    use crate::converter::class::{AttributeInfo, MethodInfo};
    use crate::converter::fn_info::FunctionInfo;
    use crate::converter::global_info::GlobalCompilerInfo;
    use crate::converter::mutable::MutableType;
    use crate::converter::type_obj::error::{AccessErrorType, AccessTooStrict};
    use crate::converter::type_obj::{InterfaceType, ObjectType, TypeObject};
    use crate::converter::CompileResult;
    use crate::parser::line_info::LineInfo;
    use crate::parser::operator_sp::OpSpTypeNode;

    use super::{UserInfo, UserTypeLike};

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
            builtins: BuiltinRef<'_>,
        ) -> Option<Cow<'_, FunctionInfo>> {
            let new_access = if access == AccessLevel::Private {
                AccessLevel::Protected
            } else {
                access
            };
            for super_cls in self.get_info().supers.iter() {
                if let Option::Some(sup_attr) =
                    super_cls.true_operator_info(o, new_access, builtins)
                {
                    if op_has_impl(o, super_cls) {
                        return Some(sup_attr);
                    }
                }
            }
            static OBJECT: ObjectType = ObjectType::new();
            OBJECT.operator_info(o, builtins).map(Cow::Borrowed)
        }

        fn base_hash<H: Hasher>(&self, state: &mut H) {
            ptr::hash(self.get_info(), state)
        }

        fn attr_type_with_generics(
            &self,
            value: &str,
            access: AccessLevel,
        ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
            let info = self.get_info();
            // Early return should only be taken during auto-interface check of superclass.
            // Given that, the auto interface will be applied to this type instead
            // of the superclass and thus still work.
            let attr = info
                .attributes
                .get()
                .ok_or(AccessErrorType::NotFound)?
                .get(value);
            match attr {
                Option::None => {
                    let new_access = if access == AccessLevel::Private {
                        AccessLevel::Protected
                    } else {
                        access
                    };
                    for super_cls in info.supers.iter() {
                        let sup_attr = super_cls.attr_type_with_generics(value, new_access);
                        if let Result::Ok(sup_attr) = sup_attr {
                            if attr_has_impl(value, super_cls) {
                                return Ok(sup_attr);
                            }
                        }
                    }
                    Err(AccessErrorType::NotFound) // FIXME: Get correct error
                }
                Option::Some(attr) => type_from_attr(self.is_const(), attr.as_ref(), access),
            }
        }

        fn static_attr_type_with_generics(
            &self,
            value: &str,
            access: AccessLevel,
        ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
            let info = self.get_info();
            match info.static_attributes.get().unwrap().get(value) {
                Option::None => info
                    .info
                    .get_param_map()
                    .get(value)
                    .map(|x| Cow::Owned(x.get_type()))
                    .ok_or(AccessErrorType::NotFound),
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

        fn generify_with_inner(
            &self,
            parent: &TypeObject,
            values: Vec<TypeObject>,
        ) -> Vec<TypeObject> {
            // TODO: Remove clone here
            if self.clone().into().same_base_type(parent) {
                return values;
            }
            let mut result = Vec::with_capacity(self.generics().len());
            for generic in self.generics() {
                if let TypeObject::Template(template) = generic {
                    if template.get_parent().same_base_type(parent) {
                        let value = &values[template.get_index()];
                        result.push(if template.is_vararg() {
                            TypeObject::list([value.clone()])
                        } else {
                            value.clone()
                        })
                    } else {
                        result.push(generic.clone())
                    }
                } else {
                    result.push(generic.generify_with(parent, values.clone()))
                }
            }
            result
        }
    }

    pub(super) fn fulfilled_interfaces(
        cls: &impl UserTypeLike,
        global_info: &GlobalCompilerInfo,
        builtins: BuiltinRef<'_>,
    ) -> CompileResult<Vec<TypeObject>> {
        let auto_interfaces;
        // If the default interfaces haven't been set yet (occurs when parsing
        // builtins), get only the global default interfaces
        let default_interfaces = match global_info.get_default_interfaces() {
            Option::Some(x) => x,
            Option::None => {
                auto_interfaces = builtins::auto_interfaces();
                &auto_interfaces
            }
        };
        default_interfaces
            .iter()
            .filter(|&ty| {
                !cls.is_subclass(&ty.clone().into()) && fulfills_contract(cls, ty, builtins)
            })
            .map(|ty| ty.generify(LineInfo::empty(), generified_params(cls, ty, builtins)))
            .collect()
    }

    fn fulfills_contract(
        cls: &impl UserTypeLike,
        contractor: &InterfaceType,
        builtins: BuiltinRef<'_>,
    ) -> bool {
        let (names, ops) = contractor.contract();
        names
            .iter()
            .all(|attr| cls.attr_type(attr, AccessLevel::Public).is_ok())
            && ops.iter().all(|&op| {
                cls.operator_info(op, AccessLevel::Public, builtins)
                    .is_some()
            })
    }

    fn generified_params(
        cls: &impl UserTypeLike,
        contractor: &InterfaceType,
        builtins: BuiltinRef<'_>,
    ) -> Vec<TypeObject> {
        let generic_count = contractor.get_info().info.len();
        if generic_count == 0 {
            return Vec::new();
        }
        let contractor_t = contractor.clone().into();
        let mut result = vec![None; generic_count];
        let (names, ops) = contractor.contract();
        for attr in names {
            let attr_t = cls
                .attr_type_with_generics(attr, AccessLevel::Public)
                .unwrap();
            let contractor_attr = contractor
                .attr_type_with_generics(attr, AccessLevel::Public)
                .unwrap();
            for (index, val) in contractor_attr.generify_as(&contractor_t, &attr_t).unwrap() {
                let index = index as usize;
                if let Option::Some(ty) = result[index].take() {
                    result[index] = Some(TypeObject::union(builtins, [ty, val]));
                } else {
                    result[index] = Some(val);
                }
            }
        }
        for &op in ops {
            let attr_t = cls
                .true_operator_info(op, AccessLevel::Public, builtins)
                .unwrap();
            let contractor_attr = contractor
                .true_operator_info(op, AccessLevel::Public, builtins)
                .unwrap();
            let attr = contractor_attr
                .to_callable()
                .generify_as(&contractor_t, &attr_t.to_callable());
            for (index, val) in attr.unwrap() {
                let index = index as usize;
                if let Option::Some(ty) = result[index].take() {
                    result[index] = Some(TypeObject::union(builtins, [ty, val]));
                } else {
                    result[index] = Some(val);
                }
            }
        }
        result.into_iter().map(|x| x.unwrap()).collect()
    }

    fn type_from_attr(
        is_const: bool,
        attr: &AttributeInfo,
        access: AccessLevel,
    ) -> Result<Cow<'_, TypeObject>, AccessErrorType> {
        // TODO: Reduce unnecessary clones (make_mut and make_const)
        if attr.get_mut_type() == MutableType::MutMethod {
            if is_const {
                Err(AccessErrorType::NeedsMut)
            } else {
                Ok(Cow::Borrowed(attr.get_type()))
            }
        } else if is_const {
            if AccessLevel::can_access(attr.get_access_level(), access) {
                Ok(Cow::Owned(attr.get_type().make_const()))
            } else {
                Err(AccessErrorType::NeedsMut)
            }
        } else if attr.get_access_level() == AccessLevel::Pubget {
            if AccessLevel::can_access(AccessLevel::Private, access) {
                Ok(Cow::Owned(attr.get_type().make_mut()))
            } else {
                Ok(Cow::Owned(attr.get_type().make_const()))
            }
        } else if AccessLevel::can_access(attr.get_access_level(), access) {
            if attr.get_mut_type().is_const_type() {
                Ok(Cow::Owned(attr.get_type().make_const()))
            } else {
                Ok(Cow::Owned(attr.get_type().make_mut()))
            }
        } else {
            Err(AccessErrorType::WeakAccess(AccessTooStrict {
                level_gotten: access,
                level_expected: attr.get_access_level(),
            }))
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
    pub fn new(
        name: String,
        supers: Option<Vec<TypeObject>>,
        info: GenericInfo,
        def_info: LineInfo,
    ) -> Self {
        Self {
            name,
            supers: SuperHolder::new(supers.map_or_else(OnceCell::new, |x| x.into())),
            operators: OnceCell::new(),
            static_operators: OnceCell::new(),
            info,
            attributes: OnceCell::new(),
            static_attributes: OnceCell::new(),
            is_sealed: AtomicBool::new(false),
            def_info,
        }
    }

    pub fn op_ret_with_generics(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: BuiltinRef<'_>,
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
        for sup in self.supers.iter() {
            if let Option::Some(op_ret) = sup.op_ret_access(o, access, builtins) {
                return Ok(Some(op_ret));
            }
        }
        Ok(None)
    }

    pub fn seal(&self) {
        self.is_sealed
            .compare_exchange(false, true, Ordering::Relaxed, Ordering::Relaxed)
            .unwrap_or_else(|_| panic!("Class {} sealed twice", self.name));
        self.supers.supers.get_or_init(Vec::new);
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

fn make_match(
    parent: &TypeObject,
    sup_generics: &[TypeObject],
    obj_generics: &[TypeObject],
) -> Option<HashMap<u16, TypeObject>> {
    if sup_generics.is_empty() && obj_generics.is_empty() {
        return Some(HashMap::new());
    } else if sup_generics.is_empty() {
        return None;
    } else if obj_generics.is_empty() {
        return Some(HashMap::new());
    }
    assert_eq!(sup_generics.len(), obj_generics.len());
    let mut result = HashMap::with_capacity(sup_generics.len());
    for (sup_g, obj_g) in zip(sup_generics, obj_generics) {
        match (sup_g, obj_g) {
            (TypeObject::Template(param), _) => {
                if param.get_parent().same_base_type(parent) {
                    result.insert(param.get_index().try_into().unwrap(), obj_g.clone());
                } else {
                    return None;
                }
            }
            (TypeObject::List(_), TypeObject::List(_)) => {
                let generics = sup_g.generify_as(parent, obj_g)?;
                if !TypeObject::add_generics_to_map(generics, &mut result) {
                    return None;
                }
            }
            _ if sup_g != obj_g => return None,
            _ => {}
        }
    }
    Some(result)
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
            values: val.get_supers().into_iter().collect(),
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

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.values.is_empty() {
            (0, Some(0))
        } else {
            (self.values.len(), None)
        }
    }
}

impl<'a> FusedIterator for RecursiveSuperIter<'a> {}
