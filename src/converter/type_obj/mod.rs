mod base;
mod defined;
mod fn_info;
mod interface;
mod list;
mod macros;
mod module;
mod object;
mod option;
mod standard;
mod template;
mod tuple;
mod type_type;
mod union_type;
mod user;

pub use self::base::BaseType;
use self::defined::{get_defined, static_defined};
pub use self::fn_info::{FunctionInfoType, GenerifiedFnInfoType};
pub use self::interface::{InterfaceAttrInfo, InterfaceFnInfo, InterfaceType};
pub use self::list::ListTypeObject;
pub use self::module::ModuleType;
pub use self::object::ObjectType;
pub use self::option::OptionTypeObject;
pub use self::standard::StdTypeObject;
pub use self::template::TemplateParam;
pub use self::tuple::TupleType;
pub use self::type_type::TypeTypeObject;
pub use self::union_type::UnionTypeObject;
use self::user::UserTypeInner;
pub use self::user::{UserType, UserTypeLike};

use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::hash::Hasher;
use std::iter::zip;

use itertools::Itertools;

use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::levenshtein;

use super::access_handler::AccessLevel;
use super::builtins::{Builtins, OBJECT};
use super::compiler_info::CompilerInfo;
use super::error::CompilerException;
use super::fn_info::FunctionInfo;
use super::{CompileResult, CompileTypes};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeObject {
    FnInfo(FunctionInfoType),
    GenerifiedFn(GenerifiedFnInfoType),
    Interface(InterfaceType),
    List(ListTypeObject),
    Module(ModuleType),
    Object(ObjectType),
    Option(OptionTypeObject),
    Std(StdTypeObject),
    Template(TemplateParam),
    Tuple(TupleType),
    Type(TypeTypeObject),
    Union(UnionTypeObject),
}

impl TypeObject {
    pub fn union<const N: usize>(info: &CompilerInfo, values: [TypeObject; N]) -> TypeObject {
        Self::union_hash(info.builtins(), values.into_iter().collect())
    }

    pub fn union_of(info: &CompilerInfo, values: Vec<TypeObject>) -> TypeObject {
        Self::union_hash(info.builtins(), values.into_iter().collect())
    }

    fn union_hash(builtins: &Builtins, mut values: HashSet<TypeObject>) -> TypeObject {
        if values.len() == 1 {
            values.into_iter().next().unwrap()
        } else {
            values.remove(builtins.throws_type());
            if values.is_empty() {
                return builtins.throws_type().clone();
            }
            let mut current_super = None;
            let mut is_optional = false;
            for value in values {
                if &value == builtins.null_type() {
                    is_optional = true;
                } else if let TypeObject::Option(option) = value {
                    is_optional = true;
                    let option = option.get_option_val();
                    Self::get_super(&mut current_super, option.clone());
                } else {
                    Self::get_super(&mut current_super, value);
                }
            }
            match current_super {
                None => builtins.null_type().clone(),
                Some(x) => {
                    if is_optional {
                        Self::optional(x)
                    } else {
                        x
                    }
                }
            }
        }
    }

    fn get_super(current: &mut Option<TypeObject>, new: TypeObject) {
        match current.take() {
            None => *current = Some(new),
            Some(old) => *current = Some(Self::super_of(old, new)),
        }
    }

    fn super_of(a: TypeObject, b: TypeObject) -> TypeObject {
        if a.is_superclass(&b) {
            return a;
        } else if b.is_superclass(&a) {
            return b;
        }
        let (user_a, user_b) = match (UserType::try_from(a), UserType::try_from(b)) {
            (Ok(a), Ok(b)) => (a, b),
            (Err(a), _) => todo!("'getSuper' on non-user type '{}'", a.name()),
            (_, Err(b)) => todo!("'getSuper' on non-user type '{}'", b.name()),
        };
        let mut a_supers = HashSet::new();
        let mut b_supers = HashSet::new();
        for (a_sup, b_sup) in zip(user_a.recursive_supers(), user_b.recursive_supers()) {
            if a_sup.is_superclass(b_sup) {
                return a_sup.clone();
            } else if b_sup.is_superclass(a_sup) {
                return b_sup.clone();
            } else if b_supers.contains(a_sup) {
                return a_sup.clone();
            } else if a_supers.contains(b_sup) {
                return b_sup.clone();
            } else {
                a_supers.insert(a_sup);
                b_supers.insert(b_sup);
            }
        }
        OBJECT.into()
    }

    pub fn list<const N: usize>(args: [TypeObject; N]) -> TypeObject {
        ListTypeObject::new(args.into()).into()
    }

    pub fn list_of(args: Vec<TypeObject>) -> TypeObject {
        ListTypeObject::new(args).into()
    }

    #[inline]
    pub fn optional(obj: TypeObject) -> TypeObject {
        OptionTypeObject::new(obj).into()
    }

    pub fn name(&self) -> Cow<'_, str> {
        match self {
            TypeObject::FnInfo(f) => f.name(),
            TypeObject::GenerifiedFn(g) => g.name(),
            TypeObject::Interface(i) => i.name(),
            TypeObject::List(l) => l.name(),
            TypeObject::Module(m) => m.name(),
            TypeObject::Object(o) => o.name(),
            TypeObject::Option(o) => o.name(),
            TypeObject::Std(s) => s.name(),
            TypeObject::Template(t) => t.name(),
            TypeObject::Tuple(t) => t.name(),
            TypeObject::Type(t) => t.name(),
            TypeObject::Union(u) => u.name(),
        }
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        match self {
            TypeObject::FnInfo(f) => f.base_name(),
            TypeObject::GenerifiedFn(g) => g.base_name(),
            TypeObject::Interface(i) => Cow::Borrowed(&i.get_info().name),
            TypeObject::List(l) => l.base_name(),
            TypeObject::Module(m) => m.base_name(),
            TypeObject::Object(o) => o.base_name(),
            TypeObject::Option(o) => o.base_name(),
            TypeObject::Std(s) => Cow::Borrowed(&s.get_info().name),
            TypeObject::Template(t) => t.base_name(),
            TypeObject::Tuple(t) => t.base_name(),
            TypeObject::Type(t) => t.base_name(),
            TypeObject::Union(u) => Cow::Borrowed(&u.get_info().name),
        }
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        match self {
            TypeObject::FnInfo(f) => f.same_base_type(other),
            TypeObject::GenerifiedFn(g) => g.same_base_type(other),
            TypeObject::Interface(i) => i.same_base_type(other),
            TypeObject::List(_) => panic!(),
            TypeObject::Module(m) => m.same_base_type(other),
            TypeObject::Object(o) => o.same_base_type(other),
            TypeObject::Option(o) => o.same_base_type(other),
            TypeObject::Std(s) => s.same_base_type(other),
            TypeObject::Template(t) => t.same_base_type(other),
            TypeObject::Tuple(t) => t.same_base_type(other),
            TypeObject::Type(t) => t.same_base_type(other),
            TypeObject::Union(u) => u.same_base_type(other),
        }
    }

    pub fn is_superclass(&self, other: &TypeObject) -> bool {
        match self {
            TypeObject::List(l) => l.is_superclass(other),
            TypeObject::Object(o) => o.is_superclass(other),
            TypeObject::Type(t) => t.is_superclass(other),
            _ => other.is_subclass(self),
        }
    }

    pub fn is_subclass(&self, other: &TypeObject) -> bool {
        match self {
            TypeObject::FnInfo(f) => f.is_subclass(other),
            TypeObject::GenerifiedFn(_) => todo!(),
            TypeObject::Interface(i) => i.is_subclass(other),
            TypeObject::List(_) => panic!("Should not be instancing list types"),
            TypeObject::Module(_) => todo!(),
            TypeObject::Object(o) => o.is_subclass(other),
            TypeObject::Option(o) => o.is_subclass(other),
            TypeObject::Std(s) => s.is_subclass(other),
            TypeObject::Template(t) => t.is_subclass(other),
            TypeObject::Tuple(t) => t.is_subclass(other),
            TypeObject::Type(_) => todo!(),
            TypeObject::Union(u) => u.is_subclass(other),
        }
    }

    pub fn will_super_recurse(&self) -> bool {
        !matches!(
            self,
            TypeObject::List(_) | TypeObject::Object(_) | TypeObject::Type(_)
        )
    }

    pub fn strip_null(self) -> Self {
        match self {
            TypeObject::Option(o) => o.strip_null().clone(),
            x => x,
        }
    }

    pub fn is_option(&self) -> bool {
        matches!(self, TypeObject::Option(_))
    }

    pub fn make_const(&self) -> TypeObject {
        match self {
            TypeObject::Interface(i) => i.make_const().into(),
            TypeObject::Option(o) => o.make_const().into(),
            TypeObject::Std(s) => s.make_const().into(),
            TypeObject::Union(u) => u.make_const().into(),
            _ => self.clone(),
        }
    }

    pub fn make_mut(&self) -> TypeObject {
        match self {
            TypeObject::Interface(i) => i.make_mut().into(),
            TypeObject::Option(o) => o.make_mut().into(),
            TypeObject::Std(s) => s.make_mut().into(),
            TypeObject::Union(u) => u.make_mut().into(),
            _ => self.clone(),
        }
    }

    pub fn get_type(&self) -> TypeObject {
        TypeTypeObject::new(self.clone()).into()
    }

    pub fn get_generics(&self) -> &[TypeObject] {
        match self {
            TypeObject::Interface(i) => i.get_generics(),
            TypeObject::Std(s) => s.get_generics(),
            TypeObject::Tuple(t) => t.get_generics(),
            TypeObject::Type(t) => t.get_generics(),
            TypeObject::Union(u) => u.get_generics(),
            _ => &[],
        }
    }

    pub fn is_user_type(&self) -> bool {
        matches!(
            self,
            TypeObject::Interface(_) | TypeObject::Std(_) | TypeObject::Union(_)
        )
    }

    pub fn generify(
        &self,
        line_info: &dyn Lined,
        args: Vec<TypeObject>,
    ) -> CompileResult<TypeObject> {
        match self {
            TypeObject::FnInfo(f) => Ok(f.generify(args)),
            TypeObject::Interface(i) => i.generify(line_info, args),
            TypeObject::Std(s) => s.generify(line_info, args),
            TypeObject::Tuple(t) => t.generify(line_info, args),
            TypeObject::Type(t) => t.generify(line_info, args),
            TypeObject::Union(u) => u.generify(line_info, args),
            _ => Err(Self::generify_err(line_info).into()),
        }
    }

    fn generify_err(line_info: impl Lined) -> CompilerException {
        CompilerException::of("Cannot generify object", line_info)
    }

    pub fn generify_with(&self, parent: &TypeObject, values: Vec<TypeObject>) -> TypeObject {
        match self {
            TypeObject::FnInfo(f) => f.generify_with(parent, values),
            TypeObject::Interface(_) => todo!(),
            TypeObject::List(l) => l.generify_with(parent, values),
            TypeObject::Option(o) => o.generify_with(parent, values),
            TypeObject::Std(_) => todo!(),
            TypeObject::Template(t) => t.generify_with(parent, values),
            TypeObject::Union(_) => todo!(),
            _ => self.clone(),
        }
    }

    // FIXME? See if this can be done by returning references instead
    // FIXME: Determine whether this should return u16 or usize
    /// Returns the generics needed to transform one type into another.
    ///
    /// The purpose for this is for more complex generic transformations, e.g.
    /// finding the correct `T` such that `list[int] instanceof Iterable[T]`.
    /// The `parent` parameter is the parent of all
    /// [`TemplateParams`](TemplateParam) which are to be included in the
    /// transformation.
    ///
    /// If it is impossible for this class to ever be a subclass of the given
    /// class (in `other`), or it is impossible while only changing
    /// [`TemplateParams`](TemplateParam) with the given parent, `None` will be
    /// returned.
    ///
    /// If this type contains enough information to generify the parent
    /// completely, it will be possible to transform the returned map into a
    /// list with no empty indices. However, for parents with more complex
    /// generics, that may not be the case. If all information is given, then it
    /// will be true that, given
    /// ```ignore
    /// let params = transform(self.generify_as(parent, other))
    /// ```
    /// where `transform` is a function turning the returned [`HashMap`] into a
    /// [`Vec`], then
    /// ```ignore
    /// self.generify_with(parent, params)
    ///     .is_superclass(other.generify_with(parent, params))
    /// ```
    /// must always be `true`.
    pub fn generify_as(
        &self,
        parent: &TypeObject,
        other: &TypeObject,
    ) -> Option<HashMap<u16, TypeObject>> {
        match self {
            TypeObject::FnInfo(f) => f.generify_as(parent, other),
            TypeObject::GenerifiedFn(_) => None,
            TypeObject::Interface(i) => i.generify_as(parent, other),
            TypeObject::List(l) => l.generify_as(parent, other),
            TypeObject::Module(_) => None,
            TypeObject::Object(_) => Some(HashMap::new()),
            TypeObject::Option(o) => o.generify_as(parent, other),
            TypeObject::Std(s) => s.generify_as(parent, other),
            TypeObject::Template(t) => t.generify_as(parent, other),
            TypeObject::Tuple(t) => t.generify_as(parent, other),
            TypeObject::Type(_) => None,
            TypeObject::Union(u) => u.generify_as(parent, other),
        }
    }

    pub fn typedef_as(&self, name: String) -> TypeObject {
        match self {
            TypeObject::FnInfo(f) => f.typedef_as(name).into(),
            TypeObject::GenerifiedFn(f) => f.typedef_as(name).into(),
            TypeObject::Interface(i) => i.typedef_as(name).into(),
            TypeObject::List(l) => l.typedef_as(name).into(),
            TypeObject::Module(m) => m.typedef_as(name).into(),
            TypeObject::Object(o) => o.typedef_as(name).into(),
            TypeObject::Option(o) => o.typedef_as(name).into(),
            TypeObject::Std(s) => s.typedef_as(name).into(),
            TypeObject::Template(t) => t.typedef_as(name).into(),
            TypeObject::Tuple(t) => t.typedef_as(name).into(),
            TypeObject::Type(t) => t.typedef_as(name).into(),
            TypeObject::Union(u) => u.typedef_as(name).into(),
        }
    }

    pub fn operator_info(
        &self,
        o: OpSpTypeNode,
        info: &CompilerInfo,
    ) -> Option<Cow<'_, FunctionInfo>> {
        self.op_info_access(o, info.access_level(self), info.builtins())
    }

    pub fn op_info_access(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: &Builtins,
    ) -> Option<Cow<'_, FunctionInfo>> {
        match self {
            TypeObject::FnInfo(f) => f.operator_info(o).map(Cow::Borrowed),
            TypeObject::GenerifiedFn(f) => f.operator_info(o).map(Cow::Owned),
            TypeObject::Interface(i) => i.operator_info(o, access, builtins).map(Cow::Owned),
            TypeObject::List(_) => None,
            TypeObject::Module(_) => None,
            TypeObject::Object(ob) => ob.operator_info(o, builtins).map(Cow::Borrowed),
            TypeObject::Option(op) => op.operator_info(o, access, builtins),
            TypeObject::Std(s) => s.operator_info(o, access, builtins).map(Cow::Owned),
            TypeObject::Template(t) => t.operator_info(o, access, builtins),
            TypeObject::Tuple(t) => t.operator_info(o, builtins).map(Cow::Owned),
            TypeObject::Type(t) => t.operator_info(o, access, builtins).map(Cow::Owned),
            TypeObject::Union(u) => u.operator_info(o, access, builtins).map(Cow::Owned),
        }
    }

    pub fn try_operator_info(
        &self,
        line_info: impl Lined,
        o: OpSpTypeNode,
        info: &mut CompilerInfo,
    ) -> CompileResult<Cow<'_, FunctionInfo>> {
        self.try_op_info_access(line_info, o, info.access_level(self), info.builtins())
    }

    pub fn try_op_info_access(
        &self,
        line_info: impl Lined,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: &Builtins,
    ) -> CompileResult<Cow<'_, FunctionInfo>> {
        self.op_info_access(o, access, builtins).ok_or_else(|| {
            match self.op_info_exception(line_info, o, access, builtins) {
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
        builtins: &Builtins,
    ) -> CompileResult<CompilerException> {
        if access != AccessLevel::Private
            && self
                .op_info_access(o, AccessLevel::Private, builtins)
                .is_some()
        {
            Ok(CompilerException::of(
                format!(
                    "Cannot get '{}' from type '{}' operator has too strict of an access level",
                    o,
                    self.name()
                ),
                line_info,
            ))
        } else if self
            .make_mut()
            .op_info_access(o, access, builtins)
            .is_some()
        {
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
        info: &mut CompilerInfo,
    ) -> Option<Vec<TypeObject>> {
        self.op_ret_access(o, info.access_level(self), info.builtins())
    }

    pub fn op_ret_access(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: &Builtins,
    ) -> Option<Vec<TypeObject>> {
        self.op_info_access(o, access, builtins)
            .map(|i| i.get_returns().to_vec())
    }

    pub fn try_operator_return_type(
        &self,
        line_info: impl Lined,
        o: OpSpTypeNode,
        info: &mut CompilerInfo,
    ) -> CompileTypes {
        self.try_op_ret_access(line_info, o, info.access_level(self), info.builtins())
    }

    pub fn try_op_ret_access(
        &self,
        line_info: impl Lined,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: &Builtins,
    ) -> CompileTypes {
        self.try_op_info_access(line_info, o, access, builtins)
            .map(|x| x.get_returns().to_vec())
    }

    pub fn try_attr_type(
        &self,
        line_info: impl Lined,
        value: &str,
        info: &mut CompilerInfo,
    ) -> CompileResult<Cow<'_, TypeObject>> {
        match self {
            TypeObject::Type(t) => t.try_attr_type(line_info, value, info.access_level(self)),
            _ => Err(self
                .attr_exception(line_info, value, info.access_level(self))
                .into()),
        }
    }

    fn attr_exception(
        &self,
        line_info: impl Lined,
        name: &str,
        access: AccessLevel,
    ) -> CompilerException {
        // FIXME: Remove unwrap() and return error instead
        if access != AccessLevel::Private
            && self.attr_type_access(name, AccessLevel::Private).is_some()
        {
            CompilerException::of(
                format!(
                    "Cannot get attribute '{}' from type '{}': \
                     too strict of an access level required",
                    name,
                    self.name()
                ),
                line_info,
            )
        } else if self.make_mut().attr_type_access(name, access).is_some() {
            CompilerException::of(
                format!(
                    "Attribute '{}' requires a mut variable for '{}'",
                    name,
                    self.name()
                ),
                line_info,
            )
        } else {
            let closest = self
                .get_defined()
                .and_then(|x| levenshtein::closest_name(name, x));
            if let Option::Some(closest) = closest {
                CompilerException::of(
                    format!(
                        "Attribute '{}' does not exist in type '{}'\n\
                         Did you mean '{}'?",
                        name,
                        self.name(),
                        closest
                    ),
                    line_info,
                )
            } else {
                CompilerException::of(
                    format!(
                        "Attribute '{}' does not exist in type '{}'",
                        name,
                        self.name()
                    ),
                    line_info,
                )
            }
        }
    }

    pub fn attr_type(&self, name: &str, info: &mut CompilerInfo) -> Option<Cow<'_, TypeObject>> {
        self.attr_type_access(name, info.access_level(self))
    }

    pub fn attr_type_access(&self, name: &str, access: AccessLevel) -> Option<Cow<'_, TypeObject>> {
        match self {
            TypeObject::Interface(i) => i.attr_type(name, access),
            TypeObject::Module(m) => m.attr_type(name).map(Cow::Borrowed),
            TypeObject::Option(o) => o.attr_type(name).map(Cow::Owned),
            TypeObject::Std(s) => s.attr_type(name, access),
            TypeObject::Template(t) => t.attr_type(name, access),
            TypeObject::Tuple(t) => t.attr_type(name).map(Cow::Borrowed),
            TypeObject::Type(t) => t.attr_type(name, access),
            TypeObject::Union(u) => u.attr_type(name, access),
            _ => None,
        }
    }

    pub fn static_attr_type(&self, name: &str, access: AccessLevel) -> Option<Cow<'_, TypeObject>> {
        match self {
            TypeObject::Interface(i) => i.static_attr_type(name, access),
            TypeObject::Std(s) => s.static_attr_type(name, access),
            TypeObject::Template(t) => t.static_attr_type(name, access),
            TypeObject::Union(u) => u.static_attr_type(name, access),
            _ => None,
        }
    }

    pub fn try_static_attr_type(
        &self,
        line_info: &dyn Lined,
        name: &str,
        access: AccessLevel,
    ) -> CompileResult<Cow<'_, TypeObject>> {
        self.static_attr_type(name, access)
            .ok_or_else(|| self.static_attr_exception(line_info, name, access).into())
    }

    fn static_attr_exception(
        &self,
        line_info: &dyn Lined,
        name: &str,
        access: AccessLevel,
    ) -> CompilerException {
        if access != AccessLevel::Private
            && self.static_attr_type(name, AccessLevel::Private).is_some()
        {
            CompilerException::of(
                format!(
                    "Cannot get static attribute '{}' from type '{}': \
                     too strict of an access level required",
                    name,
                    self.name()
                ),
                line_info,
            )
        } else if self.make_mut().static_attr_type(name, access).is_some() {
            CompilerException::of(
                format!(
                    "Static attribute '{}' requires a mut variable for '{}'",
                    name,
                    self.name()
                ),
                line_info,
            )
        } else {
            let closest = self
                .static_defined()
                .and_then(|x| levenshtein::closest_name(name, x));
            if let Option::Some(closest) = closest {
                CompilerException::of(
                    format!(
                        "Static attribute '{}' does not exist in type '{}'\n\
                         Did you mean '{}'?",
                        name,
                        self.name(),
                        closest
                    ),
                    line_info,
                )
            } else {
                CompilerException::of(
                    format!(
                        "Static attribute '{}' does not exist in type '{}'",
                        name,
                        self.name()
                    ),
                    line_info,
                )
            }
        }
    }

    pub(self) fn attr_type_with_generics(
        &self,
        name: &str,
        access: AccessLevel,
    ) -> Option<Cow<'_, TypeObject>> {
        match self {
            TypeObject::Interface(i) => i.attr_type_with_generics(name, access),
            TypeObject::Std(s) => s.attr_type_with_generics(name, access),
            TypeObject::Union(u) => u.attr_type_with_generics(name, access),
            x => x.attr_type_access(name, access),
        }
    }

    pub fn can_set_attr(&self, name: &str, info: &mut CompilerInfo) -> bool {
        self.can_set_access(name, info.access_level(self))
    }

    pub fn can_set_access(&self, name: &str, access: AccessLevel) -> bool {
        match self {
            TypeObject::Interface(i) => i.can_set_attr(name, access),
            TypeObject::Std(s) => s.can_set_attr(name, access),
            TypeObject::Union(u) => u.can_set_attr(name, access),
            _ => false,
        }
    }

    pub fn true_operator_info(
        &self,
        o: OpSpTypeNode,
        access: AccessLevel,
        builtins: &Builtins,
    ) -> Option<Cow<'_, FunctionInfo>> {
        match self {
            TypeObject::Interface(i) => i.true_operator_info(o, access, builtins),
            TypeObject::Std(s) => s.true_operator_info(o, access, builtins),
            TypeObject::Union(u) => u.true_operator_info(o, access, builtins),
            x => x.op_info_access(o, access, builtins),
        }
    }

    pub fn add_generics_to_map(
        to_add: HashMap<u16, TypeObject>,
        result: &mut HashMap<u16, TypeObject>,
    ) -> bool {
        for (index, obj) in to_add {
            match result.entry(index) {
                Entry::Occupied(mut entry) => {
                    if obj.is_superclass(entry.get()) {
                        entry.insert(obj);
                    } else if !entry.get().is_superclass(&obj) {
                        return false;
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(obj);
                }
            }
        }
        true
    }

    pub fn get_defined(&self) -> Option<Box<dyn Iterator<Item = Cow<'_, str>> + '_>> {
        match self {
            TypeObject::Interface(i) => Some(Box::new(get_defined(i))),
            TypeObject::Option(o) => Some(Box::new(o.get_defined().map_into())),
            TypeObject::Std(s) => Some(Box::new(get_defined(s))),
            TypeObject::Template(t) => t.get_defined(),
            TypeObject::Tuple(t) => Some(Box::new(t.get_defined().map_into())),
            TypeObject::Type(t) => t.get_defined(),
            TypeObject::Union(u) => Some(Box::new(get_defined(u))),
            _ => None,
        }
    }

    pub fn static_defined(&self) -> Option<Box<dyn Iterator<Item = &'_ str> + '_>> {
        match self {
            TypeObject::Interface(i) => Some(Box::new(static_defined(i))),
            TypeObject::Std(s) => Some(Box::new(static_defined(s))),
            TypeObject::Template(t) => t.static_defined(),
            TypeObject::Union(u) => Some(Box::new(static_defined(u))),
            _ => None,
        }
    }

    pub(self) fn base_hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TypeObject::FnInfo(f) => f.base_hash(state),
            TypeObject::GenerifiedFn(f) => f.base_hash(state),
            TypeObject::Interface(i) => i.base_hash(state),
            TypeObject::List(_) => panic!("Should not be getting the base hash of list types"),
            TypeObject::Module(m) => m.base_hash(state),
            TypeObject::Object(o) => o.base_hash(state),
            TypeObject::Option(o) => o.base_hash(state),
            TypeObject::Std(s) => s.base_hash(state),
            TypeObject::Template(t) => t.base_hash(state),
            TypeObject::Tuple(t) => t.base_hash(state),
            TypeObject::Type(t) => t.base_hash(state),
            TypeObject::Union(u) => u.base_hash(state),
        }
    }
}
