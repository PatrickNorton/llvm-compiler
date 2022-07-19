use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::parser::descriptor::DescriptorNode;

use super::type_obj::{BaseType, TypeObject, UserType};

/// A class to determine which clearance a given access should get.
///
/// This class has the additional responsibility of determining any constructors
/// the current context is in: this is because constructors override mutability
/// rules. This responsibility is managed through the
/// [`enter_constructor`](Self::enter_constructor) and
/// [`exit_constructor`](Self::exit_constructor) methods.
///
/// # Access determination
///
/// Determining access to a field is dependent on the parent type of that
/// field, as well as the mutability of that access (only relevant for
/// `pubget` variables). This handler does not take into account the mutability
/// of accesses when determining the access level; that should be determined by
/// other means.
#[derive(Debug)]
pub struct AccessHandler {
    classes_with_access: HashMap<BaseType, usize>,
    classes_with_protected: HashMap<BaseType, usize>,
    cls_types: Vec<TypeObject>,
    super_types: Vec<TypeObject>,
    constructors: Vec<TypeObject>,
    defined_in_file: HashSet<BaseType>,
}

/// The level of access that an item can have.
///
/// This can be used in two contexts: as the access level for any variable or
/// variable-like object, or as the strength of accessibility the current
/// context uses when attempting to reference an object.
///
/// Note that the latter interpretation is not global for any object; each
/// access has its own strength which is determined by the type of the object it
/// is accessing. As an example, attempting to access a member of a type defined
/// in the current file may use [`AccessLevel::File`], while an access in the
/// same line of a type defined somewhere else may use [`AccessLevel::Public`].
///
/// This is determined by the [`AccessHandler`] class, and is explained in more
/// detail there. In addition, which levels can access which is explained in
/// [`Self::can_access`].
///
/// There are 5 access levels:
/// - [`public`](Self::Public)
/// - [`private`](Self::Private)
/// - [`protected`](Self::Protected)
/// - [`pubget`](Self::Pubget)
/// - [`file`](Self::File)
///
/// Each of these correspond with a [descriptor](DescriptorNode), with the
/// exception of [`file`](Self::File), which is the default access level used in
/// the absence of any descriptors. These can be created from a set of
/// descriptors by the [`from_descriptors`](Self::from_descriptors) method.
// TODO? Add separate enum for clearance (in particular, a ProtectedAndFile
// clearance is necessary to fully express the levels of clearance)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AccessLevel {
    /// The `public` access level.
    ///
    /// An item annotated with `public` has no restrictions on the contexts from
    /// which it can be accessed. It is the weakest access level, as it can be
    /// accessed from any place in the code. It is also the weakest level of
    /// clearance; attempting to access with `public` clearance is only
    /// successful on items with a `public` access level.
    Public,

    /// The `private` access level.
    ///
    /// An item annotated with `private` can only be accessed by other methods
    /// belonging to the same type as it. It is both the strongest access level
    /// as well as the strongest clearance. Any access with a `private`
    /// clearance is guaranteed to succeed, at least in terms of privacy.
    Private,

    /// The `protected` access level.
    ///
    /// An item annotated with `protected` can be accessed by other methods
    /// belonging to the same type as it, as well as any subclasses of that
    /// type. `protected` clearance can access any variable with access level
    /// `protected` or `private`.
    Protected,

    /// The `pubget` access level.
    ///
    /// An item annotated with `pubget` is equivalent to `public` for immutable
    /// accesses, but `private` for mutable accesses. This access level is not
    /// currently being used as a clearance, and therefore has no defined
    /// semantics in that context.
    Pubget,

    /// The `file` access level.
    ///
    /// An item annotated with `file` is accessible by any code within the file
    /// that this is defined in. An access with `file` clearance can access any
    /// variable with access level `file` or `private`.
    File,
}

impl AccessHandler {
    /// Creates a new, empty [`AccessHandler`].
    ///
    /// # Examples
    /// ```
    /// let handler = AccessHandler::new();
    /// ```
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

    /// The current type that is referred to by the `cls` variable.
    ///
    /// This returns a reference to the last type added via [`Self::add_cls`],
    /// or [`None`] if no classes have been added (excluding all types removed
    /// with [`Self::remove_cls`]).
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// assert_eq!(handler.get_cls(), None);
    /// handler.add_cls(OBJECT.into());
    /// assert_eq!(handler.get_cls(), Some(&OBJECT.into()));
    /// // Always returns the last class added
    /// handler.add_cls(NULL_TYPE.into());
    /// assert_eq!(handler.get_cls(), Some(&NULL_TYPE.into()));
    /// ```
    pub fn get_cls(&self) -> Option<&TypeObject> {
        self.cls_types.last()
    }

    /// The current type that is referred to by the `super` variable.
    ///
    /// This returns a reference to the last type added via [`Self::add_super`],
    /// or [`None`] if no classes have been added (excluding all types removed
    /// with [`Self::remove_super`]).
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// assert_eq!(handler.get_super(), None);
    /// handler.add_super(OBJECT.into());
    /// assert_eq!(handler.get_super(), Some(&OBJECT.into()));
    /// // Always returns the last class added
    /// handler.add_super(NULL_TYPE.into());
    /// assert_eq!(handler.get_super(), Some(&NULL_TYPE.into()));
    /// ```
    pub fn get_super(&self) -> Option<&TypeObject> {
        self.super_types.last()
    }

    /// The current access level of the given [`TypeObject`].
    ///
    /// # Access levels
    ///
    /// The [access level](AccessLevel) returned is the highest-security level
    /// that can be accessed by code in the current scope. All security levels
    /// less strict than the current one may be used. Others should throw an
    /// exception at some point in compilation. Which levels can access which
    /// are explained in [`AccessLevel::can_access`].
    ///
    /// # Alternatives
    ///
    /// If the caller has only a [`UserType`], [`Self::user_access_level`] is an
    /// equivalent alternative which removes a clone.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// // The default access level is `AccessLevel::Public`
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// // Stricter levels of access can be given by
    /// // allow_{private, protected}_access
    /// handler.allow_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Protected);
    /// // This always returns the strictest access level allowed
    /// handler.allow_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Private);
    /// ```
    pub fn access_level(&self, obj: &TypeObject) -> AccessLevel {
        // FIXME: Do this without cloning
        let base = if let TypeObject::Type(ty) = obj {
            BaseType::new(ty.represented_type().clone())
        } else {
            BaseType::new(obj.clone())
        };
        self.access_lvl_inner(&base)
    }

    /// The current [access level](AccessLevel) of the given [`UserType`].
    ///
    /// # Alternatives
    ///
    /// To get access levels for non-user-types, use [`Self::access_level`].
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// // The default access level is `AccessLevel::Public`
    /// assert_eq!(handler.access_level(&*NULL_TYPE), AccessLevel::Public);
    /// // Stricter levels of access can be given by
    /// // allow_{private, protected}_access
    /// handler.allow_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Protected);
    /// // This always returns the strictest access level allowed
    /// handler.allow_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Private);
    /// ```
    pub fn user_access_level(&self, obj: &UserType) -> AccessLevel {
        // FIXME: Do this without cloning
        self.access_lvl_inner(&BaseType::new(obj.clone().into()))
    }

    fn access_lvl_inner(&self, base: &BaseType) -> AccessLevel {
        if self.defined_in_file.contains(base) {
            if self.classes_with_access.contains_key(base) {
                AccessLevel::Private
            } else {
                AccessLevel::File
            }
        } else if self.classes_with_access.contains_key(base) {
            AccessLevel::Private
        } else if self.classes_with_protected.contains_key(base) {
            AccessLevel::Protected
        } else {
            AccessLevel::Public
        }
    }

    /// Allows private access to the [`TypeObject`] given.
    ///
    /// Private access is done via a counter system, where each call to
    /// [`Self::allow_private_access`] increments the counter, and each call to
    /// [`Self::remove_private_access`] decrements it. Note that that means the
    /// latter function may not actually remove private access from the given
    /// type, as the counter may remain above 0.
    ///
    /// See also [`Self::remove_private_access`].
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.allow_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Private);
    /// // The opposite of this method is `remove_private_access`
    /// handler.remove_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// ```
    pub fn allow_private_access(&mut self, obj: TypeObject) {
        increment(BaseType::new(obj), &mut self.classes_with_access)
    }

    /// Removes private access from the [`TypeObject`] given.
    ///
    /// # Caveats
    ///
    /// This does not guarantee that the value returned by
    /// [`Self::access_level`] will be stricter than `private`, as it is
    /// possible that private access was given in another place. Private access
    /// will last until this method has been called as many times as
    /// [`Self::allow_private_access`], in order to allow correct access.
    ///
    /// See also [`Self::allow_private_access`].
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.allow_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Private);
    /// // This method is the opposite of `add_private_access`
    /// handler.remove_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// ```
    ///
    /// Access levels can be stacked, as thus:
    ///
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.allow_private_access(OBJECT);
    /// handler.allow_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Private);
    /// handler.remove_private_access(OBJECT);
    /// // Note that the access level is still `private` here
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Private);
    /// handler.remove_private_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// ```
    pub fn remove_private_access(&mut self, obj: TypeObject) {
        decrement(BaseType::new(obj), &mut self.classes_with_access)
    }

    /// Allows protected access to the [`TypeObject`] given.
    ///
    /// See also [`Self::remove_protected_access`].
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.allow_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Protected);
    /// // The opposite of this method is `remove_protected_access`
    /// handler.remove_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// ```
    pub fn allow_protected_access(&mut self, obj: TypeObject) {
        increment(BaseType::new(obj), &mut self.classes_with_protected)
    }

    /// Removes protected access from the [`TypeObject`] given.
    ///
    /// # Caveats
    ///
    /// This does not guarantee that the value returned by
    /// [`Self::access_level`] will be stricter than `protected`, as it is
    /// possible that private access was given in another place. In addition, it
    /// is possible that `private` access was granted by a call to
    /// [`Self::allow_private_access`], which overrides any result of this
    /// method or its counterpart. Protected access will last until this method
    /// has been called as many times as [`Self::allow_private_access`], in
    /// order to allow correct access.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.allow_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Protected);
    /// // This method is the opposite of `add_protected_access`
    /// handler.remove_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// ```
    ///
    /// Access levels can be stacked, as thus:
    ///
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.allow_protected_access(OBJECT);
    /// handler.allow_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Protected);
    /// handler.remove_protected_access(OBJECT);
    /// // Note that the access level is still `protected` here
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Protected);
    /// handler.remove_protected_access(OBJECT);
    /// assert_eq!(handler.access_level(&OBJECT), AccessLevel::Public);
    /// ```
    pub fn remove_protected_access(&mut self, obj: TypeObject) {
        decrement(BaseType::new(obj), &mut self.classes_with_protected)
    }

    /// Adds a new [type](TypeObject) to be represented by the type variable
    /// `cls` in method definitions.
    ///
    /// In order to prevent leakage of types, this should always be used in
    /// conjunction with [`Self::remove_cls`].
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.add_cls(OBJECT);
    /// assert_eq!(handler.get_cls(), Some(&OBJECT))
    /// ```
    pub fn add_cls(&mut self, obj: TypeObject) {
        self.cls_types.push(obj)
    }

    /// Removes a type from being represented by `cls` in method definitions.
    ///
    /// This method is the reverse of [`Self::add_cls`], and should always be
    /// used in conjunction with it.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.add_cls(OBJECT);
    /// assert_eq!(handler.get_cls(), Some(&OBJECT));
    /// handler.pop_cls();
    /// assert_eq!(handler.get_cls(), None);
    /// ```
    pub fn remove_cls(&mut self) {
        self.cls_types.pop();
    }

    /// Adds a new [type](TypeObject) to be represented by the type variable
    /// `super` in method definitions.
    ///
    /// In order to prevent leakage of types, this should always be used in
    /// conjunction with [`Self::remove_super`].
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.add_super(OBJECT);
    /// assert_eq!(handler.get_super(), Some(&OBJECT))
    /// ```
    pub fn add_super(&mut self, obj: TypeObject) {
        self.super_types.push(obj);
    }

    /// Removes a type from being represented by `super` in method definitions.
    ///
    /// This method is the reverse of [`Self::remove_super`], and should always
    /// be used in conjunction with it.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.add_super(OBJECT);
    /// assert_eq!(handler.get_super(), Some(&OBJECT));
    /// handler.pop_super();
    /// assert_eq!(handler.get_super(), None);
    /// ```
    pub fn remove_super(&mut self) {
        self.super_types.pop();
    }

    /// Adds a new [type](TypeObject) to be treated as if it were in that type's
    /// constructor.
    ///
    /// Being in the constructor means that `self.foo = bar()`-style statements
    /// will always work, regardless of mutability of `self.foo` under normal
    /// circumstances.
    ///
    /// This should always be used in conjunction with
    /// [`Self::exit_constructor`], in order to prevent leakage of types.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.enter_constructor(OBJECT);
    /// assert!(handler.is_in_constructor(&OBJECT));
    /// ```
    pub fn enter_constructor(&mut self, ty: TypeObject) {
        self.constructors.push(ty);
    }

    /// Removes a [type](TypeObject) from being treated as if it were in that
    /// type's constructor.
    ///
    /// This undoes the effects of [`Self::enter_constructor`], and should
    /// always be called in conjunction with that.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.enter_constructor(OBJECT);
    /// assert!(handler.is_in_constructor(&OBJECT));
    /// handler.exit_constructor();
    /// assert!(!handler.is_in_constructor(&OBJECT));
    /// ```
    pub fn exit_constructor(&mut self) {
        self.constructors.pop();
    }

    /// Returns whether or not the current code is within the given type's
    /// constructor.
    ///
    /// # Examples
    /// ```
    /// let mut handler = AccessHandler::new();
    /// handler.enter_constructor(OBJECT);
    /// assert!(handler.is_in_constructor(&OBJECT));
    /// handler.enter_constructor(NULL_TYPE);
    /// // Note that multiple calls of `enter_constructor` do not override each
    /// // other
    /// assert!(handler.is_in_constructor(&OBJECT));
    /// assert!(handler.is_in_constructor(&NULL_TYPE));
    /// ```
    pub fn is_in_constructor(&self, ty: &TypeObject) -> bool {
        self.constructors.iter().any(|x| x.same_base_type(ty))
    }

    /// Sets the types defined in the current file to the given set of types.
    ///
    /// This method may only be called once, otherwise it will panic.
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
    /// Converts a set of [`DescriptorNode`]s to an [`AccessLevel`].
    ///
    /// This assumes that there is no more than one access-level descriptor
    /// (e.g. a descriptor which corresponds to an `AccessLevel`) in the set.
    /// Behavior is unspecified if there are multiple. If no access-level
    /// descriptors are present, this returns [`AccessLevel::File`].
    ///
    /// # Examples
    /// ```
    /// assert_eq!(
    ///     AccessLevel::from_descriptors(&HashSet::new()),
    ///     AccessLevel::File
    /// );
    /// assert_eq!(
    ///     AccessLevel::from_descriptors(&hash_set!(
    ///         DescriptorNode::Public,
    ///         DescriptorNode::Static
    ///     )),
    ///     AccessLevel::Public
    /// );
    /// ```
    pub fn from_descriptors(descriptors: &HashSet<DescriptorNode>) -> AccessLevel {
        DESCRIPTOR_PAIRS
            .iter()
            .find(|x| descriptors.contains(&x.0))
            .map(|x| x.1)
            .unwrap_or(AccessLevel::File)
    }

    /// Returns whether or not the second parameter is strong enough to access a
    /// variable with accessibility given by the first parameter.
    ///
    /// In essence, this returns whether `value_level` is weaker or stronger
    /// than `access_level`. This does not take mutability into account; it
    /// assumes the given access is immutable.
    ///
    /// # Accessibility table
    ///
    /// Access levels are along the top, while clearances are on the side.
    ///
    /// | Can access? | `public` | `pubget` | `file` | `protected` | `private` |
    /// |-------------|----------|----------|--------|-------------|-----------|
    /// | `public`    | yes      | immut.   | no     | no          | no        |
    /// | `file`      | yes      | immut.   | yes    | no          | no        |
    /// | `protected` | yes      | immut.   | no     | yes         | no        |
    /// | `private`   | yes      | yes      | yes    | yes         | yes       |
    ///
    /// # Examples
    /// ```
    /// assert!(AccessLevel::can_access(AccessLevel::Public, AccessLevel::Public));
    /// assert!(!AccessLevel::can_access(AccessLevel::Private, AccessLevel::File));
    /// assert!(AccessLevel::can_access(AccessLevel::Public, AccessLevel::Private));
    /// assert!(AccessLevel::can_access(AccessLevel::Private, AccessLevel::Private));
    /// assert!(!AccessLevel::can_access(AccessLevel::Private, AccessLevel::Public));
    /// ```
    pub fn can_access(value_level: AccessLevel, access_level: AccessLevel) -> bool {
        match value_level {
            // FIXME: Mutability w.r.t pubget
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

#[inline]
fn increment<T: Hash + Eq>(value: T, map: &mut HashMap<T, usize>) {
    match map.entry(value) {
        Entry::Occupied(mut e) => *e.get_mut() += 1,
        Entry::Vacant(e) => {
            e.insert(1);
        }
    };
}

#[inline]
fn decrement<T: Hash + Eq>(value: T, map: &mut HashMap<T, usize>) {
    match map.entry(value) {
        Entry::Occupied(mut e) => {
            *e.get_mut() -= 1;
            if *e.get_mut() == 0 {
                e.remove();
            }
        }
        Entry::Vacant(_) => panic!(),
    }
}

impl Default for AccessHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod handler_tests {
    use crate::converter::access_handler::AccessLevel;
    use crate::converter::builtins::{NULL_TYPE, OBJECT};
    use crate::converter::type_obj::BaseType;
    use crate::macros::hash_set;

    use super::AccessHandler;

    #[test]
    fn default_cls_super() {
        let handler = AccessHandler::new();
        assert_eq!(handler.get_cls(), None);
        assert_eq!(handler.get_super(), None);
    }

    #[test]
    fn default_access_level() {
        let handler = AccessHandler::new();
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
        assert_eq!(handler.access_level(&NULL_TYPE), AccessLevel::Public);
    }

    #[test]
    fn private_access() {
        let mut handler = AccessHandler::new();
        handler.allow_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.remove_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
    }

    #[test]
    fn stacked_private() {
        let mut handler = AccessHandler::new();
        handler.allow_private_access(OBJECT.into());
        handler.allow_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.remove_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.remove_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
    }

    #[test]
    fn protected_access() {
        let mut handler = AccessHandler::new();
        handler.allow_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Protected);
        handler.remove_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
    }

    #[test]
    fn stacked_protected() {
        let mut handler = AccessHandler::new();
        handler.allow_protected_access(OBJECT.into());
        handler.allow_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Protected);
        handler.remove_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Protected);
        handler.remove_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
    }

    #[test]
    fn layered_protected_private() {
        let mut handler = AccessHandler::new();
        handler.allow_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Protected);
        handler.allow_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.remove_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Protected);
        handler.allow_private_access(OBJECT.into());
        handler.remove_protected_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.remove_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
    }

    #[test]
    fn in_file() {
        let mut handler = AccessHandler::new();
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
        handler.set_defined_in_file(hash_set!(BaseType::new(OBJECT.into())));
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::File);
    }

    #[test]
    fn in_file_private() {
        let mut handler = AccessHandler::new();
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Public);
        handler.allow_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.set_defined_in_file(hash_set!(BaseType::new(OBJECT.into())));
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::Private);
        handler.remove_private_access(OBJECT.into());
        assert_eq!(handler.access_level(&OBJECT.into()), AccessLevel::File);
    }

    #[test]
    fn cls_type() {
        let mut handler = AccessHandler::new();
        assert_eq!(handler.get_cls(), None);
        handler.add_cls(OBJECT.into());
        assert_eq!(handler.get_cls(), Some(&OBJECT.into()));
        handler.remove_cls();
        assert_eq!(handler.get_cls(), None);
    }

    #[test]
    fn super_type() {
        let mut handler = AccessHandler::new();
        assert_eq!(handler.get_super(), None);
        handler.add_super(OBJECT.into());
        assert_eq!(handler.get_super(), Some(&OBJECT.into()));
        handler.remove_super();
        assert_eq!(handler.get_super(), None);
    }
}

#[cfg(test)]
mod level_tests {
    use std::collections::HashSet;

    use crate::{macros::hash_set, parser::descriptor::DescriptorNode};

    use super::AccessLevel;

    #[test]
    fn private_access() {
        assert!(AccessLevel::can_access(
            AccessLevel::Private,
            AccessLevel::Private
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Protected,
            AccessLevel::Private
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::File,
            AccessLevel::Private
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Pubget,
            AccessLevel::Private
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Public,
            AccessLevel::Private
        ));
    }

    #[test]
    fn protected_access() {
        assert!(!AccessLevel::can_access(
            AccessLevel::Private,
            AccessLevel::Protected
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Protected,
            AccessLevel::Protected
        ));
        assert!(!AccessLevel::can_access(
            AccessLevel::File,
            AccessLevel::Protected
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Pubget,
            AccessLevel::Protected
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Public,
            AccessLevel::Protected
        ));
    }

    #[test]
    fn file_access() {
        assert!(!AccessLevel::can_access(
            AccessLevel::Private,
            AccessLevel::File
        ));
        assert!(!AccessLevel::can_access(
            AccessLevel::Protected,
            AccessLevel::File
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::File,
            AccessLevel::File
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Pubget,
            AccessLevel::File
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Public,
            AccessLevel::File
        ));
    }

    #[test]
    fn public_access() {
        assert!(!AccessLevel::can_access(
            AccessLevel::Private,
            AccessLevel::Public
        ));
        assert!(!AccessLevel::can_access(
            AccessLevel::Protected,
            AccessLevel::Public
        ));
        assert!(!AccessLevel::can_access(
            AccessLevel::File,
            AccessLevel::Public
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Pubget,
            AccessLevel::Public
        ));
        assert!(AccessLevel::can_access(
            AccessLevel::Public,
            AccessLevel::Public
        ));
    }

    #[test]
    fn from_empty_descriptors() {
        assert_eq!(
            AccessLevel::from_descriptors(&HashSet::new()),
            AccessLevel::File
        );
    }

    #[test]
    fn from_valid_descriptors() {
        assert_eq!(
            AccessLevel::from_descriptors(&hash_set!(DescriptorNode::Public)),
            AccessLevel::Public
        );
        assert_eq!(
            AccessLevel::from_descriptors(&hash_set!(DescriptorNode::Const)),
            AccessLevel::File
        );
        assert_eq!(
            AccessLevel::from_descriptors(&hash_set!(
                DescriptorNode::Auto,
                DescriptorNode::Const,
                DescriptorNode::Private
            )),
            AccessLevel::Private
        );
    }
}
