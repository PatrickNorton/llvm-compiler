use std::collections::HashSet;

use crate::parser::descriptor::DescriptorNode;

/// The type of mutability that a variable uses.
///
/// Mutability on a variable comes in two orthogonal forms: mutability on
/// the variable and mutability on the underlying data.
///
/// # Mutability table
///
/// |                               | Variable  | Data      |
/// |-------------------------------|-----------|-----------|
/// | [Standard](Self::Standard)    | Immutable | Immutable |
/// | [`mut`](Self::Mut)            | Mutable   | Mutable   |
/// | [`final`](Self::Final)        | Immutable | Mutable   |
/// | [`mref`](Self::Mref)          | Mutable   | Immutable |
/// | [Mut method](Self::MutMethod) | Immutable | Immutable |
///
/// # Variable mutability
///
/// Variables are mutable when annotated with the [`mut`](MutableType::Mut) or
/// [`mref`](MutableType::Mref) specifiers.
///
/// If a variable is mutable, it can be reassigned multiple times. As an
/// example, the following code is valid:
/// ```text
/// mref var x = foo()
/// x = bar()
/// ```
///
/// # Data mutability
///
/// Data is mutable when annotated with the [`mut`](MutableType::Mut) or
/// [`final`](MutableType::Mref) specifiers. Additionally, data mutability is
/// inherent on the *type*, not the variable. Types annotated with `mut` in a
/// context that is not a variable declaration are data-mutable. As such, these
/// are often referred to as `mut` types. It is legal to call a `mut` method if
/// and only if the type is data-mutable.
///
/// As an example, the following code is valid (note that `push` is a `mut`
/// method on `list[T]`):
/// ```text
/// final var x = [1, 2, 3]
/// x.push(4)
/// ```
///
/// # Method mutability
///
/// The odd mutability out is [`MutableType::MutMethod`]. This is neither
/// variable-mutable nor data-mutable (so it acts like [`MutableType::Standard`]
/// in that regard), but it represents the mutability type of a `mut method`
/// declaration. This means that calls to it require a `mut` type for the
/// method's parent type (i.e. the type referred to as `self` in the method).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MutableType {
    Standard,
    Mut,
    Final,
    Mref,
    MutMethod,
}

impl MutableType {
    /// Returns if the given mutable type is data-const.
    ///
    /// # Examples
    /// ```
    /// assert!(Self::Standard.is_const_type());
    /// assert!(Self::Mref.is_const_type());
    /// assert!(Self::MutMethod.is_const_type());
    /// assert!(!Self::Final.is_const_type());
    /// assert!(!Self::Mut.is_const_type());
    /// ```
    pub fn is_const_type(self) -> bool {
        matches!(self, Self::Mref | Self::Standard | Self::MutMethod)
    }

    /// Returns if the given mutable type is variable-const.
    ///
    /// # Examples
    /// ```
    /// assert!(Self::Standard.is_const_ref());
    /// assert!(Self::Final.is_const_ref());
    /// assert!(Self::MutMethod.is_const_ref());
    /// assert!(!Self::Mref.is_const_ref());
    /// assert!(!Self::Mut.is_const_ref());
    /// ```
    pub fn is_const_ref(self) -> bool {
        matches!(self, Self::Standard | Self::Final | Self::MutMethod)
    }

    /// Converts the given descriptor to a mutability type, returning [`None`]
    /// if the descriptor is not a mutability descriptor.
    ///
    /// # Examples
    /// ```
    /// assert_eq!(Self::from_descriptor(DescriptorNode::Mut), Some(Self::Mut));
    /// assert_eq!(Self::from_descriptor(DescriptorNode::Final), Some(Self::Final));
    /// assert_eq!(Self::from_descriptor(DescriptorNode::Mref), Some(Self::Mref));
    /// assert_eq!(Self::from_descriptor(DescriptorNode::Auto), None);
    /// assert_eq!(Self::from_descriptor(DescriptorNode::Static), None);
    /// ```
    pub fn from_descriptor(descriptor: DescriptorNode) -> Option<Self> {
        match descriptor {
            DescriptorNode::Mut => Some(Self::Mut),
            DescriptorNode::Final => Some(Self::Final),
            DescriptorNode::Mref => Some(Self::Mref),
            _ => None,
        }
    }

    /// Determines the mutability type from the set of descriptors on an object.
    ///
    /// This assumes that there is no more than one mutability descriptor in the
    /// set; the behavior is unspecified if that is not the case. If there are
    /// no mutability descriptors in the set, this returns [standard
    /// mutability](Self::Standard).
    ///
    /// # Examples
    /// ```
    /// assert_eq!(Self::from_descriptors(&HashSet::new()), Self::Standard);
    ///
    /// let descriptors = HashSet::from([
    ///     DescriptorNode::Public,
    ///     DescriptorNode::Static,
    ///     DescriptorNode::Final,
    /// ]);
    /// assert_eq!(Self::from_descriptors(&descriptors), Self::Final);
    /// ```
    pub fn from_descriptors(descriptors: &HashSet<DescriptorNode>) -> Self {
        descriptors
            .iter()
            .find_map(|&x| Self::from_descriptor(x))
            .unwrap_or(Self::Standard)
    }

    /// Determines the mutability type of the given optional descriptor.
    ///
    /// If `descriptor` is [`None`], this method returns [`Self::Standard`]. If
    /// the given descriptor is not a mutability descriptor, this method panics.
    ///
    /// # Examples
    /// ```
    /// assert_eq!(Self::from_nullable(Some(DescriptorNode::Mut)), Self::Mut);
    /// assert_eq!(Self::from_nullable(Some(DescriptorNode::Final)), Self::Final);
    /// assert_eq!(Self::from_nullable(Some(DescriptorNode::Mref)), Self::Mref);
    /// assert_eq!(Self::from_nullable(None), Self::Standard);
    /// ```
    pub fn from_nullable(descriptor: Option<DescriptorNode>) -> Self {
        descriptor.map_or_else(
            || MutableType::Standard,
            |x| {
                Self::from_descriptor(x).unwrap_or_else(|| {
                    panic!(
                        "Descriptor does not correspond to a mutability: {}",
                        x.name()
                    )
                })
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::macros::hash_set;
    use crate::parser::descriptor::DescriptorNode;

    use super::MutableType;

    const NON_MUTABLE_DESCRIPTORS: &[DescriptorNode] = &[
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Protected,
        DescriptorNode::Pubget,
        DescriptorNode::Static,
        DescriptorNode::Readonly,
        DescriptorNode::Nonfinal,
        DescriptorNode::Native,
        DescriptorNode::Generator,
        DescriptorNode::Synced,
        DescriptorNode::Auto,
        DescriptorNode::Const,
    ];

    const MUTABLE_DESCRIPTORS: &[(DescriptorNode, MutableType)] = &[
        (DescriptorNode::Mut, MutableType::Mut),
        (DescriptorNode::Mref, MutableType::Mref),
        (DescriptorNode::Final, MutableType::Final),
    ];

    #[test]
    fn const_mutability_type() {
        assert!(MutableType::Standard.is_const_type());
        assert!(MutableType::Mref.is_const_type());
        assert!(MutableType::MutMethod.is_const_type());
        assert!(!MutableType::Final.is_const_type());
        assert!(!MutableType::Mut.is_const_type());
    }

    #[test]
    fn const_mutability_ref() {
        assert!(MutableType::Standard.is_const_ref());
        assert!(MutableType::Final.is_const_ref());
        assert!(MutableType::MutMethod.is_const_ref());
        assert!(!MutableType::Mref.is_const_ref());
        assert!(!MutableType::Mut.is_const_ref());
    }

    #[test]
    fn from_descriptor_some() {
        assert_eq!(
            MutableType::from_descriptor(DescriptorNode::Mut),
            Some(MutableType::Mut)
        );
        assert_eq!(
            MutableType::from_descriptor(DescriptorNode::Final),
            Some(MutableType::Final)
        );
        assert_eq!(
            MutableType::from_descriptor(DescriptorNode::Mref),
            Some(MutableType::Mref)
        );
    }

    #[test]
    fn from_descriptor_none() {
        for &descriptor in NON_MUTABLE_DESCRIPTORS {
            assert_eq!(MutableType::from_descriptor(descriptor), None);
        }
    }

    #[test]
    fn from_empty_descriptors() {
        assert_eq!(
            MutableType::from_descriptors(&HashSet::new()),
            MutableType::Standard
        );
    }

    #[test]
    fn from_single_descriptors() {
        for &(descriptor, mut_type) in MUTABLE_DESCRIPTORS {
            assert_eq!(
                MutableType::from_descriptors(&hash_set!(descriptor)),
                mut_type
            );
        }
        for &descriptor in NON_MUTABLE_DESCRIPTORS {
            assert_eq!(
                MutableType::from_descriptors(&hash_set!(descriptor)),
                MutableType::Standard
            );
        }
    }

    #[test]
    fn from_multiple_descriptors() {
        let descriptors = hash_set!(DescriptorNode::Public, DescriptorNode::Final);
        assert_eq!(
            MutableType::from_descriptors(&descriptors),
            MutableType::Final
        );

        let descriptors = hash_set!(DescriptorNode::Private, DescriptorNode::Auto);
        assert_eq!(
            MutableType::from_descriptors(&descriptors),
            MutableType::Standard
        );

        let descriptors = hash_set!(
            DescriptorNode::Protected,
            DescriptorNode::Mut,
            DescriptorNode::Native
        );
        assert_eq!(
            MutableType::from_descriptors(&descriptors),
            MutableType::Mut
        );
    }
}
