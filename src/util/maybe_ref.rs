use std::fmt::{Binary, Display, LowerExp, LowerHex, Octal, UpperExp, UpperHex};
use std::ops::Deref;

/// A possibly-owned value, similar to [`Cow`](std::borrow::Cow), but not
/// requiring `T` to be [`Borrow`](std::borrow::Borrow).
#[derive(Debug, Copy, Clone)]
pub enum MaybeRef<'a, T> {
    Owned(T),
    Ref(&'a T),
}

impl<'a, T> Deref for MaybeRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeRef::Owned(val) => val,
            MaybeRef::Ref(val) => val,
        }
    }
}

impl<'a, T> From<T> for MaybeRef<'a, T> {
    fn from(x: T) -> Self {
        Self::Owned(x)
    }
}

impl<'a, T> From<&'a T> for MaybeRef<'a, T> {
    fn from(x: &'a T) -> Self {
        Self::Ref(x)
    }
}

impl<'a, T: PartialEq> PartialEq for MaybeRef<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<'a, T: Eq> Eq for MaybeRef<'a, T> {}

macro_rules! impl_fmt {
    ($format:ident, $( $formats:ident ),+ $(,)?) => {
        impl_fmt!($format);
        $(
            impl_fmt!($formats);
        )+
    };

    ($format:ident) => {
        impl<'a, T> $format for MaybeRef<'a, T>
        where
            T: $format,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::Owned(x) => $format::fmt(x, f),
                    Self::Ref(x) => $format::fmt(x, f),
                }
            }
        }
    };
}

impl_fmt!(Display, Octal, Binary, UpperHex, LowerHex, UpperExp, LowerExp);
