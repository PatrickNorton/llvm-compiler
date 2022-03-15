macro_rules! arc_eq_hash {
    ($value:ty) => {
        impl PartialEq for $value {
            fn eq(&self, other: &Self) -> bool {
                std::sync::Arc::ptr_eq(&self.value, &other.value)
            }
        }

        impl Eq for $value {}

        impl std::hash::Hash for $value {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::ptr::hash(Arc::as_ptr(&self.value), state);
            }
        }
    };
}

macro_rules! arc_partial_eq {
    ($value:ty, $variant:ident) => {
        impl PartialEq<TypeObject> for $value {
            fn eq(&self, other: &TypeObject) -> bool {
                match other {
                    TypeObject::$variant(x) => self == x,
                    _ => false,
                }
            }
        }
    };
}

macro_rules! type_obj_from {
    ($value:ty, $variant:ident) => {
        impl From<$value> for TypeObject {
            fn from(x: $value) -> Self {
                Self::$variant(x)
            }
        }
    };
}

macro_rules! try_from_type_obj {
    ($value:ty, $variant:ident) => {
        impl TryFrom<TypeObject> for $value {
            type Error = TypeObject;

            fn try_from(value: TypeObject) -> Result<Self, Self::Error> {
                match value {
                    TypeObject::$variant(x) => Ok(x),
                    x => Err(x),
                }
            }
        }

        impl<'a> TryFrom<&'a TypeObject> for &'a $value {
            type Error = ();

            fn try_from(value: &'a TypeObject) -> Result<Self, Self::Error> {
                match value {
                    TypeObject::$variant(x) => Ok(x),
                    _ => Err(()),
                }
            }
        }
    };
}

macro_rules! user_type_from {
    ($value:ty, $variant:ident) => {
        impl From<$value> for UserType {
            fn from(x: $value) -> Self {
                Self::$variant(x)
            }
        }
    };
}

macro_rules! try_from_user_type {
    ($value:ty, $variant:ident) => {
        impl TryFrom<UserType> for $value {
            type Error = UserType;

            fn try_from(value: UserType) -> Result<Self, Self::Error> {
                match value {
                    UserType::$variant(x) => Ok(x),
                    x => Err(x),
                }
            }
        }
    };
}

pub(super) use arc_eq_hash;
pub(super) use arc_partial_eq;
pub(super) use try_from_type_obj;
pub(super) use try_from_user_type;
pub(super) use type_obj_from;
pub(super) use user_type_from;
