macro_rules! constant_getter {
    ($name:ident) => {
        pub fn $name(&self) -> &LangConstant {
            &self.$name
        }
    };

    ($name:ident, $field:ident) => {
        pub fn $name(&self) -> &LangConstant {
            &self.$field
        }
    };
}

macro_rules! type_getter {
    ($name:ident) => {
        pub fn $name(&self) -> &TypeObject {
            &self.$name
        }
    };

    ($name:ident, $field:ident) => {
        pub fn $name(&self) -> &TypeObject {
            &self.$field
        }
    };

    (global $name:ident) => {
        pub fn $name(&self) -> &TypeObject {
            &self.globals.$name
        }
    };

    (global $name:ident, $field:ident) => {
        pub fn $name(&self) -> &TypeObject {
            &self.globals.$field
        }
    };
}

macro_rules! parsed_ty_getter {
    ($name:ident) => {
        pub fn $name(&self) -> &TypeObject {
            self.all_builtins[stringify!($name)].as_type()
        }
    };

    ($name:ident, $field:ident) => {
        pub fn $name(&self) -> &TypeObject {
            self.all_builtins[stringify!($field)].as_type()
        }
    };
}

macro_rules! parsed_const_getter {
    ($name:ident) => {
        pub fn $name(&self) -> &LangConstant {
            panic!("Cannot get constants at the moment")
        }
    };

    ($name:ident, $field:ident) => {
        pub fn $name(&self) -> &LangConstant {
            panic!("Cannot get constants at the moment")
        }
    };
}

macro_rules! builtin_ty_fwd {
    ($name:ident) => {
        pub fn $name(self) -> &'a TypeObject {
            match self {
                Self::Standard(b) => b.$name(),
                Self::Parsed(b) => b.$name(),
            }
        }
    };
}

macro_rules! builtin_const_fwd {
    ($name:ident) => {
        pub fn $name(self) -> &'a LangConstant {
            match self {
                Self::Standard(b) => b.$name(),
                Self::Parsed(b) => b.$name(),
            }
        }
    };
}

pub(super) use {
    builtin_const_fwd, builtin_ty_fwd, constant_getter, parsed_const_getter, parsed_ty_getter,
    type_getter,
};
