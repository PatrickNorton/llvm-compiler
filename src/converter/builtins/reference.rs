use std::borrow::Cow;

use crate::converter::constant::LangConstant;
use crate::converter::lang_obj::LangObject;
use crate::converter::type_obj::TypeObject;
use crate::converter::CompileResult;

use super::macros::{builtin_const_fwd, builtin_ty_fwd};
use super::{Builtins, ParsedBuiltins};

#[derive(Debug, Copy, Clone)]
pub enum BuiltinRef<'a> {
    Standard(&'a Builtins),
    Parsed(&'a ParsedBuiltins),
}

impl<'a> BuiltinRef<'a> {
    pub fn de_iterable(self, val: &TypeObject) -> CompileResult<Vec<TypeObject>> {
        match self {
            BuiltinRef::Standard(s) => s.de_iterable(val),
            BuiltinRef::Parsed(p) => p.de_iterable(val),
        }
    }

    pub fn has_type(self, name: &str) -> bool {
        match self {
            BuiltinRef::Standard(s) => s.has_type(name),
            BuiltinRef::Parsed(_) => todo!(),
        }
    }

    pub fn constant_of(self, name: &str) -> Option<Cow<'a, LangConstant>> {
        match self {
            BuiltinRef::Standard(s) => s.constant_of(name),
            BuiltinRef::Parsed(p) => p.constant_of(name),
        }
    }

    pub fn has_name(self, name: &str) -> bool {
        match self {
            BuiltinRef::Standard(s) => s.has_name(name),
            BuiltinRef::Parsed(_) => todo!(),
        }
    }

    pub fn get_name(self, name: &str) -> Option<&'a LangObject> {
        match self {
            BuiltinRef::Standard(s) => s.get_name(name),
            BuiltinRef::Parsed(_) => todo!(),
        }
    }

    pub fn constant_no(&self, index: u16) -> Option<&'a LangObject> {
        match self {
            BuiltinRef::Standard(s) => s.constant_no(index),
            BuiltinRef::Parsed(_) => todo!(),
        }
    }

    pub fn builtin_names(self) -> Box<dyn Iterator<Item = &'a str> + 'a> {
        match self {
            BuiltinRef::Standard(s) => Box::new(s.builtin_names()),
            BuiltinRef::Parsed(p) => Box::new(p.builtin_names()),
        }
    }

    builtin_const_fwd!(bool_constant);
    builtin_const_fwd!(dec_constant);
    builtin_const_fwd!(char_constant);
    builtin_const_fwd!(iter_constant);
    builtin_const_fwd!(range_const);
    builtin_const_fwd!(arith_error_const);
    builtin_const_fwd!(assert_error_const);
    builtin_const_fwd!(null_error_const);
    builtin_const_fwd!(null_type_constant);
    builtin_const_fwd!(object_const);
    builtin_const_fwd!(test_const);
    builtin_const_fwd!(format_const);

    builtin_ty_fwd!(int_type);
    builtin_ty_fwd!(dec_type);
    builtin_ty_fwd!(bool_type);
    builtin_ty_fwd!(str_type);
    builtin_ty_fwd!(char_type);
    builtin_ty_fwd!(list_type);
    builtin_ty_fwd!(set_type);
    builtin_ty_fwd!(dict_type);
    builtin_ty_fwd!(tuple_type);
    builtin_ty_fwd!(range_type);
    builtin_ty_fwd!(null_type);
    builtin_ty_fwd!(slice_type);
    builtin_ty_fwd!(bytes_type);
    builtin_ty_fwd!(throwable);
    builtin_ty_fwd!(iterable);
    builtin_ty_fwd!(type_type);
    builtin_ty_fwd!(callable);
    builtin_ty_fwd!(iterator);
    builtin_ty_fwd!(throws_type);
}
