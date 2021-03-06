mod boolean;
mod builtin;
mod bytes;
mod character;
mod class_const;
mod decimal;
mod format;
mod function;
mod import;
mod module;
mod null;
mod number;
mod option;
mod range;
mod string;
mod temp;
mod tuple;

pub use self::boolean::BoolConstant;
pub use self::builtin::BuiltinConstant;
pub use self::bytes::BytesConstant;
pub use self::character::CharConstant;
pub use self::class_const::ClassConstant;
pub use self::decimal::DecimalConstant;
pub use self::format::FormatConstant;
pub use self::function::FunctionConstant;
pub use self::import::ImportConstant;
pub use self::module::ModuleConstant;
pub use self::null::NullConstant;
pub use self::number::{BigintConstant, IntConstant, NumberConstant};
pub use self::option::{OptionConstant, OptionTypeConstant};
pub use self::range::{Range, RangeConstant};
pub use self::string::StringConstant;
pub use self::temp::TempConstant;
pub use self::tuple::TupleConstant;

use std::borrow::Cow;
use std::fmt::Display;
use std::hash::Hash;

use derive_new::new;
use num::Zero;

use super::builtins::{BuiltinRef, Builtins};
use super::file_writer::ConstantSet;
use super::type_obj::{TypeObject, UserType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LangConstant {
    Bigint(BigintConstant),
    Bool(BoolConstant),
    Builtin(BuiltinConstant),
    Bytes(BytesConstant),
    Char(CharConstant),
    Class(ClassConstant),
    Decimal(DecimalConstant),
    Fmt(FormatConstant),
    Func(FunctionConstant),
    Import(ImportConstant),
    Int(IntConstant),
    Module(ModuleConstant),
    Null(NullConstant),
    Option(OptionConstant),
    OptionType(OptionTypeConstant),
    Range(RangeConstant),
    String(StringConstant),
    Temp(TempConstant),
    Tuple(TupleConstant),
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(self) enum ConstantBytes {
    Null,
    Str,
    Int,
    Bigint,
    Decimal,
    Import,
    Builtin,
    Function,
    Bool,
    Class,
    Option,
    Bytes,
    Range,
    Tuple,
    OptionType,
    Char,
    Ascii,
    Format,
    Module,
}

impl LangConstant {
    pub fn bool_value(&self) -> Option<bool> {
        match self {
            LangConstant::Bigint(b) => Some(!b.value.is_zero()),
            LangConstant::Bool(b) => Some(b.bool_value()),
            LangConstant::Builtin(_) => None,
            LangConstant::Bytes(b) => Some(!b.value.is_empty()),
            LangConstant::Char(c) => Some(c.value != '\0'),
            LangConstant::Class(_) => None,
            LangConstant::Decimal(d) => Some(!d.value.is_zero()),
            LangConstant::Fmt(_) => None,
            LangConstant::Func(_) => None,
            LangConstant::Import(_) => None,
            LangConstant::Int(i) => Some(i.value != 0),
            LangConstant::Module(_) => None,
            LangConstant::Null(_) => Some(false),
            LangConstant::Option(_) => None,
            LangConstant::OptionType(_) => None,
            LangConstant::Range(_) => None,
            LangConstant::String(s) => Some(!s.value.is_empty()),
            LangConstant::Temp(t) => t.value.value.get().and_then(|x| x.bool_value()),
            LangConstant::Tuple(t) => Some(!t.value.is_empty()),
        }
    }

    pub fn str_value(&self) -> Option<String> {
        match self {
            LangConstant::Bigint(b) => Some(b.str_value()),
            LangConstant::Bool(b) => Some(b.str_value()),
            LangConstant::Builtin(_) => None, // TODO
            LangConstant::Bytes(_) => None,
            LangConstant::Char(c) => Some(c.str_value()),
            LangConstant::Class(c) => Some(c.str_value()),
            LangConstant::Decimal(_) => None, // TODO
            LangConstant::Fmt(_) => None,
            LangConstant::Func(_) => None,
            LangConstant::Import(_) => None,
            LangConstant::Int(i) => Some(i.str_value()),
            LangConstant::Module(_) => None,
            LangConstant::Null(n) => Some(n.str_value()),
            LangConstant::Option(_) => None,
            LangConstant::OptionType(o) => Some(o.str_value()),
            LangConstant::Range(r) => Some(r.str_value()),
            LangConstant::String(s) => Some(s.str_value()),
            LangConstant::Temp(t) => t.str_value(),
            LangConstant::Tuple(_) => None,
        }
    }

    pub fn repr_value(&self) -> Option<String> {
        match self {
            LangConstant::Bigint(b) => Some(b.repr_value()),
            LangConstant::Bool(b) => Some(b.repr_value()),
            LangConstant::Builtin(_) => None, // TODO
            LangConstant::Bytes(b) => Some(b.repr_value()),
            LangConstant::Char(c) => Some(c.repr_value()),
            LangConstant::Class(c) => Some(c.repr_value()),
            LangConstant::Decimal(_) => None, // TODO
            LangConstant::Fmt(_) => None,
            LangConstant::Func(_) => None,
            LangConstant::Import(_) => None,
            LangConstant::Int(i) => Some(i.repr_value()),
            LangConstant::Module(_) => None,
            LangConstant::Null(n) => Some(n.repr_value()),
            LangConstant::Option(_) => None,
            LangConstant::OptionType(o) => Some(o.repr_value()),
            LangConstant::Range(r) => Some(r.repr_value()),
            LangConstant::String(s) => Some(s.repr_value()),
            LangConstant::Temp(t) => t.repr_value(),
            LangConstant::Tuple(_) => None,
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, LangConstant::String(_))
    }

    pub fn get_type<'a>(&'a self, builtins: BuiltinRef<'a>) -> Cow<'a, TypeObject> {
        match self {
            LangConstant::Bigint(b) => Cow::Borrowed(b.get_type(builtins)),
            LangConstant::Bool(b) => Cow::Borrowed(b.get_type(builtins)),
            LangConstant::Builtin(b) => b.get_type(builtins),
            LangConstant::Bytes(b) => Cow::Borrowed(b.get_type(builtins)),
            LangConstant::Char(c) => Cow::Borrowed(c.get_type(builtins)),
            LangConstant::Class(c) => Cow::Owned(c.get_type()),
            LangConstant::Decimal(d) => Cow::Borrowed(d.get_type(builtins)),
            LangConstant::Fmt(f) => Cow::Borrowed(f.get_type()),
            LangConstant::Func(f) => Cow::Borrowed(f.get_type(builtins)),
            LangConstant::Import(i) => Cow::Borrowed(i.get_type()),
            LangConstant::Int(i) => Cow::Borrowed(i.get_type(builtins)),
            LangConstant::Module(m) => Cow::Borrowed(m.get_type()),
            LangConstant::Null(n) => Cow::Borrowed(n.get_type(builtins)),
            LangConstant::Option(o) => Cow::Owned(o.get_type(builtins)),
            LangConstant::OptionType(o) => Cow::Owned(o.get_type()),
            LangConstant::Range(r) => Cow::Borrowed(r.get_type(builtins)),
            LangConstant::String(s) => Cow::Borrowed(s.get_type(builtins)),
            LangConstant::Temp(t) => Cow::Borrowed(t.get_type()),
            LangConstant::Tuple(t) => Cow::Owned(t.get_type(builtins)),
        }
    }

    pub fn to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        match self {
            LangConstant::Bigint(b) => b.to_bytes(),
            LangConstant::Bool(b) => b.to_bytes(),
            LangConstant::Builtin(b) => b.to_bytes(),
            LangConstant::Bytes(b) => b.to_bytes(),
            LangConstant::Char(c) => c.to_bytes(),
            LangConstant::Class(c) => c.to_bytes(),
            LangConstant::Decimal(d) => d.to_bytes(),
            LangConstant::Fmt(f) => f.to_bytes(),
            LangConstant::Func(f) => f.to_bytes(),
            LangConstant::Import(i) => i.to_bytes(),
            LangConstant::Int(i) => i.to_bytes(),
            LangConstant::Module(m) => m.to_bytes(constants),
            LangConstant::Null(n) => n.to_bytes(),
            LangConstant::Option(o) => o.to_bytes(constants),
            LangConstant::OptionType(o) => o.to_bytes(constants),
            LangConstant::Range(r) => r.to_bytes(),
            LangConstant::String(s) => s.to_bytes(),
            LangConstant::Temp(t) => t.to_bytes(),
            LangConstant::Tuple(t) => t.to_bytes(constants),
        }
    }

    pub fn fmt_name(
        &self,
        builtins: &Builtins,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            LangConstant::Bigint(b) => Display::fmt(b, f),
            LangConstant::Bool(b) => Display::fmt(b, f),
            LangConstant::Builtin(b) => b.fmt_name(builtins, f),
            LangConstant::Bytes(b) => Display::fmt(b, f),
            LangConstant::Char(c) => Display::fmt(c, f),
            LangConstant::Class(c) => Display::fmt(c, f),
            LangConstant::Decimal(d) => Display::fmt(d, f),
            LangConstant::Fmt(c) => Display::fmt(c, f),
            LangConstant::Func(c) => Display::fmt(c, f),
            LangConstant::Import(i) => Display::fmt(i, f),
            LangConstant::Int(i) => Display::fmt(i, f),
            LangConstant::Module(m) => Display::fmt(m, f),
            LangConstant::Null(n) => Display::fmt(n, f),
            LangConstant::Option(o) => o.fmt_name(builtins, f),
            LangConstant::OptionType(o) => Display::fmt(o, f),
            LangConstant::Range(r) => Display::fmt(r, f),
            LangConstant::String(s) => Display::fmt(s, f),
            LangConstant::Temp(t) => t.fmt_name(builtins, f),
            LangConstant::Tuple(t) => t.fmt_name(builtins, f),
        }
    }

    pub fn constituent_values(&self) -> Vec<&LangConstant> {
        match self {
            LangConstant::Option(o) => o.constituent_values(),
            LangConstant::OptionType(o) => o.constituent_values(),
            LangConstant::Temp(t) => t.constituent_values(),
            LangConstant::Tuple(t) => t.constituent_values(),
            _ => vec![],
        }
    }

    pub fn display<'a>(&'a self, builtins: &'a Builtins) -> impl Display + 'a {
        ConstantNamer::new(self, builtins)
    }
}

#[derive(Debug, new)]
struct ConstantNamer<'a> {
    constant: &'a LangConstant,
    builtins: &'a Builtins,
}

impl<'a> Display for ConstantNamer<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.constant.fmt_name(self.builtins, f)
    }
}
