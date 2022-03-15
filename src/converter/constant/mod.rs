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
pub use self::number::{BigintConstant, IntConstant, NumberConstant};
pub use self::option::{OptionConstant, OptionTypeConstant};
pub use self::range::{Range, RangeConstant};
pub use self::string::StringConstant;
pub use self::temp::TempConstant;
pub use self::tuple::TupleConstant;

use std::fmt::Display;
use std::hash::Hash;

use derive_new::new;
use num::Zero;

use super::builtins::Builtins;
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
    // TODO: NullConstant
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
            LangConstant::Option(_) => None,
            LangConstant::OptionType(_) => None,
            LangConstant::Range(_) => None,
            LangConstant::String(s) => Some(!s.value.is_empty()),
            LangConstant::Temp(t) => t.value.value.get().and_then(|x| x.bool_value()),
            LangConstant::Tuple(t) => Some(!t.value.is_empty()),
        }
    }

    pub fn str_value(&self) -> Option<String> {
        todo!()
    }

    pub fn repr_value(&self) -> Option<String> {
        todo!()
    }

    pub fn is_string(&self) -> bool {
        matches!(self, LangConstant::String(_))
    }

    pub fn get_type(&self) -> &TypeObject {
        todo!()
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
            LangConstant::Option(o) => o.fmt_name(builtins, f),
            LangConstant::OptionType(o) => Display::fmt(o, f),
            LangConstant::Range(r) => Display::fmt(r, f),
            LangConstant::String(s) => Display::fmt(s, f),
            LangConstant::Temp(t) => t.fmt_name(builtins, f),
            LangConstant::Tuple(t) => t.fmt_name(builtins, f),
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
