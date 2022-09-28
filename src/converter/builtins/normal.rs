use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::Debug;

use itertools::Itertools;
use num::ToPrimitive;

use crate::converter::access_handler::AccessLevel;
use crate::converter::builtins::reference::BuiltinRef;
use crate::converter::constant::{BuiltinConstant, LangConstant};
use crate::converter::lang_obj::LangObject;
use crate::converter::type_obj::TypeObject;
use crate::converter::CompileResult;
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::macros::{constant_getter, type_getter};
use super::{GlobalBuiltins, ParsedBuiltins};

/// The main struct that holds builtin information.
///
/// This contains all types, constants, and functions obtained from parsing
/// `__builtins__.newlang`, and makes them accessible from methods.
pub struct Builtins {
    true_builtins: Vec<LangObject>,
    all_builtins: HashMap<String, LangObject>,
    hidden_builtins: HashMap<String, LangObject>,

    iter_constant: LangConstant,
    range_constant: LangConstant,
    bool_constant: LangConstant,
    null_type_constant: LangConstant,
    null_error_constant: LangConstant,
    assertion_constant: LangConstant,
    arith_constant: LangConstant,
    char_constant: LangConstant,
    format_constant: LangConstant,
    decimal_constant: LangConstant,
    test_constant: LangConstant,
    object_constant: LangConstant,

    range_type: TypeObject,
    str_type: TypeObject,
    int_type: TypeObject,
    list_type: TypeObject,
    set_type: TypeObject,
    dict_type: TypeObject,
    slice_type: TypeObject,
    bytes_type: TypeObject,
    char_type: TypeObject,
    dec_type: TypeObject,
    bool_type: TypeObject,
    throwable: TypeObject,
    hashable: TypeObject,

    globals: GlobalBuiltins,
}

impl Builtins {
    pub fn has_name(&self, name: &str) -> bool {
        self.all_builtins.contains_key(name)
    }

    pub fn get_name(&self, name: &str) -> Option<&LangObject> {
        self.all_builtins.get(name)
    }

    pub fn builtin_names(&self) -> impl Iterator<Item = &str> {
        self.all_builtins.keys().map(|x| &**x)
    }

    pub fn builtin_map(&self) -> &HashMap<String, LangObject> {
        &self.all_builtins
    }

    pub fn type_constant(&self, name: &str) -> Option<LangConstant> {
        let builtin = self
            .all_builtins
            .get(name)
            .filter(|x| matches!(x, LangObject::Type(_)))?;
        self.true_builtins
            .iter()
            .position(|x| x == builtin)
            .map(|index| BuiltinConstant::new(index.try_into().unwrap()).into())
    }

    pub fn constant_of(&self, name: &str) -> Option<Cow<'_, LangConstant>> {
        let builtin = self.all_builtins.get(name)?;
        let index = self.true_builtins.iter().position(|x| x == builtin);
        if let Option::Some(index) = index {
            Some(Cow::Owned(
                BuiltinConstant::new(index.try_into().unwrap()).into(),
            ))
        } else if let LangObject::Constant(constant) = builtin {
            Some(Cow::Borrowed(constant))
        } else {
            None
        }
    }

    constant_getter!(bool_constant);
    constant_getter!(dec_constant, decimal_constant);
    constant_getter!(char_constant);
    constant_getter!(iter_constant);
    constant_getter!(range_const, range_constant);
    constant_getter!(arith_error_const, arith_constant);
    constant_getter!(assert_error_const, assertion_constant);
    constant_getter!(null_error_const, null_error_constant);
    constant_getter!(null_type_constant);
    constant_getter!(object_const, object_constant);
    constant_getter!(test_const, test_constant);
    constant_getter!(format_const, format_constant);

    type_getter!(int_type);
    type_getter!(dec_type);
    type_getter!(bool_type);
    type_getter!(str_type);
    type_getter!(char_type);
    type_getter!(list_type);
    type_getter!(set_type);
    type_getter!(dict_type);
    type_getter!(global tuple_type);
    type_getter!(range_type);
    type_getter!(global null_type);
    type_getter!(slice_type);
    type_getter!(bytes_type);
    type_getter!(throwable);
    type_getter!(global iterable);
    type_getter!(global type_type);
    type_getter!(global callable);
    type_getter!(global iterator);
    type_getter!(global throws_type, throws);
    type_getter!(hashable);

    pub fn de_iterable(&self, val: &TypeObject) -> CompileResult<Vec<TypeObject>> {
        if val.same_base_type(self.iterable()) {
            let generics = &val.get_generics()[0];
            match generics {
                TypeObject::List(l) => Ok(l.get_values().to_vec()),
                _ => panic!(),
            }
        } else {
            assert!(
                self.iterable().is_superclass(val),
                "{} is not an iterable",
                val.name()
            );
            let rets = val.try_op_ret_access(
                LineInfo::empty(),
                OpSpTypeNode::Iter,
                AccessLevel::Public,
                BuiltinRef::Standard(self),
            )?;
            self.de_iterable(&rets[0])
        }
    }

    pub fn builtin_name(&self, index: u16) -> Option<&str> {
        let result = self.true_builtins.get(index as usize)?;
        if result == &self.globals.null_type {
            return Some("type(null)");
        }
        for (name, value) in &self.all_builtins {
            if value == result {
                return Some(name);
            }
        }
        for (name, value) in &self.hidden_builtins {
            if value == result {
                return Some(name);
            }
        }
        None
    }

    pub fn constant_no(&self, index: u16) -> Option<&LangObject> {
        self.true_builtins.get(index.to_usize()?)
    }
}

#[inline]
fn get_type_obj(name: &str, values: &HashMap<String, LangObject>) -> TypeObject {
    values[name].as_type().clone()
}

#[inline]
fn builtin_const(
    name: &str,
    values: &HashMap<String, LangObject>,
    true_builtins: &[LangObject],
) -> LangConstant {
    builtin_const_of(&values[name], true_builtins)
}

#[inline]
fn builtin_const_of<T>(ty: &T, true_builtins: &[LangObject]) -> LangConstant
where
    LangObject: PartialEq<T>,
{
    BuiltinConstant::new(
        true_builtins
            .iter()
            .position(|x| x == ty)
            .unwrap()
            .try_into()
            .unwrap(),
    )
    .into()
}

impl Debug for Builtins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Builtins {{ ... }}")
    }
}

impl From<ParsedBuiltins> for Builtins {
    fn from(value: ParsedBuiltins) -> Self {
        let true_builtins = value
            .true_builtins
            .into_iter()
            .map(|x| x.expect("All true builtins should be set by now"))
            .collect_vec();
        Self {
            range_type: get_type_obj("range", &value.all_builtins),
            str_type: get_type_obj("str", &value.all_builtins),
            int_type: get_type_obj("int", &value.all_builtins),
            list_type: get_type_obj("list", &value.all_builtins),
            set_type: get_type_obj("set", &value.all_builtins),
            dict_type: get_type_obj("dict", &value.all_builtins),
            slice_type: get_type_obj("slice", &value.all_builtins),
            bytes_type: get_type_obj("bytes", &value.all_builtins),
            char_type: get_type_obj("char", &value.all_builtins),
            dec_type: get_type_obj("dec", &value.all_builtins),
            bool_type: get_type_obj("bool", &value.all_builtins),
            throwable: get_type_obj("Throwable", &value.all_builtins),
            hashable: get_type_obj("Hashable", &value.all_builtins),

            iter_constant: builtin_const("iter", &value.all_builtins, &true_builtins),
            range_constant: builtin_const("range", &value.all_builtins, &true_builtins),
            bool_constant: builtin_const("bool", &value.all_builtins, &true_builtins),
            null_type_constant: builtin_const_of(&value.globals.null_type, &true_builtins),
            null_error_constant: builtin_const("NullError", &value.all_builtins, &true_builtins),
            assertion_constant: builtin_const(
                "AssertionError",
                &value.all_builtins,
                &true_builtins,
            ),
            arith_constant: builtin_const("ArithmeticError", &value.all_builtins, &true_builtins),
            char_constant: builtin_const("char", &value.all_builtins, &true_builtins),
            format_constant: builtin_const_of(
                &value.hidden_builtins["__format_internal"],
                &true_builtins,
            ),
            decimal_constant: builtin_const("dec", &value.all_builtins, &true_builtins),
            test_constant: builtin_const_of(
                &value.hidden_builtins["__test_internal"],
                &true_builtins,
            ),
            object_constant: builtin_const("object", &value.all_builtins, &true_builtins),

            globals: value.globals,

            true_builtins,
            all_builtins: value.all_builtins,
            hidden_builtins: value.hidden_builtins,
        }
    }
}

#[cfg(test)]
impl Builtins {
    // FIXME: This is *hideous*; maybe replace Builtins w/enum when cfg(test)?
    #[cfg(test)]
    pub fn test_builtins() -> Builtins {
        use super::parsed::{initial_all_builtins, initial_true_builtins};

        Builtins {
            true_builtins: initial_true_builtins()
                .into_iter()
                .map(|x| x.unwrap_or_else(test_lang_object))
                .collect(),
            all_builtins: initial_all_builtins(),
            hidden_builtins: HashMap::new(),

            iter_constant: simple_test_constant(),
            range_constant: simple_test_constant(),
            bool_constant: simple_test_constant(),
            null_type_constant: simple_test_constant(),
            null_error_constant: simple_test_constant(),
            assertion_constant: simple_test_constant(),
            arith_constant: simple_test_constant(),
            char_constant: simple_test_constant(),
            format_constant: simple_test_constant(),
            decimal_constant: simple_test_constant(),
            test_constant: simple_test_constant(),
            object_constant: simple_test_constant(),

            range_type: simple_test_type(),
            str_type: simple_test_type(),
            int_type: simple_test_type(),
            list_type: simple_test_type(),
            set_type: simple_test_type(),
            dict_type: simple_test_type(),
            slice_type: simple_test_type(),
            bytes_type: simple_test_type(),
            char_type: simple_test_type(),
            dec_type: simple_test_type(),
            bool_type: simple_test_type(),
            throwable: simple_test_type(),
            hashable: simple_test_type(),

            globals: GlobalBuiltins::new(),
        }
    }
}

#[cfg(test)]
fn simple_test_constant() -> LangConstant {
    BuiltinConstant::new(u16::MAX).into()
}

#[cfg(test)]
fn simple_test_type() -> TypeObject {
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{StdTypeObject, UserTypeLike};

    let ty = StdTypeObject::new(
        "TEST_SHOULD_NOT_SEE".into(),
        Some(Vec::new()),
        GenericInfo::empty(),
        true,
        LineInfo::empty(),
    );
    ty.seal(None, None);
    ty.into()
}

#[cfg(test)]
fn test_lang_object() -> LangObject {
    LangObject::Constant(simple_test_constant())
}
