use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;

use crate::converter::access_handler::AccessLevel;
use crate::converter::constant::{BuiltinConstant, LangConstant, NullConstant};
use crate::converter::lang_obj::LangObject;
use crate::converter::type_obj::{TupleType, TypeObject, TypeTypeObject};
use crate::converter::CompileResult;
use crate::macros::hash_map;
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::globals::{CALLABLE, FALSE, ITERABLE, ITERATOR, NULL_TYPE, OBJECT, TRUE};
use super::macros::{parsed_const_getter, parsed_ty_getter, type_getter};
use super::{BuiltinRef, GlobalBuiltins};

/// The struct used to contain builtin info during the parsing of
/// `__builtins__.newlang`.
///
/// Unlike its companions, [`Builtins`] and [`GlobalBuiltins`], this struct is
/// meant to be accessed through an `&mut` reference so that the information
/// can be set.
pub struct ParsedBuiltins {
    pub(super) true_builtins: Vec<Option<LangObject>>,
    pub(super) all_builtins: HashMap<String, LangObject>,
    pub(super) hidden_builtins: HashMap<String, LangObject>,
    pub(super) globals: GlobalBuiltins,
}

impl ParsedBuiltins {
    pub fn new() -> Self {
        Self {
            // FIXME: Populate these maps
            true_builtins: initial_true_builtins(),
            all_builtins: initial_all_builtins(),
            hidden_builtins: HashMap::new(),
            globals: GlobalBuiltins::new(),
        }
    }

    pub fn globals(&self) -> &GlobalBuiltins {
        &self.globals
    }

    pub fn set_builtin(
        &mut self,
        name: String,
        index: Option<usize>,
        is_hidden: bool,
        value: LangObject,
    ) {
        if !is_hidden {
            match self.all_builtins.entry(name) {
                Entry::Vacant(e) => e.insert(value.clone()),
                Entry::Occupied(e) => panic!("Cannot set builtin {} twice", e.key()),
            };
        } else {
            match self.hidden_builtins.entry(name) {
                Entry::Vacant(e) => e.insert(value.clone()),
                Entry::Occupied(e) => panic!("Cannot set builtin {} twice", e.key()),
            };
        }
        if let Option::Some(index) = index {
            assert_eq!(self.true_builtins.get(index), Some(&None));
            self.true_builtins[index] = Some(value);
        }
    }

    pub fn get_name(&self, name: &str) -> Option<&TypeObject> {
        self.all_builtins.get(name).and_then(|x| match x {
            LangObject::Type(t) => Some(t),
            _ => None,
        })
    }

    pub fn builtin_names(&self) -> impl Iterator<Item = &'_ str> {
        self.all_builtins.keys().map(|x| x.as_str())
    }

    pub fn builtin_map(&self) -> &HashMap<String, LangObject> {
        &self.all_builtins
    }

    pub fn de_iterable(&self, val: &TypeObject) -> CompileResult<Vec<TypeObject>> {
        if val.same_base_type(self.iterable()) {
            let generics = &val.get_generics()[0];
            match generics {
                TypeObject::List(l) => Ok(l.get_values().to_vec()),
                _ => panic!(),
            }
        } else {
            assert!(self.iterable().is_superclass(val));
            let rets = val.try_op_ret_access(
                LineInfo::empty(),
                OpSpTypeNode::Iter,
                AccessLevel::Public,
                BuiltinRef::Parsed(self),
            )?;
            self.de_iterable(&rets[0])
        }
    }

    pub fn constant_of(&self, name: &str) -> Option<Cow<'_, LangConstant>> {
        let builtin = self.all_builtins.get(name)?;
        let index = self
            .true_builtins
            .iter()
            .position(|x| x.as_ref() == Some(builtin));
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

    parsed_const_getter!(bool_constant);
    parsed_const_getter!(dec_constant, decimal_constant);
    parsed_const_getter!(char_constant);
    parsed_const_getter!(iter_constant);
    parsed_const_getter!(range_const, range_constant);
    parsed_const_getter!(arith_error_const, arith_constant);
    parsed_const_getter!(assert_error_const, assertion_constant);
    parsed_const_getter!(null_error_const, null_error_constant);
    parsed_const_getter!(null_type_constant);
    parsed_const_getter!(object_const, object_constant);
    parsed_const_getter!(test_const, test_constant);
    parsed_const_getter!(format_const, format_constant);

    parsed_ty_getter!(int_type, int);
    parsed_ty_getter!(dec_type, dec);
    parsed_ty_getter!(bool_type, bool);
    parsed_ty_getter!(str_type, str);
    parsed_ty_getter!(char_type, char);
    parsed_ty_getter!(list_type, list);
    parsed_ty_getter!(set_type, set);
    parsed_ty_getter!(dict_type, dict);
    type_getter!(global tuple_type);
    parsed_ty_getter!(range_type, range);
    type_getter!(global null_type);
    parsed_ty_getter!(slice_type, slice);
    parsed_ty_getter!(bytes_type, bytes);
    parsed_ty_getter!(throwable, Throwable);
    type_getter!(global iterable);
    type_getter!(global type_type);
    type_getter!(global callable);
    type_getter!(global iterator);
    type_getter!(global throws_type, throws);
}

pub(super) fn initial_true_builtins() -> Vec<Option<LangObject>> {
    vec![
        None, // print
        Some((*CALLABLE).clone().into()),
        None, // int
        None, // str
        None, // bool
        None, // range
        Some(TypeTypeObject::new_empty().into()),
        None, // iter
        None, // repr
        None, // input
        None, // list
        None, // set
        None, // char
        None, // open
        None, // reversed
        None, // slice
        None, // id
        None, // Array
        None, // enumerate
        None, // bytes
        None, // dict
        Some(OBJECT.into()),
        None, // NotImplemented
        Some(TupleType::default().into()),
        None, // Throwable
        Some((*NULL_TYPE).clone().into()),
        None, // hash
        None, // ValueError
        None, // NullError
        Some((*ITERABLE).clone().into()),
        None, // AssertionError
        None, // __format_internal
        Some((*ITERATOR).clone().into()),
        None, // ArithmeticError
        None, // __test_internal
        None, // option
        None, // dec
    ]
}

pub(super) fn initial_all_builtins() -> HashMap<String, LangObject> {
    hash_map!(
        "type".into() => TypeTypeObject::new_empty().into(),
        "true".into() => LangConstant::from(TRUE).into(),
        "false".into() => LangConstant::from(FALSE).into(),
        "Callable".into() => (*CALLABLE).clone().into(),
        "Iterable".into() => (*ITERABLE).clone().into(),
        "object".into() => OBJECT.into(),
        "Iterator".into() => (*ITERATOR).clone().into(),
        "tuple".into() => TupleType::new(Vec::new()).into(),
        "null".into() => LangConstant::from(NullConstant::new()).into()
    )
}

impl Default for ParsedBuiltins {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for ParsedBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParsedBuiltins {{ ... }}")
    }
}
