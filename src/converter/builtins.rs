use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use itertools::Itertools;
use num::ToPrimitive;
use once_cell::sync::Lazy;

use crate::converter::class::AttributeInfo;
use crate::macros::{hash_map, hash_set};
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::access_handler::AccessLevel;
use super::argument::ArgumentInfo;
use super::class::MethodInfo;
use super::constant::{BoolConstant, BuiltinConstant, LangConstant, NullConstant};
use super::fn_info::FunctionInfo;
use super::generic::GenericInfo;
use super::lang_obj::LangObject;
use super::type_obj::{
    InterfaceType, ListTypeObject, ObjectType, StdTypeObject, TemplateParam, TupleType, TypeObject,
    TypeTypeObject, UserTypeLike,
};
use super::CompileResult;

// High-level overview of builtins and how they may differ from the Java version:
// * Here, Builtins is a class that is created for each GlobalCompilerInfo
// * Values not created in __builtins__.newlang are global constants (maybe
//    clone them into each instance of Builtins so there's no Lazy syncing?)
// * Values created in __builtins__.newlang are part of an InnerBuiltins class,
//    which is given to the ImportHandler for __builtins__ (with type
//    Option<Box<ParsedBuiltins>>), and then transferred to the GlobalCompilerInfo
//    after parsing is complete, with the type changed to InnerBuiltins.
// * The Builtins class, which is part of GlobalCompilerInfo, has a field of
//    type OnceCell<InnerBuiltins>, which is filled once parsing of __builtins__
//    is complete.
// It might make more sense to have this be an Arc somehow and cached locally in
// each CompilerInfo (so as to avoid the synchronization on the global OnceCell).
// As an example, have each Compiler have a field `builtins:
// Option<Arc<Builtins>>`, and then
// ```
// pub fn get_builtins(&mut self) -> &Builtins {
//     self.builtins.get_or_insert_with(|| {
//         self.global_info.get_builtins().clone()
//     })
// }
// ```
// or something similar.
//
// In the Java version, everything is global. Since this would involve a huge
// amount of inter-process synchronization, it makes more sense to have the
// values cached locally, since they won't change post-initialization.

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

/// The struct used to contain builtin info during the parsing of
/// `__builtins__.newlang`.
///
/// Unlike its companions, [`Builtins`] and [`GlobalBuiltins`], this struct is
/// meant to be accessed through an `&mut` reference so that the information
/// can be set.
pub struct ParsedBuiltins {
    true_builtins: Vec<Option<LangObject>>,
    all_builtins: HashMap<String, LangObject>,
    hidden_builtins: HashMap<String, LangObject>,
    globals: GlobalBuiltins,
}

/// The struct containing builtins not defined in `__builtins__.newlang`, i.e.
/// those that must be accessible during the parsing of that file.
pub struct GlobalBuiltins {
    tuple_type: TypeObject,
    null_type: TypeObject,
    type_type: TypeObject,
    callable: TypeObject,
    context: TypeObject,
    iterable: TypeObject,
    iterator: TypeObject,
    throws: TypeObject,
}

#[derive(Debug, Copy, Clone)]
pub enum BuiltinRef<'a> {
    Standard(&'a Builtins),
    Parsed(&'a ParsedBuiltins),
}

pub const FORBIDDEN_NAMES: &[&str] = &[
    "true",
    "false",
    "__default__",
    "self",
    "cls",
    "super",
    "option",
    "null",
];

pub const TRUE: BoolConstant = BoolConstant::new(true);
pub const FALSE: BoolConstant = BoolConstant::new(false);

pub const OBJECT: ObjectType = ObjectType::new();

pub const STABLE_FEATURES: &[&str] = &[];

pub static NULL_TYPE: Lazy<TypeObject> = Lazy::new(|| {
    let ty = StdTypeObject::new("null".into(), Some(Vec::new()), GenericInfo::empty(), true);
    ty.is_const_class();
    ty.seal();
    ty.into()
});
pub static THROWS_TYPE: Lazy<TypeObject> = Lazy::new(|| {
    StdTypeObject::new(
        "throws".into(),
        Some(Vec::new()),
        GenericInfo::empty(),
        true,
    )
    .into()
});
pub static CALLABLE: Lazy<TypeObject> = Lazy::new(|| {
    let args = TemplateParam::new_vararg("K".into(), 0);
    let rets = TemplateParam::new("R".into(), 1, ListTypeObject::default().into());
    let call_info = MethodInfo::new(
        LineInfo::empty(),
        AccessLevel::Public,
        false,
        FunctionInfo::with_args(
            ArgumentInfo::of_types([args.clone().into()]),
            vec![rets.clone().into()],
        ),
    );
    let callable = InterfaceType::new_operators(
        "Callable".into(),
        GenericInfo::new(vec![args, rets]),
        hash_map!(OpSpTypeNode::Call => call_info),
    );
    callable.set_generic_parent();
    callable.into()
});
static CONTEXT: Lazy<TypeObject> = Lazy::new(|| {
    let param = TemplateParam::new("T".into(), 0, OBJECT.into());
    let enter_info = MethodInfo::new(
        LineInfo::empty(),
        AccessLevel::Public,
        false,
        FunctionInfo::with_args(ArgumentInfo::of_types([param.clone().into()]), Vec::new()),
    );
    let exit_info = MethodInfo::new(
        LineInfo::empty(),
        AccessLevel::Public,
        false,
        FunctionInfo::from_returns(vec![TypeObject::list([])]),
    );
    let context = InterfaceType::new_operators(
        "Context".into(),
        GenericInfo::new(vec![param]),
        hash_map!(OpSpTypeNode::Enter => enter_info, OpSpTypeNode::Exit => exit_info),
    );
    context.set_generic_parent();
    context.into()
});
static ITERABLE: Lazy<TypeObject> = Lazy::new(|| {
    let param = TemplateParam::new_vararg("K".into(), 0);
    let iter_info = MethodInfo::new(
        LineInfo::empty(),
        AccessLevel::Public,
        false,
        FunctionInfo::with_args(ArgumentInfo::of_types([param.clone().into()]), Vec::new()),
    );
    let iterable = InterfaceType::new_operators(
        "Iterable".into(),
        GenericInfo::new(vec![param]),
        hash_map!(OpSpTypeNode::Iter => iter_info),
    );
    iterable.set_generic_parent();
    iterable.into()
});
static ITERATOR: Lazy<TypeObject> = Lazy::new(|| {
    let param = TemplateParam::new_vararg("K".into(), 0);
    let next_fn_info =
        FunctionInfo::named_with_args("next", ArgumentInfo::empty(), vec![param.clone().into()]);
    let peek_fn_info =
        FunctionInfo::named_with_args("peek", ArgumentInfo::empty(), vec![param.clone().into()]);
    let next_info = AttributeInfo::method(next_fn_info);
    let peek_info = AttributeInfo::method(peek_fn_info);
    let iter_info = MethodInfo::new(
        LineInfo::empty(),
        AccessLevel::Public,
        false,
        FunctionInfo::from_returns(vec![ITERABLE
            .generify(&LineInfo::empty(), vec![param.clone().into()])
            .unwrap()]),
    );
    let iterator = InterfaceType::new_attrs(
        "Iterator".into(),
        GenericInfo::new(vec![param]),
        hash_map!(OpSpTypeNode::Iter => iter_info),
        hash_set!(),
        hash_map!("next".into() => next_info, "peek".into() => peek_info),
        hash_set!("next".into()),
    );
    iterator.set_generic_parent();
    iterator.into()
});
static THROWS: Lazy<TypeObject> = Lazy::new(|| {
    let throws = StdTypeObject::new(
        "throws".into(),
        Some(Vec::new()),
        GenericInfo::empty(),
        true,
    );
    throws.set_generic_parent();
    throws.into()
});

pub fn auto_interfaces() -> HashSet<InterfaceType> {
    hash_set!(
        (*CONTEXT).clone().try_into().unwrap(),
        (*CALLABLE).clone().try_into().unwrap(),
        (*ITERABLE).clone().try_into().unwrap()
    )
}

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

    pub fn has_type(&self, name: &str) -> bool {
        self.all_builtins
            .get(name)
            .filter(|x| matches!(x, LangObject::Type(_)))
            .is_some()
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

    // FIXME: This is *hideous*; maybe replace Builtins w/enum when cfg(test)?
    #[cfg(test)]
    pub fn test_builtins() -> Builtins {
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

fn initial_true_builtins() -> Vec<Option<LangObject>> {
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

fn initial_all_builtins() -> HashMap<String, LangObject> {
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

impl GlobalBuiltins {
    fn new() -> Self {
        Self {
            tuple_type: TupleType::new(Vec::new()).into(),
            null_type: NULL_TYPE.clone(),
            type_type: TypeTypeObject::new_empty().into(),
            callable: CALLABLE.clone(),
            context: CONTEXT.clone(),
            iterable: ITERABLE.clone(),
            iterator: ITERATOR.clone(),
            throws: THROWS.clone(),
        }
    }
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

impl Default for ParsedBuiltins {
    fn default() -> Self {
        Self::new()
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

impl Debug for ParsedBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParsedBuiltins {{ ... }}")
    }
}

impl Debug for GlobalBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GlobalBuiltins {{ ... }}")
    }
}

#[cfg(test)]
fn simple_test_constant() -> LangConstant {
    BuiltinConstant::new(u16::MAX).into()
}

#[cfg(test)]
fn simple_test_type() -> TypeObject {
    let ty = StdTypeObject::new(
        "TEST_SHOULD_NOT_SEE".into(),
        Some(Vec::new()),
        GenericInfo::empty(),
        true,
    );
    ty.seal();
    ty.into()
}

#[cfg(test)]
fn test_lang_object() -> LangObject {
    LangObject::Constant(simple_test_constant())
}
