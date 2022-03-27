use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::error::Error;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use itertools::Itertools;
use num::ToPrimitive;
use once_cell::sync::{Lazy, OnceCell};

use crate::macros::hash_map;
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::Parser;

use super::access_handler::AccessLevel;
use super::argument::ArgumentInfo;
use super::class::MethodInfo;
use super::compiler_info::CompilerInfo;
use super::constant::{BoolConstant, BuiltinConstant, LangConstant};
use super::fn_info::FunctionInfo;
use super::generic::GenericInfo;
use super::global_info::GlobalCompilerInfo;
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

// NOTE: Maybe have InnerBuiltins have the interesting information & impl Deref
// on Builtins & GlobalBuiltins?
// Also, since we now have mutable access to GlobalCompilerInfo at the time we
// parse the builtins, can we do away with this distinction & just have
// GlobalCompilerInfo take ownership of the Builtins?

/// The [`Builtins`] instance that is associated with a [`GlobalCompilerInfo`].
///
/// The main use of this struct is to create [`Builtins`] from; it also contains
/// the machinery to parse `__builtins__.newlang`.
#[derive(Debug)]
pub struct GlobalBuiltins {
    value: OnceCell<Arc<InnerBuiltins>>,
}

/// The main struct that holds builtin information.
///
/// This contains all types, constants, and functions obtained from parsing
/// `__builtins__.newlang`, and makes them accessible from methods. The reason
/// that this is separate from [`GlobalBuiltins`] is so that each access of a
/// builtin type doesn't invoke a [`OnceCell`] access.
#[derive(Debug)]
pub struct Builtins {
    value: Arc<InnerBuiltins>,
}

/// The struct used to contain builtin info during the parsing of
/// `__builtins__.newlang`.
///
/// Unlike its companions, [`Builtins`] and [`GlobalBuiltins`], this struct is
/// meant to be accessed through an `&mut` reference so that the information
/// can be set.
#[derive(Debug)]
pub struct ParsedBuiltins {
    true_builtins: Vec<Option<LangObject>>,
    all_builtins: HashMap<String, LangObject>,
    hidden_builtins: HashMap<String, LangObject>,

    // Types given a static definition, i.e. not defined in __builtins__.newlang
    tuple_type: TypeObject,
    null_type: TypeObject,
    type_type: TypeObject,
    callable: TypeObject,
}

#[derive(Debug)]
pub struct InnerBuiltins {
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
    iter_type: TypeObject,
    hashable: TypeObject,

    tuple_type: TypeObject,
    null_type: TypeObject,
    type_type: TypeObject,
    callable: TypeObject,
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

impl GlobalBuiltins {
    pub const fn new() -> Self {
        Self {
            value: OnceCell::new(),
        }
    }

    pub fn parse(
        &self,
        global_info: &GlobalCompilerInfo,
        path: PathBuf,
    ) -> Result<(), Box<dyn Error>> {
        let node = Parser::parse_file(path.clone())??;
        let mut builtins = ParsedBuiltins::new();
        let mut info = CompilerInfo::new_builtins(global_info, path, &mut builtins);
        // FIXME: Double-check this is all that's needed for Builtins
        info.compile(&node)?;
        self.value
            .set(Arc::new(builtins.into()))
            .expect("Should only have one builtins file");
        Ok(())
    }

    pub fn get_local(&self) -> Builtins {
        Builtins {
            value: self
                .value
                .get()
                .expect("Local builtins should have been parsed")
                .clone(),
        }
    }

    pub fn try_get_local(&self) -> Option<Builtins> {
        self.value.get().map(|x| Builtins { value: x.clone() })
    }
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
}

impl Builtins {
    pub fn new(global: &GlobalBuiltins) -> Self {
        Self {
            value: global
                .value
                .get()
                .expect("Builtins should have been parsed by now")
                .clone(),
        }
    }
}

impl InnerBuiltins {
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
    type_getter!(tuple_type);
    type_getter!(range_type);
    type_getter!(null_type);
    type_getter!(slice_type);
    type_getter!(bytes_type);
    type_getter!(throwable);
    type_getter!(iterable, iter_type);
    type_getter!(type_type);
    type_getter!(callable);

    pub fn throws_type(&self) -> &TypeObject {
        todo!()
    }

    pub fn iterator(&self) -> &TypeObject {
        todo!()
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
            let rets =
                val.try_op_ret_access(LineInfo::empty(), OpSpTypeNode::Iter, AccessLevel::Public)?;
            self.de_iterable(&rets[0])
        }
    }

    pub fn builtin_name(&self, index: u16) -> Option<&str> {
        let result = self.true_builtins.get(index as usize)?;
        if result == &self.null_type {
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

impl ParsedBuiltins {
    pub fn new() -> Self {
        Self {
            // FIXME: Populate these maps
            true_builtins: Vec::new(),
            all_builtins: HashMap::new(),
            hidden_builtins: HashMap::new(),
            tuple_type: TupleType::new(Vec::new()).into(),
            null_type: NULL_TYPE.clone(),
            type_type: TypeTypeObject::new_empty().into(),
            callable: CALLABLE.clone(),
        }
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
}

impl From<ParsedBuiltins> for InnerBuiltins {
    fn from(value: ParsedBuiltins) -> Self {
        let true_builtins = value
            .true_builtins
            .into_iter()
            .map(|x| x.unwrap())
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
            iter_type: get_type_obj("iter", &value.all_builtins),
            hashable: get_type_obj("Hashable", &value.all_builtins),

            tuple_type: value.tuple_type.clone(),
            null_type: value.null_type.clone(),
            type_type: value.type_type.clone(),
            callable: value.callable.clone(),

            iter_constant: builtin_const("iter", &value.all_builtins, &true_builtins),
            range_constant: builtin_const("range", &value.all_builtins, &true_builtins),
            bool_constant: builtin_const("bool", &value.all_builtins, &true_builtins),
            null_type_constant: builtin_const_of(&value.null_type, &true_builtins),
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
    let ty = get_type_obj(name, values);
    builtin_const_of(&ty, true_builtins)
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

impl Deref for Builtins {
    type Target = InnerBuiltins;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
