use std::collections::HashSet;
use std::fmt::Debug;

use once_cell::sync::Lazy;

use crate::converter::access_handler::AccessLevel;
use crate::converter::argument::ArgumentInfo;
use crate::converter::class::{AttributeInfo, MethodInfo};
use crate::converter::constant::BoolConstant;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::{
    InterfaceFnInfo, InterfaceType, ListTypeObject, ObjectType, StdTypeObject, TemplateParam,
    TupleType, TypeObject, TypeTypeObject, UserTypeLike,
};
use crate::macros::{hash_map, hash_set};
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;

/// The struct containing builtins not defined in `__builtins__.newlang`, i.e.
/// those that must be accessible during the parsing of that file.
pub struct GlobalBuiltins {
    pub tuple_type: TypeObject,
    pub null_type: TypeObject,
    pub type_type: TypeObject,
    pub callable: TypeObject,
    pub context: TypeObject,
    pub iterable: TypeObject,
    pub iterator: TypeObject,
    pub throws: TypeObject,
}

pub const TRUE: BoolConstant = BoolConstant::new(true);
pub const FALSE: BoolConstant = BoolConstant::new(false);

pub const OBJECT: ObjectType = ObjectType::new();

pub const STABLE_FEATURES: &[&str] = &[];

pub static NULL_TYPE: Lazy<TypeObject> = Lazy::new(|| {
    let ty = StdTypeObject::new(
        "null".into(),
        Some(Vec::new()),
        GenericInfo::empty(),
        true,
        LineInfo::empty(),
    );
    ty.is_const_class();
    ty.seal(None, None);
    ty.into()
});
pub static THROWS_TYPE: Lazy<TypeObject> = Lazy::new(|| {
    StdTypeObject::new(
        "throws".into(),
        Some(Vec::new()),
        GenericInfo::empty(),
        true,
        LineInfo::empty(),
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
        LineInfo::empty(),
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
        LineInfo::empty(),
    );
    context.set_generic_parent();
    context.into()
});
pub static ITERABLE: Lazy<TypeObject> = Lazy::new(|| {
    let param = TemplateParam::new_vararg("K".into(), 0);
    let iterable = InterfaceType::new(
        "Iterable".to_string(),
        GenericInfo::new(vec![param.clone()]),
        Some(Vec::new()),
        LineInfo::empty(),
    );
    let iter_info = MethodInfo::new(
        LineInfo::empty(),
        AccessLevel::Public,
        false,
        FunctionInfo::from_returns(vec![iterable
            .generify(LineInfo::empty(), vec![param.into()])
            .unwrap()]),
    );
    let iter_fn_info = InterfaceFnInfo::new(iter_info, false);
    iterable.set_operators(hash_map!(OpSpTypeNode::Iter => iter_fn_info));
    iterable.seal(None, None);
    iterable.set_generic_parent();
    iterable.into()
});
pub static ITERATOR: Lazy<TypeObject> = Lazy::new(|| {
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
        GenericInfo::new(vec![param.clone()]),
        Some(vec![ITERABLE
            .generify(LineInfo::empty_ref(), vec![param.into()])
            .unwrap()]),
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
        LineInfo::empty(),
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

impl GlobalBuiltins {
    pub(super) fn new() -> Self {
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

impl Debug for GlobalBuiltins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GlobalBuiltins {{ ... }}")
    }
}
