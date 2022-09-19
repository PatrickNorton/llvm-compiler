mod attribute;
mod base_class;
mod class_converter;
mod class_info;
mod converter_holder;
mod enum_converter;
mod interface_converter;
mod method;
mod operator_def;
mod property;
mod union_converter;
mod variant;

use itertools::Itertools;

pub use self::attribute::AttributeInfo;
pub use self::base_class::{BaseClass, BodiedClass};
pub use self::class_converter::ClassConverter;
pub use self::class_info::ClassInfo;
pub use self::enum_converter::EnumConverter;
pub use self::interface_converter::InterfaceConverter;
pub use self::method::MethodInfo;
pub use self::union_converter::UnionConverter;
pub use self::variant::VariantConverter;

use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::hash::Hash;
use std::iter::zip;

use crate::parser::annotation::AnnotatableNode;
use crate::parser::class_def::ClassStatementNode;
use crate::parser::definition::BaseClassRef;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::reborrow_option;

use self::converter_holder::ConverterHolder;
use self::method::{Method, RawMethod};

use super::access_handler::{AccessHandler, AccessLevel};
use super::base_converter::BaseConverter;
use super::builtins::{BuiltinRef, FORBIDDEN_NAMES};
use super::compiler_info::CompilerInfo;
use super::constant::{ClassConstant, LangConstant};
use super::convertible::ConverterBase;
use super::default_holder::DefaultHolder;
use super::error::{CompilerException, CompilerInternalError};
use super::fn_info::FunctionInfo;
use super::type_loader::TypeLoader;
use super::type_obj::{SuperRef, TypeObject, UserType};
use super::warning::{self, WarningType};
use super::{annotation, CompileResult};

trait ClassConverterBase<'a>: ConverterBase {
    type Converted: BaseClass + 'a;

    fn get_node(&self) -> &'a Self::Converted;

    fn convert_inner<'b, T: Eq + Hash + IsOperatorNew>(
        &self,
        info: &mut CompilerInfo,
        type_val: &UserType,
        functions: impl IntoIterator<Item = (T, RawMethod<'b>)>,
    ) -> CompileResult<HashMap<T, Method>> {
        let functions = functions.into_iter();
        let mut result = HashMap::with_capacity(functions.size_hint().0);
        for (key, method_info) in functions {
            let is_const_method = !method_info.is_mut;
            let generic_info = method_info.info.get_generics();
            let is_operator_new = key.is_operator_new();
            let has_generics = !generic_info.is_empty();
            if has_generics {
                info.add_local_types(type_val.clone().into(), generic_info.get_param_map());
            }
            info.add_stack_frame();
            info.add_variable(
                "self".to_string(),
                if is_const_method {
                    type_val.make_const().into()
                } else {
                    type_val.make_mut().into()
                },
                is_const_method,
                self.get_node().line_info().clone(),
            );
            let var_type = info
                .builtins()
                .type_type()
                .generify(&LineInfo::empty(), vec![type_val.clone().into()])?;
            if type_val.is_final() && type_val.get_generic_info().is_empty() {
                // Classes can be used as a constant sometimes!
                let constant = ClassConstant::new(
                    &type_val.name(),
                    info.class_index(type_val),
                    type_val.clone(),
                );
                info.add_constant_variable(
                    "cls".to_string(),
                    var_type.clone(),
                    constant.into(),
                    self.get_node().line_info().clone(),
                );
                info.add_variable(
                    String::new(),
                    var_type,
                    true,
                    self.get_node().line_info().clone(),
                );
            } else {
                info.add_variable(
                    "cls".to_string(),
                    var_type,
                    true,
                    self.get_node().line_info().clone(),
                );
            }
            let handler = info.access_handler_mut();
            handler.allow_private_access(type_val.clone().into());
            handler.add_cls(type_val.clone().into());
            recursively_allow_protected_access(handler, type_val);
            // No ? here b/c we want to clean up before propagating errors
            let method = convert_method(info, type_val, &key, method_info);
            info.remove_stack_frame();
            if has_generics {
                info.remove_local_types();
            }
            info.access_handler_mut()
                .remove_private_access(type_val.clone().into());
            info.access_handler_mut().remove_cls();
            recursively_remove_protected_access(info.access_handler_mut(), type_val);
            if is_operator_new {
                info.access_handler_mut().exit_constructor();
            }
            result.insert(key, method?);
        }
        Ok(result)
    }

    fn parse_statements(
        &self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        mut defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()>
    where
        Self::Converted: BodiedClass,
    {
        let op_converter = &mut converter.ops;
        let operators = annotation::derive_operators(self.get_node().get_annotations())?;
        for op in operators {
            op_converter.parse_derived(info, op, &self.get_node().get_annotations()[0])?;
        }
        for stmt in self.get_node().get_body() {
            if let Option::Some(annotations) = AnnotatableNode::try_get_annotations(stmt) {
                if annotation::should_compile(stmt, info, annotations)? {
                    self.parse_statement(info, stmt, converter, reborrow_option(&mut defaults))?
                }
            } else {
                self.parse_statement(info, stmt, converter, reborrow_option(&mut defaults))?
            }
        }
        Ok(())
    }

    fn parse_statement(
        &self,
        info: &mut CompilerInfo,
        stmt: &'a ClassStatementNode,
        converter: &mut ConverterHolder<'a>,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        match stmt {
            ClassStatementNode::Declaration(d) => converter.attrs.parse(info, d),
            ClassStatementNode::DeclaredAssign(d) => converter.attrs.parse_assign(info, d),
            ClassStatementNode::Method(m) => converter.methods.parse(info, m, defaults),
            ClassStatementNode::Operator(o) => converter.ops.parse(info, o, defaults),
            ClassStatementNode::Property(p) => converter.props.parse(info, p),
            _ => Err(CompilerInternalError::of(
                format!("Unknown class statement {:?}", stmt),
                stmt,
            )
            .into()),
        }
    }

    fn get_super_constants(
        &self,
        info: &mut CompilerInfo,
        supers: SuperRef<'_>,
    ) -> CompileResult<Vec<LangConstant>> {
        zip(supers, self.get_node().get_superclasses())
            .map(|(ty, node)| {
                TypeLoader::type_constant(node.line_info(), ty, info)?.ok_or_else(|| {
                    CompilerException::of("Cannot yet serialize local types", node).into()
                })
            })
            .collect()
    }

    fn put_in_info(
        &self,
        info: &mut CompilerInfo,
        type_val: UserType,
        def_type: &str,
        variants: Option<Vec<String>>,
        super_constants: Vec<LangConstant>,
        converter: ConverterHolder<'_>,
    ) -> CompileResult<()> {
        let node = self.get_node();
        let name = node.get_name().str_name();
        info.check_definition(name, node)?;
        let constant = ClassConstant::new(name, info.class_index(&type_val), type_val.clone());
        let type_obj = type_type(info.builtins(), node, type_val.clone())?;
        info.add_constant_variable(
            name.to_string(),
            type_obj,
            constant.into(),
            node.line_info().clone(),
        );
        let cls = self.create(info, type_val, variants, super_constants, converter)?;
        info.set_class(cls);
        if FORBIDDEN_NAMES.contains(&name) {
            Err(CompilerException::of(
                format!("Illegal name for {} '{}'", def_type, name),
                node.get_name(),
            )
            .into())
        } else {
            Ok(())
        }
    }

    fn add_to_info(
        &self,
        info: &mut CompilerInfo,
        type_val: UserType,
        def_type: &str,
        variants: Option<Vec<String>>,
        super_constants: Vec<LangConstant>,
        converter: ConverterHolder<'_>,
    ) -> CompileResult<()> {
        let node = self.get_node();
        let name = node.get_name().str_name();
        info.check_definition(name, node)?;
        let type_obj = type_type(info.builtins(), node, type_val.clone())?;
        info.reserve_const_var(name.to_string(), type_obj, node.line_info().clone());
        let cls = self.create(info, type_val.clone(), variants, super_constants, converter)?;
        let class_index = info.add_class(cls);
        if FORBIDDEN_NAMES.contains(&name) {
            Err(CompilerException::of(
                format!("Illegal name for {} '{}'", def_type, name),
                node.get_name(),
            )
            .into())
        } else {
            info.set_reserved_var(name, ClassConstant::new(name, class_index, type_val).into());
            Ok(())
        }
    }

    fn create(
        &self,
        info: &mut CompilerInfo,
        type_val: UserType,
        variants: Option<Vec<String>>,
        super_constants: Vec<LangConstant>,
        converter: ConverterHolder<'_>,
    ) -> CompileResult<ClassInfo> {
        info.access_handler_mut().add_cls(type_val.clone().into());
        info.add_local_types(
            type_val.clone().into(),
            type_val.get_generic_info().get_param_map(),
        );
        let result = self.create_class(info, type_val, variants, super_constants, converter);
        info.access_handler_mut().remove_cls();
        info.remove_local_types();
        result
    }

    fn create_class(
        &self,
        info: &mut CompilerInfo,
        ty: UserType,
        variants: Option<Vec<String>>,
        super_constants: Vec<LangConstant>,
        converter: ConverterHolder<'_>,
    ) -> CompileResult<ClassInfo> {
        let mut factory = ClassInfo::factory();
        let (ops, static_ops) = converter.ops.take_ops();
        let (methods, static_methods) = converter.methods.take_methods();
        let (vars, static_vars) = converter.attrs.vars_with_ints();
        // FIXME: Colons
        let (getters, setters, static_getters, static_setters) = converter.props.split();
        factory
            .set_super_constants(super_constants)
            .set_variables(vars)
            .set_static_vars(static_vars)
            .set_operator_defs(self.convert_inner(
                info,
                &ty,
                ops.into_iter().map(|(x, (_, y))| (x, y)),
            )?)
            .set_static_ops(self.convert_inner(
                info,
                &ty,
                static_ops.into_iter().map(|(x, (_, y))| (x, y)),
            )?)
            .set_method_defs(self.convert_inner(
                info,
                &ty,
                methods.into_iter().map(|(x, y)| (x.to_string(), y)),
            )?)
            .set_static_methods(self.convert_inner(
                info,
                &ty,
                static_methods.into_iter().map(|(x, y)| (x.to_string(), y)),
            )?)
            .set_properties(merge(
                self.convert_inner(info, &ty, getters.map(|(x, y)| (x.to_string(), y)))?,
                self.convert_inner(info, &ty, setters.map(|(x, y)| (x.to_string(), y)))?,
            ))
            .set_static_props(merge(
                self.convert_inner(info, &ty, static_getters.map(|(x, y)| (x.to_string(), y)))?,
                self.convert_inner(info, &ty, static_setters.map(|(x, y)| (x.to_string(), y)))?,
            ))
            .set_type(ty)
            .set_variants(variants);
        Ok(factory.create())
    }
}

#[inline]
fn convert_method<U: IsOperatorNew + Eq + Hash>(
    info: &mut CompilerInfo,
    type_val: &UserType,
    key: &U,
    method_info: RawMethod<'_>,
) -> CompileResult<Method> {
    let fn_info = &method_info.info;
    for arg in fn_info.get_args().iter() {
        if arg.is_vararg() {
            let iterable = info
                .builtins()
                .iterable()
                .generify(arg, vec![arg.get_type().clone()])?;
            info.add_variable(
                arg.get_name().to_string(),
                iterable,
                true,
                method_info.line_info().clone(),
            );
        } else {
            info.add_variable(
                arg.get_name().to_string(),
                arg.get_type().clone(),
                true,
                method_info.line_info().clone(),
            );
        }
    }
    let returns = fn_returns(fn_info, info.builtins())?;
    let ret_info = info.fn_returns_mut();
    ret_info.add_fn_returns(
        fn_info.is_generator(),
        returns,
        fn_info.line_info().clone(),
        Some(method_info.info.get_name().to_string()),
    );
    if key.is_operator_new() {
        info.access_handler_mut()
            .enter_constructor(type_val.clone().into());
    }
    let (bytes, ret_info) = BaseConverter::bytes_with_return(&*method_info.body, info)?;
    if ends_without_returning(type_val, fn_info, info.builtins(), ret_info.will_return())? {
        warning::warn(
            "Function ends without returning",
            WarningType::NoType,
            info,
            &method_info.line_info,
        )?;
    }
    info.fn_returns_mut().pop_fn_returns();
    let line_info = method_info.line_info.clone();
    Ok(Method::new(line_info, method_info.into(), bytes))
}

pub fn set_supers(info: &mut CompilerInfo, node: BaseClassRef<'_>) -> CompileResult<()> {
    let type_val = info.get_type_obj(node.str_name()).unwrap();
    let user_type: UserType = type_val.clone().try_into().unwrap();
    info.add_local_types(
        type_val.clone(),
        user_type.get_generic_info().get_param_map(),
    );
    let supers = convert_supers(&node, info.types_of(node.get_superclasses())?)?;
    let true_supers = supers.into_iter().map_into().collect();
    match user_type {
        UserType::Interface(_) => {} // TODO
        UserType::Std(s) => s.set_supers(true_supers),
        UserType::Union(u) => u.set_supers(true_supers),
    }
    info.remove_local_types();
    Ok(())
}

pub(self) fn convert_supers(
    node: &impl BaseClass,
    supers: Vec<TypeObject>,
) -> CompileResult<Vec<UserType>> {
    supers
        .into_iter()
        .map(|x| x.try_into())
        .collect::<Result<_, _>>()
        .map_err(|_| {
            CompilerException::of(
                format!(
                    "Class '{}' inherits from a non-standard type",
                    node.get_name().str_name()
                ),
                node,
            )
            .into()
        })
}

pub(self) fn ensure_proper_inheritance(
    node: impl Lined,
    ty: &UserType,
    supers: &[UserType],
) -> CompileResult<()> {
    for super_cls in supers {
        if super_cls.is_final() {
            return Err(CompilerException::of(
                format!(
                    "Class '{}' inherits from class '{}', which is not marked 'nonfinal'",
                    ty.name(),
                    super_cls.name()
                ),
                node,
            )
            .into());
        }
    }
    Ok(())
}

fn recursively_allow_protected_access(handler: &mut AccessHandler, type_val: &UserType) {
    for super_cls in type_val.get_supers() {
        handler.allow_protected_access(super_cls.clone());
        if let TypeObject::Std(super_cls) = super_cls {
            // TODO: Remove clone
            recursively_allow_protected_access(handler, &super_cls.clone().into())
        }
    }
}

fn recursively_remove_protected_access(handler: &mut AccessHandler, type_val: &UserType) {
    for super_cls in type_val.get_supers() {
        handler.remove_protected_access(super_cls.clone());
        if let TypeObject::Std(super_cls) = super_cls {
            // TODO: Remove clone
            recursively_remove_protected_access(handler, &super_cls.clone().into())
        }
    }
}

fn ends_without_returning(
    type_val: &UserType,
    fn_info: &FunctionInfo,
    builtins: BuiltinRef<'_>,
    returns: bool,
) -> CompileResult<bool> {
    if fn_info.is_generator() || fn_returns(fn_info, builtins)?.is_empty() || returns {
        return Ok(false);
    }
    let (names, operators) = type_val.contract();
    Ok(if names.contains(fn_info.get_name()) {
        false
    } else {
        !operators
            .iter()
            .any(|x| x.to_string() == fn_info.get_name())
    })
}

fn fn_returns(fn_info: &FunctionInfo, builtins: BuiltinRef<'_>) -> CompileResult<Vec<TypeObject>> {
    if !fn_info.is_generator() {
        Ok(fn_info.get_returns().to_owned())
    } else {
        let returns = fn_info.get_returns();
        assert_eq!(returns.len(), 1);
        builtins.de_iterable(&returns[0])
    }
}

fn merge<T: Eq + Hash, U>(a: HashMap<T, U>, mut b: HashMap<T, U>) -> HashMap<T, (U, U)> {
    assert_eq!(a.len(), b.len());
    a.into_iter()
        .map(|(x, y)| {
            let z = b.remove(&x).unwrap();
            (x, (y, z))
        })
        .collect()
}

pub(self) fn check_contract(
    node: impl Lined,
    ty: &UserType,
    supers: SuperRef<'_>,
    builtins: BuiltinRef<'_>,
) -> CompileResult<()> {
    let mut_ty = ty.make_mut();
    for sup in supers {
        if ty.same_base_type(sup) {
            continue;
        }
        if let Option::Some((methods, operators)) = contract_by_ref(sup) {
            for attr in methods {
                if mut_ty.attr_ty_access(attr, AccessLevel::Public).is_none() {
                    return Err(CompilerException::of(
                        format!(
                            "Missing impl for method '{}' (defined by interface '{}')",
                            attr,
                            sup.name()
                        ),
                        node,
                    )
                    .into());
                }
            }
            for &op in operators {
                // FIXME: Mutability
                if mut_ty
                    .op_info_access(op, AccessLevel::Public, builtins)
                    .is_none()
                {
                    return Err(CompilerException::of(
                        format!(
                            "Missing impl for {} (defined by interface '{}')",
                            op,
                            sup.name()
                        ),
                        node,
                    )
                    .into());
                }
            }
        }
    }
    Ok(())
}

fn contract_by_ref(ty: &TypeObject) -> Option<&(HashSet<String>, HashSet<OpSpTypeNode>)> {
    match ty {
        TypeObject::Interface(i) => Some(i.contract()),
        TypeObject::Std(s) => Some(s.contract()),
        TypeObject::Union(u) => Some(u.contract()),
        _ => None,
    }
}

fn type_type(
    builtins: BuiltinRef<'_>,
    lined: &dyn Lined,
    ty: UserType,
) -> CompileResult<TypeObject> {
    builtins.type_type().generify(lined, vec![ty.into()])
}

// Trait for the operator-new checks in convert_inner
trait IsOperatorNew {
    fn is_operator_new(&self) -> bool;
}

impl IsOperatorNew for OpSpTypeNode {
    fn is_operator_new(&self) -> bool {
        self == &OpSpTypeNode::New
    }
}

impl IsOperatorNew for String {
    fn is_operator_new(&self) -> bool {
        false
    }
}

impl IsOperatorNew for &'_ str {
    fn is_operator_new(&self) -> bool {
        false
    }
}
