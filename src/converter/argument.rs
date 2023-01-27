use std::cmp::max;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::ops::{Deref, Index};
use std::sync::Arc;
use std::{ptr, slice};

use derive_new::new;
use itertools::Itertools;
use once_cell::sync::OnceCell;

use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::typed_arg::{TypedArgumentListNode, TypedArgumentNode};
use crate::util::first;
use crate::util::maybe_ref::MaybeRef;

use super::builtins::FORBIDDEN_NAMES;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{LangConstant, OptionConstant};
use super::convertible::{ConverterBase, ConverterTest, TestConvertible};
use super::default_holder::DefaultHolder;
use super::error::{CompilerError, CompilerException, CompilerInternalError, CompilerTodoError};
use super::fn_info::FunctionInfo;
use super::function::Function;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::CompileResult;

#[derive(Debug, Clone)]
pub struct Argument {
    line_info: LineInfo,
    name: String,
    type_val: TypeObject,
    is_vararg: bool,
    default_val: Option<Arc<DefaultValue>>,
}

#[derive(Debug, Clone, new)]
pub struct ArgumentInfo {
    position_args: Vec<Argument>,
    normal_args: Vec<Argument>,
    keyword_args: Vec<Argument>,
}

#[derive(Debug)]
pub enum ArgPosition<'a> {
    Default(&'a DefaultValue),
    Standard(u16),
    Vararg(Vec<u16>, TypeObject),
}

#[derive(Debug)]
pub struct DefaultValue {
    parent_type: TypeObject,
    value: OnceCell<(BytecodeList, Option<LangConstant>)>,
    bytes_index: OnceCell<u16>,
}

impl Argument {
    pub fn new(name: String, type_val: TypeObject) -> Self {
        Self {
            line_info: LineInfo::empty(),
            name,
            type_val,
            is_vararg: false,
            default_val: None,
        }
    }

    pub fn new_full(
        name: String,
        type_val: TypeObject,
        is_vararg: bool,
        line_info: LineInfo,
    ) -> Self {
        Self {
            line_info,
            name,
            type_val,
            is_vararg,
            default_val: None,
        }
    }

    pub fn new_default<'a>(
        name: String,
        type_val: TypeObject,
        is_vararg: bool,
        line_info: LineInfo,
        default_val: &'a TestNode,
        default_holder: &mut DefaultHolder<'a>,
    ) -> Self {
        let default_value = Arc::new(DefaultValue::new(type_val.clone()));
        default_holder.add_argument(default_value.clone(), default_val);
        Self {
            line_info,
            name,
            type_val,
            is_vararg,
            default_val: Some(default_value),
        }
    }

    pub fn from_type(type_val: TypeObject) -> Self {
        Self::new(String::new(), type_val)
    }

    pub fn get_type(&self) -> &TypeObject {
        &self.type_val
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn is_vararg(&self) -> bool {
        self.is_vararg
    }

    pub fn clone_with_type(&self, ty: TypeObject) -> Self {
        Self {
            line_info: self.line_info.clone(),
            name: self.name.clone(),
            type_val: ty,
            is_vararg: self.is_vararg,
            default_val: self.default_val.clone(),
        }
    }
}

impl ArgumentInfo {
    pub fn of_types<const N: usize>(args: [TypeObject; N]) -> Self {
        Self {
            position_args: args.into_iter().map(Argument::from_type).collect(),
            normal_args: Vec::new(),
            keyword_args: Vec::new(),
        }
    }

    pub fn of_args<const N: usize>(args: [Argument; N]) -> Self {
        Self {
            position_args: args.into_iter().collect(),
            normal_args: Vec::new(),
            keyword_args: Vec::new(),
        }
    }

    pub const fn empty() -> Self {
        Self {
            position_args: Vec::new(),
            normal_args: Vec::new(),
            keyword_args: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.position_args.len() + self.normal_args.len() + self.keyword_args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.position_args.is_empty() && self.normal_args.is_empty() && self.keyword_args.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&Argument> {
        self.position_args.get(index).or_else(|| {
            self.normal_args
                .get(index - self.position_args.len())
                .or_else(|| {
                    self.keyword_args
                        .get(index - self.position_args.len() - self.normal_args.len())
                })
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = &Argument> {
        self.position_args
            .iter()
            .chain(self.normal_args.iter())
            .chain(self.keyword_args.iter())
    }

    pub fn iter_rev(&self) -> impl Iterator<Item = &Argument> {
        self.keyword_args
            .iter()
            .rev()
            .chain(self.normal_args.iter().rev())
            .chain(self.keyword_args.iter().rev())
    }

    pub fn get_position_args(&self) -> &[Argument] {
        &self.position_args
    }

    pub fn get_normal_args(&self) -> &[Argument] {
        &self.normal_args
    }

    pub fn get_keyword_args(&self) -> &[Argument] {
        &self.keyword_args
    }

    pub fn arg_positions(&self, args: &[Argument]) -> CompileResult<Vec<ArgPosition<'_>>> {
        assert!(self.vararg_is_valid());
        if self.has_vararg() {
            self.arg_positions_with_vararg(args)
        } else {
            self.arg_positions_no_vararg(args)
        }
    }

    pub fn arg_positions_with_vararg(
        &self,
        args: &[Argument],
    ) -> CompileResult<Vec<ArgPosition<'_>>> {
        let new_args = self.expand_tuples(args)?;
        let kw_positions = get_kw_positions(args);
        assert!(self.vararg_is_valid() && self.has_vararg());
        // The total number of arguments in the declaration with a possible
        // default parameter, excluding those which are explicitly passed as a
        // kwarg
        let default_count = self.args_with_defaults(&kw_positions);
        // The total number of keyword-only arguments in the declaration which
        // will be using their default parameter
        let defaulted_kwargs = self.kwargs_with_defaults(&kw_positions);
        assert!(defaulted_kwargs <= default_count);
        // The total number of positionally-passable arguments in the
        // declaration which do not have a matched value yet
        let unused = self.len() - kw_positions.len() - defaulted_kwargs;
        // The total number of non-keyword arguments in the invocation
        let non_keyword_count = new_args.len() - kw_positions.len();
        // The number of positional arguments which will be given a default
        // parameter
        let defaults_used = (unused + 1).saturating_sub(non_keyword_count);
        // The number of default arguments which will take a positional
        // parameter in the argument list
        let mut defaults_unused = default_count - defaults_used - defaulted_kwargs;
        // The size of the variadic argument
        let vararg_count = max(non_keyword_count - unused + 1, 0);
        let mut arg_no = 0;
        Ok(self
            .iter()
            .map(|value_arg| {
                if let Option::Some(&pos) = kw_positions.get(&*value_arg.name) {
                    ArgPosition::Standard(pos as u16)
                } else if value_arg.is_vararg {
                    let mut values = Vec::with_capacity(vararg_count);
                    for _ in 0..vararg_count {
                        values.push(arg_no as u16);
                        arg_no = self.next_eligible_param(arg_no + 1, args);
                    }
                    ArgPosition::Vararg(values, value_arg.type_val.clone())
                } else if value_arg.default_val.is_none() {
                    let arg = arg_no;
                    arg_no = self.next_eligible_param(arg_no + 1, args);
                    ArgPosition::Standard(arg as u16)
                } else if defaults_unused > 0 {
                    let arg = arg_no;
                    defaults_unused -= 1;
                    arg_no = self.next_eligible_param(arg_no + 1, args);
                    ArgPosition::Standard(arg as u16)
                } else {
                    let default_val = value_arg.default_val.as_ref().unwrap();
                    ArgPosition::Default(default_val)
                }
            })
            .collect())
    }

    pub fn arg_positions_no_vararg(
        &self,
        args: &[Argument],
    ) -> CompileResult<Vec<ArgPosition<'_>>> {
        // FIXME? Ensure none of this math will overflow
        let new_args = self.expand_tuples(args)?;
        let kw_positions = get_kw_positions(args);
        let default_count = self.args_with_defaults(&kw_positions);
        let non_keyword_count = new_args.len() - kw_positions.len();
        let unused = self.len() - kw_positions.len();
        let defaults_used = unused - non_keyword_count;
        let mut defaults_unused = default_count.saturating_sub(defaults_used);
        let mut arg_no = 0;
        Ok(self
            .iter()
            .map(|value_arg| {
                if let Option::Some(pos) = kw_positions.get(&*value_arg.name) {
                    ArgPosition::Standard(*pos as u16)
                } else if value_arg.default_val.is_none() {
                    arg_no += 1;
                    ArgPosition::Standard(arg_no - 1)
                } else if defaults_unused > 0 {
                    arg_no += 1;
                    defaults_unused -= 1;
                    ArgPosition::Standard(arg_no - 1)
                } else {
                    let default_val = value_arg.default_val.as_ref().unwrap();
                    ArgPosition::Default(default_val)
                }
            })
            .collect())
    }

    pub fn matches(&self, parent: &FunctionInfo, args: &[Argument]) -> bool {
        self.generify_args(parent, args).is_ok()
    }

    pub fn generify_args(
        &self,
        parent: &FunctionInfo,
        args: &[Argument],
    ) -> CompileResult<(HashMap<u16, TypeObject>, HashSet<u16>)> {
        assert!(ptr::eq(parent.get_args(), self));
        let par = parent.to_callable();
        let new_args = self.expand_tuples(args)?;
        let keyword_map = Self::init_keywords(&new_args)?;
        let mut result = HashMap::new();
        let mut needs_make_option = HashSet::new();
        // Check if all given keyword arguments are subclasses of the declaration
        let mut matched_count = 0;
        for arg in self.all_keywords() {
            if let Option::Some(&keyword_type) = keyword_map.get(&*arg.name) {
                if update(
                    self.index_of(&arg.name).unwrap(),
                    &par,
                    &mut result,
                    &mut needs_make_option,
                    arg,
                    keyword_type,
                ) {
                    matched_count += 1;
                } else {
                    return Err(CompilerException::of(
                        format!(
                            "Argument mismatch: Keyword argument {} is of type '{}', \
                            which is not assignable to type '{}'",
                            arg.name,
                            keyword_type.name(),
                            arg.get_type().name()
                        ),
                        find_name(&arg.name, &new_args)?,
                    )
                    .into());
                }
            }
        }
        // Ensure the number of matched keywords is the same as the total number of keywords
        // If it's not, then there are keyword arguments passed that don't have an equivalent
        // in the function definition
        if matched_count != keyword_map.len() {
            for keyword in keyword_map.keys() {
                if self.index_of(keyword).is_none() {
                    let keyword_arg = find_name(keyword, &new_args)?;
                    return Err(CompilerException::of(
                        format!("Keyword argument {keyword} is unexpected"),
                        keyword_arg,
                    )
                    .into());
                }
            }
        }
        // Check that all keyword-only arguments are either used or have default
        for arg in &self.keyword_args {
            if !keyword_map.contains_key(&*arg.name) && arg.default_val.is_none() {
                // FIXME: Get line info for all missing-argument scenarios
                let line_info = new_args
                    .first()
                    .map_or(LineInfo::empty_ref(), |x| x.line_info());
                return Err(CompilerException::of(
                    format!(
                        "Missing value for required keyword-only argument {}",
                        arg.name
                    ),
                    line_info,
                )
                .into());
            }
        }
        assert!(self.vararg_is_valid());
        let default_count = self.args_with_defaults(&keyword_map);
        let non_keyword_count = new_args.len() - keyword_map.len();
        let unused = self.len() - keyword_map.len();
        let defaults_used = unused.checked_sub(non_keyword_count);
        if self.has_vararg() && keyword_map.contains_key(&*self.normal_args.last().unwrap().name) {
            return Err(CompilerException::of(
                "Vararg cannot be referred to as a keyword argument",
                LineInfo::empty(),
            )
            .into());
        }
        match defaults_used {
            Option::None => {
                if !self.has_vararg() {
                    let count_str = if default_count == 0 {
                        "exactly"
                    } else {
                        "no more than"
                    };
                    return Err(CompilerException::of(
                        format!(
                            "Too many parameters passed: \
                             Expected {count_str} {unused} unnamed parameters, \
                             got {non_keyword_count}",
                        ),
                        &args[0],
                    )
                    .into());
                }
            }
            Option::Some(defaults_used) if defaults_used > default_count => {
                if !self.has_vararg() || defaults_used != default_count + 1 {
                    return Err(self.not_enough_args(&new_args, &keyword_map));
                }
            }
            _ => (),
        }
        let mut defaults_unused = match defaults_used {
            Option::Some(def) => default_count - def,
            Option::None => default_count + non_keyword_count - unused,
        };
        let mut next_arg = self.next_eligible_arg(0, &keyword_map, defaults_unused > 0);
        for arg in &new_args {
            if keyword_map.contains_key(&*arg.name) {
                continue;
            } else if next_arg >= self.len() {
                return Err(CompilerInternalError::with_note(
                    "Error in parameter expansion: nextEligibleArg() should never return >= size",
                    "All cases where this branch is taken should be \
                     spotted earlier in the function",
                    &**arg,
                )
                .into());
            }
            let arg_value = &self[next_arg];
            if !update(
                next_arg as u16,
                &par,
                &mut result,
                &mut needs_make_option,
                arg_value,
                arg.get_type(),
            ) {
                return Err(CompilerInternalError::of(
                    format!(
                        "Argument mismatch: Argument is of type '{}', \
                         which is not assignable to type '{}'",
                        arg.type_val.name(),
                        arg_value.get_type().name()
                    ),
                    &**arg,
                )
                .into());
            }
            if arg_value.default_val.is_some() {
                defaults_unused -= 1;
            }
            // Because the variadic argument is always the last non-keyword one,
            // we don't update it once we reach it, since any other arguments
            // we get from here on out are going to be part of this one.
            if !arg_value.is_vararg {
                next_arg = self.next_eligible_arg(next_arg + 1, &keyword_map, defaults_unused > 0);
            }
        }
        Ok((result, needs_make_option))
    }

    fn expand_tuples<'a>(
        &self,
        args: &'a [Argument],
    ) -> CompileResult<Vec<MaybeRef<'a, Argument>>> {
        let mut result = Vec::with_capacity(args.len());
        for arg in args {
            if arg.is_vararg {
                if !arg.name.is_empty() {
                    return Err(CompilerException::with_note(
                        "Illegal parameter expansion in argument",
                        "Named arguments cannot be expanded",
                        arg,
                    )
                    .into());
                } else if let TypeObject::Tuple(tup) = &arg.type_val {
                    for generic in tup.get_generics() {
                        result.push(Argument::new(String::new(), generic.clone()).into())
                    }
                } else {
                    return Err(CompilerException::with_note(
                        "Illegal parameter expansion in argument",
                        format!("Type '{}' is not a tuple", arg.type_val.name()),
                        arg,
                    )
                    .into());
                }
            } else {
                result.push(arg.into())
            }
        }
        Ok(result)
    }

    pub fn of<'a>(
        args: &'a TypedArgumentListNode,
        info: &mut CompilerInfo,
        mut defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<ArgumentInfo> {
        let pos_args = get_args(info, args.get_pos_args(), &mut defaults)?;
        let normal_args = get_args(info, args.get_args(), &mut defaults)?;
        let kw_args = get_args(info, args.get_name_args(), &mut defaults)?;
        Ok(ArgumentInfo::new(pos_args, normal_args, kw_args))
    }

    fn init_keywords<T: Deref<Target = Argument>>(
        args: &[T],
    ) -> CompileResult<HashMap<&str, &TypeObject>> {
        let mut keyword_map = HashMap::with_capacity(args.len());
        for arg in args {
            if !arg.name.is_empty() {
                // NOTE: This can be simplified with feature(map_try_insert) (#82766)
                match keyword_map.entry(&*arg.name) {
                    Entry::Occupied(_) => {
                        return Err(CompilerException::of(
                            format!(
                                "Keyword argument {} defined twice in parameter list",
                                arg.name
                            ),
                            &**arg,
                        )
                        .into())
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(arg.get_type());
                    }
                }
            }
        }
        Ok(keyword_map)
    }

    fn all_keywords(&self) -> impl Iterator<Item = &Argument> {
        self.normal_args.iter().chain(self.keyword_args.iter())
    }

    fn index_of(&self, name: &str) -> Option<u16> {
        self.iter()
            .enumerate()
            .find(|(_, val)| name == &*val.name)
            .map(|(i, _)| i as u16)
    }

    fn vararg_is_valid(&self) -> bool {
        match self.normal_args.last() {
            Option::Some(normal) => self.iter().all(|x| ptr::eq(x, normal) || !x.is_vararg),
            Option::None => true,
        }
    }

    fn args_with_defaults<T>(&self, to_exclude: &HashMap<&str, T>) -> usize {
        self.iter()
            .filter(|x| !to_exclude.contains_key(&*x.name) && x.default_val.is_some())
            .count()
    }

    fn has_vararg(&self) -> bool {
        debug_assert!(self.vararg_is_valid());
        self.normal_args.last().map_or(false, |x| x.is_vararg)
    }

    fn not_enough_args<T: Deref<Target = Argument>>(
        &self,
        new_args: &[T],
        keyword_map: &HashMap<&str, &TypeObject>,
    ) -> CompilerError {
        let unmatched = self
            .iter_rev()
            .map(|x| &*x.name)
            .filter(|x| !keyword_map.contains_key(x))
            .collect_vec();
        let arg_line_info = new_args
            .first()
            .map_or(LineInfo::empty_ref(), |x| x.line_info());
        match *unmatched {
            [] => CompilerInternalError::of(
                "`Argument::not_enough_args` was called with no unmatched arguments",
                arg_line_info,
            )
            .into(),
            [value] => CompilerException::of(
                format!("Missing value for positional argument {value}"),
                arg_line_info,
            )
            .into(),
            _ => CompilerException::of(
                format!(
                    "Missing value for positional arguments {}",
                    unmatched.iter().format(", ")
                ),
                arg_line_info,
            )
            .into(),
        }
    }

    fn next_eligible_arg<T>(
        &self,
        mut next_unused: usize,
        keyword_args: &HashMap<&str, T>,
        defaults_remaining: bool,
    ) -> usize {
        while let Option::Some(current_arg) = self.get(next_unused) {
            if !keyword_args.contains_key(&*current_arg.name)
                && (defaults_remaining || current_arg.default_val.is_none())
            {
                return next_unused;
            }
            next_unused += 1;
        }
        next_unused
    }

    fn next_eligible_param(&self, next_unused: usize, params: &[Argument]) -> usize {
        params[next_unused..]
            .iter()
            .position(|param| param.name.is_empty())
            .map_or_else(|| params.len(), |x| x + next_unused)
    }

    fn kwargs_with_defaults<T>(&self, to_exclude: &HashMap<&str, T>) -> usize {
        self.keyword_args
            .iter()
            .filter(|arg| !to_exclude.contains_key(&*arg.name) && arg.default_val.is_some())
            .count()
    }
}

impl<'a> ArgPosition<'a> {
    #[inline]
    pub fn as_vararg(&self) -> Option<(&[u16], &TypeObject)> {
        match self {
            ArgPosition::Vararg(x, y) => Some((x, y)),
            _ => None,
        }
    }
}

impl DefaultValue {
    pub fn new(parent_type: TypeObject) -> Self {
        Self {
            parent_type,
            value: OnceCell::new(),
            bytes_index: OnceCell::new(),
        }
    }

    pub fn compile(&self, info: &mut CompilerInfo, node: &TestNode) -> CompileResult<()> {
        self.value.get_or_try_init(|| {
            let mut converter = node.test_conv_expected(1, slice::from_ref(&self.parent_type));
            let return_type = first(converter.return_type(info)?);
            if !self.parent_type.is_superclass(&return_type) {
                if OptionTypeObject::needs_make_option(&self.parent_type, &return_type) {
                    let constant = converter
                        .constant_return(info)?
                        .map(|x| OptionConstant::new(x).into());
                    Ok((
                        OptionTypeObject::wrap_bytes(converter.convert(info)?),
                        constant,
                    ))
                } else {
                    Err(CompilerException::of(
                        format!(
                            "Default value of argument with type '{}' is of type '{}'",
                            self.parent_type.name(),
                            return_type.name()
                        ),
                        node,
                    )
                    .into())
                }
            } else {
                let constant = converter.constant_return(info)?;
                converter.convert(info).map(|x| (x, constant))
            }
        })?;
        Ok(())
    }

    pub fn load_bytes(&self, bytes: &mut BytecodeList, info: &mut CompilerInfo) {
        match self.value.get() {
            Option::Some((_, Option::Some(constant))) => {
                bytes.add(Bytecode::LoadConst(constant.clone().into()));
            }
            Option::Some((byte_fn, None)) => {
                let function = self.save_function(byte_fn, info);
                bytes.add(Bytecode::CallFn(function.into(), 0.into()));
            }
            Option::None => panic!("Called DefaultInfo::load_bytes without function set"),
        }
    }

    fn save_function(&self, byte_fn: &BytecodeList, info: &mut CompilerInfo) -> u16 {
        *self.bytes_index.get_or_init(|| {
            let mut byte_fn = byte_fn.clone();
            byte_fn.add(Bytecode::Return(1.into()));
            let fn_info = FunctionInfo::default(); // FIXME: Get return type
            info.add_function(Function::new(LineInfo::empty(), fn_info, byte_fn))
        })
    }
}

fn update(
    i: u16,
    par: &TypeObject,
    result: &mut HashMap<u16, TypeObject>,
    needs_make_option: &mut HashSet<u16>,
    arg: &Argument,
    passed_type: &TypeObject,
) -> bool {
    if let Option::Some(arg_generics) = arg.type_val.generify_as(par, passed_type) {
        TypeObject::add_generics_to_map(arg_generics, result)
    } else {
        if OptionTypeObject::needs_and_super(&arg.type_val, passed_type) {
            needs_make_option.insert(i);
            return true;
        }
        let option_generics = arg
            .get_type()
            .generify_as(par, &TypeObject::optional(passed_type.clone()));
        if option_generics.is_none()
            || !TypeObject::add_generics_to_map(option_generics.unwrap(), result)
        {
            false
        } else {
            needs_make_option.insert(i);
            true
        }
    }
}

fn find_name<'a, T>(name: &str, args: &'a [T]) -> CompileResult<&'a Argument>
where
    T: Deref<Target = Argument>,
{
    args.iter()
        .find(|x| x.name == name)
        .map(|x| &**x)
        .ok_or_else(|| {
            CompilerInternalError::of(format!("Unknown name {name}"), LineInfo::empty()).into()
        })
}

fn get_kw_positions(args: &[Argument]) -> HashMap<&str, usize> {
    args.iter()
        .enumerate()
        .map(|(i, arg)| (&*arg.name, i))
        .filter(|(name, _)| !name.is_empty())
        .collect()
}

// FIXME: Reborrow Option<&mut T>
fn get_args<'a>(
    info: &mut CompilerInfo,
    args: &'a [TypedArgumentNode],
    defaults: &mut Option<&mut DefaultHolder<'a>>,
) -> CompileResult<Vec<Argument>> {
    args.iter()
        .map(|arg| {
            if FORBIDDEN_NAMES.contains(&arg.get_name().get_name()) {
                Err(CompilerException::of(
                    format!("Illegal name for variable '{}'", arg.get_name().get_name()),
                    arg.get_name(),
                )
                .into())
            } else if arg.get_default_val().is_empty() {
                Ok(Argument::new_full(
                    arg.get_name().get_name().to_string(),
                    info.convert_type(arg.get_type().as_type())?,
                    !arg.get_vararg().is_empty(),
                    arg.line_info().clone(),
                ))
            } else {
                let default_holder = match defaults {
                    Option::Some(holder) => holder,
                    Option::None => {
                        return Err(CompilerTodoError::of(
                            "Default arguments in non-statically-defined functions",
                            arg,
                        )
                        .into())
                    }
                };
                let argument = Argument::new_default(
                    arg.get_name().get_name().to_string(),
                    info.convert_type(arg.get_type().as_type())?,
                    !arg.get_vararg().is_empty(),
                    arg.line_info().clone(),
                    arg.get_default_val(),
                    default_holder,
                );
                Ok(argument)
            }
        })
        .collect()
}

impl Lined for Argument {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Index<usize> for ArgumentInfo {
    type Output = Argument;

    fn index(&self, index: usize) -> &Self::Output {
        match self.get(index) {
            Option::Some(x) => x,
            Option::None => panic!(
                "Index {} out of bounds for ArgumentInfo of length {}",
                index,
                self.len()
            ),
        }
    }
}

impl Display for ArgumentInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        if !self.position_args.is_empty() {
            write!(f, "{}", self.position_args.iter().format(", "))?;
            f.write_str(", /")?;
            first = false;
        }
        if !self.normal_args.is_empty() {
            if !first {
                f.write_str(", ")?;
            }
            write!(f, "{}", self.normal_args.iter().format(", "))?;
            first = false;
        }
        if !self.keyword_args.is_empty() {
            if !first {
                f.write_str(", *,")?;
            }
            write!(f, "{}", self.keyword_args.iter().format(", "))?
        }
        Ok(())
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.get_name().is_empty() {
            if self.is_vararg() {
                write!(f, "*{}", self.get_type().name())
            } else {
                f.write_str(&self.get_type().name())
            }
        } else if self.is_vararg() {
            write!(f, "*{} {}", self.get_type().name(), self.get_name())
        } else {
            write!(f, "{} {}", self.get_type().name(), self.get_name())
        }
    }
}

impl Default for ArgumentInfo {
    fn default() -> Self {
        Self::empty()
    }
}
