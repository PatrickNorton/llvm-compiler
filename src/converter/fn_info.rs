use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::parser::line_info::{LineInfo, Lined};

use super::argument::{Argument, ArgumentInfo};
use super::generic::GenericInfo;
use super::type_obj::{FunctionInfoType, TypeObject};
use super::CompileResult;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    value: Arc<FnInfoInner>,
}

#[derive(Debug)]
struct FnInfoInner {
    line_info: LineInfo,
    name: String,
    is_generator: bool,
    arguments: ArgumentInfo,
    returns: Vec<TypeObject>,
    generics: GenericInfo,
    is_deprecated: AtomicBool,
    must_use: OnceCell<String>, // NOTE: An empty string here means $mustUse, but no message
}

impl FunctionInfo {
    pub fn new(
        line_info: LineInfo,
        name: String,
        is_generator: bool,
        generics: GenericInfo,
        arguments: ArgumentInfo,
        returns: Vec<TypeObject>,
    ) -> Self {
        Self {
            value: Arc::new(FnInfoInner {
                line_info,
                name,
                is_generator,
                arguments,
                returns,
                generics,
                is_deprecated: AtomicBool::new(false),
                must_use: OnceCell::new(),
            }),
        }
    }

    pub fn from_returns(returns: Vec<TypeObject>) -> Self {
        Self::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            ArgumentInfo::empty(),
            returns,
        )
    }

    pub fn with_args(args: ArgumentInfo, returns: Vec<TypeObject>) -> Self {
        Self::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            args,
            returns,
        )
    }

    pub fn named(name: impl ToString) -> Self {
        Self::new(
            LineInfo::empty(),
            name.to_string(),
            false,
            GenericInfo::empty(),
            ArgumentInfo::empty(),
            Vec::new(),
        )
    }

    pub fn named_with_args(
        name: impl ToString,
        args: ArgumentInfo,
        returns: Vec<TypeObject>,
    ) -> Self {
        Self::new(
            LineInfo::empty(),
            name.to_string(),
            false,
            GenericInfo::empty(),
            args,
            returns,
        )
    }

    pub fn matches(&self, args: &[Argument]) -> bool {
        self.value.arguments.matches(self, args)
    }

    pub fn boundify(&self) -> FunctionInfo {
        let callable = self.to_callable();
        let pos_args = boundify_array(&callable, self.get_args().get_position_args());
        let normal_args = boundify_array(&callable, self.get_args().get_normal_args());
        let kw_args = boundify_array(&callable, self.get_args().get_keyword_args());
        let arg_info = ArgumentInfo::new(pos_args, normal_args, kw_args);
        FunctionInfo::new(
            self.value.line_info.clone(),
            self.value.name.clone(),
            self.value.is_generator,
            GenericInfo::empty(),
            arg_info,
            self.value.returns.to_vec(),
        )
    }

    pub fn get_args(&self) -> &ArgumentInfo {
        &self.value.arguments
    }

    pub fn get_returns(&self) -> &[TypeObject] {
        &self.value.returns
    }

    pub fn get_name(&self) -> &str {
        &self.value.name
    }

    pub fn set_generic_parent(&self) {
        self.value.generics.set_parent(self.to_callable());
    }

    pub fn is_generator(&self) -> bool {
        self.value.is_generator
    }

    pub fn get_generics(&self) -> &GenericInfo {
        &self.value.generics
    }

    pub fn to_callable(&self) -> TypeObject {
        FunctionInfoType::new(self.clone()).into()
    }

    pub fn is_deprecated(&self) -> bool {
        self.value.is_deprecated.load(Ordering::Relaxed)
    }

    pub fn must_use(&self) -> bool {
        self.value.must_use.get().is_some()
    }

    pub fn get_must_use_message(&self) -> &str {
        self.value.must_use.get().unwrap()
    }

    pub fn set_must_use(&self, message: String) {
        self.value
            .must_use
            .set(message)
            .expect("Cannot set message multiple times");
    }

    pub fn set_deprecated(&self, deprecated: bool) {
        self.value
            .is_deprecated
            .store(deprecated, Ordering::Relaxed)
    }

    pub fn generify_args(
        &self,
        args: &[Argument],
    ) -> CompileResult<(HashMap<u16, TypeObject>, HashSet<u16>)> {
        self.value.arguments.generify_args(self, args)
    }

    pub fn generify(&self, parent: &TypeObject, args: Vec<TypeObject>) -> FunctionInfo {
        let pos_args = generify_array(parent, self.get_args().get_position_args(), &args);
        let norm_args = generify_array(parent, self.get_args().get_normal_args(), &args);
        let kw_args = generify_array(parent, self.get_args().get_keyword_args(), &args);
        let arg_info = ArgumentInfo::new(pos_args, norm_args, kw_args);
        FunctionInfo::new(
            self.line_info().clone(),
            self.get_name().to_string(),
            self.is_generator(),
            GenericInfo::empty(),
            arg_info,
            generify_type_array(parent, self.get_returns(), &args),
        )
    }
}

fn generify_array(parent: &TypeObject, arr: &[Argument], generics: &[TypeObject]) -> Vec<Argument> {
    arr.iter()
        .map(|arg| {
            let arg_type = arg.get_type();
            let ty = generify_type(arg_type, parent, generics);
            arg.clone_with_type(ty)
        })
        .collect()
}

fn generify_type_array(
    parent: &TypeObject,
    arr: &[TypeObject],
    generics: &[TypeObject],
) -> Vec<TypeObject> {
    arr.iter()
        .map(|arg| generify_type(arg, parent, generics))
        .collect()
}

fn generify_type(val: &TypeObject, parent: &TypeObject, generics: &[TypeObject]) -> TypeObject {
    if let TypeObject::Template(template) = val {
        if template.get_parent().same_base_type(parent) {
            generics[template.get_index()].clone()
        } else {
            template.clone().into()
        }
    } else {
        val.generify_with(parent, generics.to_vec())
    }
}

fn boundify_array(callable: &TypeObject, args: &[Argument]) -> Vec<Argument> {
    args.iter()
        .map(|x| x.clone_with_type(boundify_type(callable, x.get_type()).clone()))
        .collect()
}

fn boundify_type<'a>(callable: &TypeObject, ty: &'a TypeObject) -> &'a TypeObject {
    if let TypeObject::Template(template) = ty {
        if template.get_parent().same_base_type(callable) {
            template.get_bound()
        } else {
            ty
        }
    } else {
        ty
    }
}

impl From<ArgumentInfo> for FunctionInfo {
    fn from(args: ArgumentInfo) -> Self {
        Self::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            args,
            Vec::new(),
        )
    }
}

impl Lined for FunctionInfo {
    fn line_info(&self) -> &LineInfo {
        &self.value.line_info
    }
}

impl PartialEq for FunctionInfo {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}

impl Eq for FunctionInfo {}

impl Hash for FunctionInfo {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(Arc::as_ptr(&self.value), state)
    }
}

impl Default for FunctionInfo {
    fn default() -> Self {
        Self::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            ArgumentInfo::empty(),
            Vec::new(),
        )
    }
}
