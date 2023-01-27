use std::cmp::Ordering;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::iter::zip;

use derive_new::new;

use crate::parser::type_node::TypeNode;

use super::builtins::OBJECT;
use super::compiler_info::CompilerInfo;
use super::type_obj::{TemplateParam, TypeObject};
use super::CompileResult;

#[derive(Debug, new)]
pub struct GenericInfo {
    params: Vec<TemplateParam>,
}

#[derive(Debug)]
pub struct GenerifyError {
    ty: GenerifyErrorType,
}

#[derive(Debug)]
enum GenerifyErrorType {
    UnequalLength(usize, usize),
    TooFewArgs(usize, usize),
    MismatchedList(TemplateParam, TypeObject),
    MismatchedNonList(TemplateParam, TypeObject),
    FailedBound(TemplateParam, TypeObject),
}

impl GenericInfo {
    pub const fn empty() -> Self {
        Self { params: Vec::new() }
    }

    pub fn parse(info: &mut CompilerInfo, generics: &[TypeNode]) -> CompileResult<GenericInfo> {
        if generics.is_empty() {
            return Ok(Self::empty());
        }
        let mut params = Vec::with_capacity(generics.len());
        for (i, generic) in generics.iter().enumerate() {
            let param = if generic.is_vararg() {
                TemplateParam::new_vararg(generic.str_name().to_string(), i)
            } else if generic.get_name().is_empty() {
                // [*T]
                assert!(generic.get_subtypes().len() == 1 && generic.get_subtypes()[0].is_vararg());
                let subtype = &generic.get_subtypes()[0];
                TemplateParam::new(subtype.str_name().to_string(), i, TypeObject::list([]))
            } else {
                let bound = if generic.get_subtypes().len() == 1 {
                    info.convert_type(&generic.get_subtypes()[0])?
                } else {
                    OBJECT.into()
                };
                TemplateParam::new(generic.str_name().to_string(), i, bound)
            };
            params.push(param)
        }
        Ok(GenericInfo::new(params))
    }

    pub fn parse_no_types(generics: &[TypeNode]) -> CompileResult<GenericInfo> {
        if generics.is_empty() {
            return Ok(Self::empty());
        }
        let mut params = Vec::with_capacity(generics.len());
        for (i, generic) in generics.iter().enumerate() {
            let param = if generic.is_vararg() {
                TemplateParam::new_vararg(generic.str_name().to_string(), i)
            } else if generic.get_name().is_empty() {
                // [*T]
                assert!(generic.get_subtypes().len() == 1 && generic.get_subtypes()[0].is_vararg());
                let subtype = &generic.get_subtypes()[0];
                TemplateParam::new(subtype.str_name().to_string(), i, TypeObject::list([]))
            } else if generic.get_subtypes().is_empty() {
                TemplateParam::new_unbounded(generic.str_name().to_string(), i)
            } else {
                // FIXME: Default interfaces are done before types are
                // registered, so this doesn't work yet
                TemplateParam::new_unknown_bound(generic.str_name().to_string(), i)
            };
            params.push(param)
        }
        Ok(GenericInfo::new(params))
    }

    pub fn re_parse(&self, info: &mut CompilerInfo, generics: &[TypeNode]) -> CompileResult<()> {
        let gen_info = Self::parse(info, generics)?;
        assert_eq!(self.params.len(), gen_info.params.len());
        for (param, new_param) in zip(&self.params, gen_info.params) {
            assert_eq!(param.name(), new_param.name());
            if !param.is_bounded() {
                param.set_bound(new_param.get_bound().clone());
            } else {
                assert_eq!(
                    param.get_bound(),
                    new_param.get_bound(),
                    "{} != {}",
                    param.get_bound().name(),
                    new_param.get_bound().name()
                );
            }
        }
        Ok(())
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    pub fn get_param_map(&self) -> HashMap<String, TypeObject> {
        self.params
            .iter()
            .map(|x| (x.base_name().to_string(), x.clone().into()))
            .collect()
    }

    pub fn set_parent(&self, parent: TypeObject) {
        for param in &self.params {
            param.set_parent(parent.clone())
        }
    }

    pub fn generify(&self, args: &[TypeObject]) -> Result<Vec<TypeObject>, GenerifyError> {
        if !self.params.iter().any(|x| x.is_vararg()) {
            self.generify_no_varargs(args)
        } else {
            self.generify_vararg(args)
        }
    }

    fn generify_no_varargs(&self, args: &[TypeObject]) -> Result<Vec<TypeObject>, GenerifyError> {
        if args.len() != self.len() {
            return Err(GenerifyError::new(GenerifyErrorType::UnequalLength(
                self.len(),
                args.len(),
            )));
        }
        zip(args, &self.params)
            .map(|(arg, param)| {
                check_arg(arg, param)?;
                Ok(arg.clone())
            })
            .collect()
    }

    fn generify_vararg(&self, args: &[TypeObject]) -> Result<Vec<TypeObject>, GenerifyError> {
        if args.len() < self.len() - 1 {
            return Err(GenerifyError::new(GenerifyErrorType::TooFewArgs(
                self.len(),
                args.len(),
            )));
        }
        let mut result = Vec::with_capacity(self.len());
        let mut i = 0;
        while i < args.len() && !self.params[i].is_vararg() {
            check_arg(&args[i], &self.params[i])?;
            result.push(args[i].clone());
            i += 1;
        }
        let remaining_args = args.len() - i;
        let remaining_params = self.len() - i;
        match remaining_args.cmp(&remaining_params) {
            Ordering::Less => {
                // Number of types resulted in empty vararg, proceed accordingly
                result.push(TypeObject::list([]));
                while i < args.len() {
                    check_arg(&args[i], &self.params[i])?;
                    result.push(args[i].clone());
                    i += 1;
                }
            }
            Ordering::Equal => {
                // Exactly one parameter goes into the vararg
                result.push(TypeObject::list([args[i].clone()]));
                i += 1;
                while i < args.len() {
                    check_arg(&args[i], &self.params[i])?;
                    result.push(args[i].clone());
                    i += 1;
                }
            }
            Ordering::Greater => {
                let diff = remaining_args - remaining_params;
                result.push(TypeObject::list_of(args[i..diff + i + 1].to_vec()));
                i += 1;
                while i < self.len() {
                    check_arg(&args[i + diff], &self.params[i])?;
                    result.push(args[i + diff].clone());
                    i += 1;
                }
            }
        }
        Ok(result)
    }
}

fn check_arg(arg: &TypeObject, param: &TemplateParam) -> Result<(), GenerifyError> {
    let is_list = matches!(param.get_bound(), TypeObject::List(_));
    let arg_is_list = matches!(arg, TypeObject::List(_));
    if is_list && !arg_is_list {
        Err(GenerifyError::new(GenerifyErrorType::MismatchedList(
            param.clone(),
            arg.clone(),
        )))
    } else if !arg_is_list && is_list {
        Err(GenerifyError::new(GenerifyErrorType::MismatchedNonList(
            param.clone(),
            arg.clone(),
        )))
    } else if !is_list && !param.get_bound().is_superclass(arg) {
        // FIXME? param.get_bound() != null
        Err(GenerifyError::new(GenerifyErrorType::FailedBound(
            param.clone(),
            arg.clone(),
        )))
    } else {
        Ok(())
    }
}

impl GenerifyError {
    fn new(ty: GenerifyErrorType) -> Self {
        Self { ty }
    }
}

impl Default for GenericInfo {
    fn default() -> Self {
        Self::empty()
    }
}

impl Error for GenerifyError {}

impl Display for GenerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ty.fmt(f)
    }
}

impl Display for GenerifyErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenerifyErrorType::UnequalLength(len1, len2) => write!(
                f,
                "Type parameter list expected {len1} types, but got {len2}",
            ),
            GenerifyErrorType::TooFewArgs(len1, len2) => write!(
                f,
                "Type parameter expected no fewer than {len1} types, but got {len2}",
            ),
            GenerifyErrorType::MismatchedList(bound, value) => write!(
                f,
                "Type parameter {} has a bound of type {} (a list type), \
                 but {} was given (not a list type)",
                bound.name(),
                bound.get_bound().name(),
                value.name(),
            ),
            GenerifyErrorType::MismatchedNonList(bound, value) => write!(
                f,
                "Type parameter {} has a bound of type {} (not a list type), \
                 but {} was given (a list type)",
                bound.name(),
                bound.get_bound().name(),
                value.name(),
            ),
            GenerifyErrorType::FailedBound(bound, value) => write!(
                f,
                "Type parameter {} has bound of type {}, but {} was given",
                bound.name(),
                bound.get_bound().name(),
                value.name()
            ),
        }
    }
}

impl Error for GenerifyErrorType {}
