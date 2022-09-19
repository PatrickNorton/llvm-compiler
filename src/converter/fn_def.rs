use derive_new::new;
use std::collections::HashMap;

use crate::parser::annotation::AnnotatableRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::generalizable::Generalizable;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::typed_arg::TypedArgumentNode;

use super::annotation::{impl_annotatable, should_compile, AnnotatableConverter};
use super::argument::{Argument, ArgumentInfo};
use super::base_converter::BaseConverter;
use super::builtins::OBJECT;
use super::bytecode::Bytecode;
use super::bytecode_list::{bytecode_list, BytecodeList};
use super::compiler_info::CompilerInfo;
use super::constant::FunctionConstant;
use super::convertible::{base_convertible, BaseConvertible};
use super::default_holder::DefaultHolder;
use super::diverge::DivergingInfo;
use super::error::{CompilerException, CompilerInternalError};
use super::fn_info::FunctionInfo;
use super::function::Function;
use super::generic::GenericInfo;
use super::global_info::FunctionRef;
use super::mutable::MutableType;
use super::syscalls::get_syscall;
use super::type_obj::{FunctionInfoType, TemplateParam, TypeObject};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct FunctionDefConverter<'a> {
    node: &'a FunctionDefinitionNode,
}

impl<'a> AnnotatableConverter<'a> for FunctionDefConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::Function(self.node)
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.convert_inner(info)?;
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

impl_annotatable!(FunctionDefConverter<'a>);

impl<'a> FunctionDefConverter<'a> {
    pub fn convert_deprecated(&self, info: &mut CompilerInfo) -> CompileBytes {
        let func = self.convert_inner(info)?;
        func.get_info().set_deprecated(true);
        Ok(BytecodeList::new())
    }

    pub fn convert_must_use(&self, info: &mut CompilerInfo, message: String) -> CompileBytes {
        let func = self.convert_inner(info)?;
        if func.get_info().get_returns().is_empty() {
            return Err(CompilerException::of(
                "$mustUse annotation requires function to return a value",
                self.node,
            )
            .into());
        }
        func.get_info().set_must_use(message);
        Ok(BytecodeList::new())
    }

    pub fn convert_sys(&self, info: &mut CompilerInfo) -> CompileBytes {
        if !info.permissions().is_stdlib() {
            return Err(CompilerException::of(
                r#"$native("sys") is only allowed in stdlib files"#,
                self.node,
            )
            .into());
        }
        assert!(self.node.get_body().is_empty());
        let name = self.node.get_name().get_name();
        if let Option::Some(mut func) = info.get_fn(name) {
            let argc = func.get_info().get_args().len();
            let returns = func.get_info().get_returns();
            let bytes = if returns.is_empty() {
                BytecodeList::of(Bytecode::Syscall(
                    get_syscall(name).into(),
                    (argc as u16).into(),
                ))
            } else {
                bytecode_list!(
                    Bytecode::Syscall(get_syscall(name).into(), (argc as u16).into()),
                    Bytecode::Return((returns.len() as u16).into())
                )
            };
            func.set_bytes(bytes);
            Ok(BytecodeList::new())
        } else {
            Err(
                CompilerInternalError::of("System function should always be predefined", self.node)
                    .into(),
            )
        }
    }

    pub fn parse_header(
        &self,
        info: &mut CompilerInfo,
        is_builtin: bool,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<Option<(TypeObject, u16)>> {
        if !should_compile(self.node, info, self.node.get_annotations())? {
            return Ok(None);
        }
        if self.node.get_generics().is_empty() {
            self.inner_header(info, GenericInfo::empty(), is_builtin, defaults)
                .map(|(x, y)| Some((x.into(), y)))
        } else {
            let generics = GenericInfo::parse(info, self.node.get_generics())?;
            // FIXME: Should be None
            info.add_local_types(OBJECT.into(), generics.get_param_map());
            let result = self.inner_header(info, generics, is_builtin, defaults)?;
            info.remove_local_types();
            result.0.set_generic_parent();
            Ok(Some((result.0.into(), result.1)))
        }
    }

    fn inner_header(
        &self,
        info: &mut CompilerInfo,
        generic_info: GenericInfo,
        is_builtin: bool,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<(FunctionInfoType, u16)> {
        let arg_info = ArgumentInfo::of(self.node.get_args(), info, Some(defaults))?;
        let returns = info.types_of(self.node.get_ret_val())?;
        let is_generator = self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Generator);
        let true_ret = if is_generator {
            vec![iterator_type(info, returns)?]
        } else {
            returns
        };
        let fn_info = FunctionInfo::new(
            self.node.line_info().clone(),
            self.node.get_name().get_name().to_string(),
            is_generator,
            generic_info,
            arg_info,
            true_ret,
        );
        let func = Function::new(
            self.node.line_info().clone(),
            fn_info.clone(),
            BytecodeList::new(),
        );
        if let Option::Some(def) = info.get_fn(func.get_name()) {
            return Err(
                CompilerException::double_def(func.get_name(), def.line_info(), self.node).into(),
            );
        }
        let index = if is_builtin {
            u16::MAX
        } else {
            info.add_function(func)
        };
        Ok((FunctionInfoType::new(fn_info), index))
    }

    pub fn convert_with_constant(
        info: &mut CompilerInfo,
        node: &FunctionDefinitionNode,
    ) -> CompileResult<FunctionConstant> {
        let converter = node.base_converter();
        let name = node.get_name().get_name();
        if let Option::Some(predefined) = info.get_fn(name).map(|x| x.get_info().clone()) {
            converter.convert_predefined(info, &predefined, name)
        } else {
            converter.convert_undefined(info, name).map(|x| x.1)
        }
    }

    fn convert_inner<'b>(&self, info: &'b mut CompilerInfo) -> CompileResult<FunctionRef<'b>> {
        let name = self.node.get_name().get_name();
        if let Option::Some(predef_info) = info.get_fn(name).map(|x| x.get_info().clone()) {
            self.convert_predefined(info, &predef_info, name)?;
            Ok(info.get_fn(name).unwrap())
        } else {
            self.convert_undefined(info, name).map(|x| x.0)
        }
    }

    fn convert_predefined(
        &self,
        info: &mut CompilerInfo,
        fn_info: &FunctionInfo,
        name: &str,
    ) -> CompileResult<FunctionConstant> {
        let generics = fn_info.get_generics().get_param_map();
        let index = info.fn_index(name).unwrap();
        let const_val = FunctionConstant::new(name.into(), index);
        let fn_rets = fn_info.get_returns();
        let is_gen = fn_info.is_generator();
        let true_ret = if is_gen {
            info.builtins().de_iterable(&fn_rets[0])?
        } else {
            fn_rets.to_vec()
        };
        info.check_definition(name, self.node)?;
        let var_holder = info.var_holder_mut();
        var_holder.add_constant_variable(
            name.to_string(),
            fn_info.to_callable(),
            const_val.clone().into(),
            self.node.line_info().clone(),
        );
        var_holder.add_stack_frame();
        self.check_gen()?;
        var_holder.add_local_types(Some(fn_info.to_callable()), generics);
        let mut bytes = BytecodeList::new();
        self.convert_body(info, &mut bytes, is_gen, true_ret)?;
        let var_holder = info.var_holder_mut();
        var_holder.remove_local_types();
        var_holder.remove_stack_frame();
        let max_size = var_holder.reset_max();
        let mut fn_info = info.get_fn(name).unwrap();
        fn_info.set_max(max_size);
        fn_info.set_bytes(bytes);
        Ok(const_val)
    }

    fn convert_undefined<'b>(
        &self,
        info: &'b mut CompilerInfo,
        name: &str,
    ) -> CompileResult<(FunctionRef<'b>, FunctionConstant)> {
        let generics = self.get_generics(info)?;
        let ret_types = info.types_of(self.node.get_ret_val())?;
        let is_generator = self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Generator);
        let true_ret = if is_generator {
            vec![iterator_type(info, ret_types)?]
        } else {
            ret_types
        };
        let converted_args = self.convert_args(info, &generics)?;
        let fn_info = FunctionInfo::new(
            self.node.line_info().clone(),
            name.to_string(),
            is_generator,
            GenericInfo::empty(),
            converted_args,
            true_ret.clone(),
        );
        let mut bytes = BytecodeList::new();
        let func = Function::new(
            self.node.line_info().clone(),
            fn_info.clone(),
            BytecodeList::new(),
        );
        let index = info.add_function(func);
        for generic in generics.values() {
            generic.set_parent(fn_info.to_callable());
        }
        let const_val = FunctionConstant::new(name.into(), index);
        info.check_definition(name, self.node)?;
        let var_holder = info.var_holder_mut();
        var_holder.add_constant_variable(
            name.to_string(),
            fn_info.to_callable(),
            const_val.clone().into(),
            self.node.line_info().clone(),
        );
        var_holder.add_stack_frame();
        self.check_gen()?;
        var_holder.add_local_types(
            Some(fn_info.to_callable()),
            generics.into_iter().map(|(x, y)| (x, y.into())).collect(),
        );
        self.convert_body(info, &mut bytes, is_generator, true_ret)?;
        let var_holder = info.var_holder_mut();
        var_holder.remove_local_types();
        var_holder.remove_stack_frame();
        let mut func = info.get_fn(name).unwrap();
        func.set_bytes(bytes);
        Ok((func, const_val))
    }

    fn convert_body(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        is_gen: bool,
        ret_types: Vec<TypeObject>,
    ) -> CompileResult<()> {
        let has_returns = !ret_types.is_empty();
        info.fn_returns_mut().add_fn_returns(
            is_gen,
            ret_types,
            self.node.line_info().clone(),
            Some(self.node.get_name().get_name().to_string()),
        );
        self.add_args(info)?;
        let (bytecode, diverging_info) =
            BaseConverter::bytes_with_return(self.node.get_body(), info)?;
        bytes.extend(bytecode);
        if !is_gen && has_returns && !diverging_info.will_return() {
            return Err(CompilerException::of("Function ends without returning", self.node).into());
        }
        info.fn_returns_mut().pop_fn_returns();
        Ok(())
    }

    fn add_args(&self, info: &mut CompilerInfo) -> CompileResult<()> {
        for arg in self.node.get_args() {
            let ty = info.convert_type(arg.get_type().as_type())?;
            let arg_name = arg.get_name().get_name().to_string();
            if !arg.get_vararg().is_empty() {
                info.add_variable(
                    arg_name,
                    info.builtins().iterable().generify(arg, vec![ty])?,
                    true,
                    arg.line_info().clone(),
                );
            } else {
                info.add_variable(arg_name, ty, true, arg.line_info().clone());
            }
        }
        Ok(())
    }

    fn convert_args(
        &self,
        info: &mut CompilerInfo,
        generics: &HashMap<String, TemplateParam>,
    ) -> CompileResult<ArgumentInfo> {
        let args = self.node.get_args();
        let kw_args = Self::convert_generics(info, generics, args.get_name_args())?;
        let normal_args = Self::convert_generics(info, generics, args.get_args())?;
        let pos_args = Self::convert_generics(info, generics, args.get_pos_args())?;
        Ok(ArgumentInfo::new(kw_args, normal_args, pos_args))
    }

    fn convert_generics(
        info: &mut CompilerInfo,
        generics: &HashMap<String, TemplateParam>,
        args: &[TypedArgumentNode],
    ) -> CompileResult<Vec<Argument>> {
        // FIXME: Deduplicate with ArgumentInfo.getArgs
        args.iter()
            .map(|arg| {
                let arg_type = arg.get_type().as_type();
                // TODO: Remove clones here
                let ty = match generics.get(arg_type.str_name()) {
                    Option::Some(t) => t.clone().into(),
                    Option::None => info.convert_type(arg_type)?,
                };
                let mutability = MutableType::from_nullable(arg_type.get_mutability());
                let proper_type = if mutability.is_const_type() {
                    ty.make_const()
                } else {
                    ty.make_mut()
                };
                if arg.get_default_val().is_empty() {
                    Ok(Argument::new_full(
                        arg.get_name().get_name().to_string(),
                        proper_type,
                        !arg.get_vararg().is_empty(),
                        arg.line_info().clone(),
                    ))
                } else {
                    // let argument = Argument::new_default(
                    //     arg.get_name().get_name().to_string(),
                    //     proper_type,
                    //     !arg.get_vararg().is_empty(),
                    //     arg.line_info().clone(),
                    //     arg.get_default_val(),
                    // );
                    // info.add_default_argument(argument)
                    // Ok(argument)
                    todo!("Default arguments for non-predefined functions")
                }
            })
            .collect()
    }

    fn check_gen(&self) -> CompileResult<()> {
        if self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Generator)
            && self.node.get_ret_val().is_empty()
        {
            Err(CompilerException::of(
                "Generator functions must have at least one return",
                self.node,
            )
            .into())
        } else {
            Ok(())
        }
    }

    fn get_generics(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<HashMap<String, TemplateParam>> {
        TemplateParam::parse_generics(info, self.node.get_generics())
    }
}

fn iterator_type(info: &CompilerInfo, params: Vec<TypeObject>) -> CompileResult<TypeObject> {
    info.builtins()
        .iterator()
        .make_mut()
        .generify(&LineInfo::empty(), params)
}

base_convertible!(FunctionDefinitionNode, FunctionDefConverter);
