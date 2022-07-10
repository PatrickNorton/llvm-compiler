use derive_new::new;

use crate::parser::lambda::LambdaNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::typed_arg::TypedArgumentNode;

use super::argument::{Argument, ArgumentInfo};
use super::base_converter::BaseConverter;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible_expected, ConverterBase, ConverterTest};
use super::error::{CompilerException, CompilerTodoError};
use super::fn_info::FunctionInfo;
use super::function::Function;
use super::generic::GenericInfo;
use super::test_converter::TestConverter;
use super::type_obj::{ListTypeObject, TypeObject};
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct LambdaConverter<'a> {
    node: &'a LambdaNode,
    ret_count: u16,
    expected_returns: Option<&'a [TypeObject]>,
}

impl<'a> ConverterTest for LambdaConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![FunctionInfo::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            self.convert_args(info)?,
            self.lambda_return_type(info)?,
        )
        .to_callable()])
    }
}

impl<'a> ConverterBase for LambdaConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            warning::warn("Unused lambda", WarningType::Unused, info, self.node)?;
            Ok(BytecodeList::new())
        } else if self.ret_count > 1 {
            Err(CompilerException::of(
                format!(
                    "Lambda literal only returns one value, not {}",
                    self.ret_count
                ),
                self.node,
            )
            .into())
        } else {
            let name = info.lambda_name();
            let fn_info = FunctionInfo::new(
                self.node.line_info().clone(),
                name,
                false,
                GenericInfo::empty(),
                self.convert_args(info)?,
                self.lambda_return_type(info)?,
            );
            let body = self.convert_body(info)?;
            let fn_index =
                info.add_function(Function::new(self.node.line_info().clone(), fn_info, body));
            Ok(BytecodeList::of(Bytecode::MakeFunction(fn_index.into())))
        }
    }
}

impl<'a> LambdaConverter<'a> {
    fn convert_body(&self, info: &mut CompilerInfo) -> CompileBytes {
        info.add_stack_frame();
        let lambda_ret = self.lambda_return_type(info)?;
        info.fn_returns_mut().add_fn_returns(false, lambda_ret);
        for arg in self.node.get_args() {
            let arg_type = info.convert_type(arg.get_type().as_type())?;
            info.add_variable(
                arg.get_name().get_name().to_string(),
                arg_type,
                false,
                arg.line_info().clone(),
            );
        }
        let fn_bytes = self.fn_body(info);
        info.remove_stack_frame();
        info.fn_returns_mut().pop_fn_returns();
        fn_bytes
    }

    fn fn_body(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.node.is_arrow() {
            todo!("Cannot convert single-statement return yet")
        } else {
            BaseConverter::bytes(self.node.get_body(), info)
        }
    }

    fn lambda_return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        if self.node.is_arrow() && self.node.get_returns().is_empty() {
            info.add_stack_frame();
            for arg in self.node.get_args() {
                let arg_type = info.convert_type(arg.get_type().as_type())?;
                info.add_variable(
                    arg.get_name().get_name().to_string(),
                    arg_type,
                    false,
                    arg.line_info().clone(),
                );
            }
            let body: &TestNode = (&self.node.get_body()[0]).try_into().unwrap();
            let ret_type = TestConverter::return_type(info, body, 1)?;
            info.remove_stack_frame();
            Ok(ret_type)
        } else {
            info.types_of(self.node.get_returns())
        }
    }

    fn convert_args(&self, info: &mut CompilerInfo) -> CompileResult<ArgumentInfo> {
        let args = self.node.get_args();
        let name_args = self.get_args(info, args.get_pos_args())?;
        let normal_args = self.get_args(info, args.get_args())?;
        let kwargs = self.get_args(info, args.get_name_args())?;
        Ok(ArgumentInfo::new(name_args, normal_args, kwargs))
    }

    fn get_args(
        &self,
        info: &mut CompilerInfo,
        args: &[TypedArgumentNode],
    ) -> CompileResult<Vec<Argument>> {
        let expected = self.expected_returns.as_ref().and_then(|x| x.first());
        args.iter()
            .enumerate()
            .map(|(i, arg)| {
                let arg_type = arg.get_type();
                if let TypeLikeNode::Type(ty) = arg_type {
                    let parsed_ty = info.convert_type(ty)?;
                    Ok(Argument::new_full(
                        arg.get_name().get_name().to_string(),
                        parsed_ty,
                        !arg.get_vararg().is_empty(),
                        arg.line_info().clone(),
                    ))
                } else if let Option::Some(expected) = expected {
                    if expected.same_base_type(info.builtins().callable()) {
                        let generics = expected.get_generics();
                        assert_eq!(generics.len(), 2);
                        let expected_args = <&ListTypeObject>::try_from(&generics[0]).unwrap();
                        Ok(Argument::new_full(
                            arg.get_name().get_name().to_string(),
                            expected_args[i].clone(),
                            !arg.get_vararg().is_empty(),
                            arg.line_info().clone(),
                        ))
                    } else {
                        Err(CompilerTodoError::of(
                            "Cannot deduce lambda types from non-Callable",
                            arg,
                        )
                        .into())
                    }
                } else {
                    Err(CompilerException::of(
                        "Cannot deduce type of lambda argument",
                        arg.get_name(),
                    )
                    .into())
                }
            })
            .collect()
    }
}

test_convertible_expected!(LambdaNode, LambdaConverter);
