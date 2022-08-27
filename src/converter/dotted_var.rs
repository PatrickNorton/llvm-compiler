use std::iter::repeat_with;

use crate::parser::argument::ArgumentNode;
use crate::parser::dotted::{DotPrefix, DottedVar, DottedVariableNode};
use crate::parser::fn_call::FunctionCallNode;
use crate::parser::index::IndexNode;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::operator_sp::{OpSpTypeNode, SpecialOpNameNode};
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::first;

use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::{CompilerException, CompilerInternalError};
use super::fn_call::FunctionCallConverter;
use super::index::IndexConverter;
use super::operator::OperatorConverter;
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug)]
pub struct DotConverter<'a> {
    pre_dot: &'a TestNode,
    post_dots: &'a [DottedVar],
    ret_count: u16,
}

impl<'a> DotConverter<'a> {
    pub fn new(node: &'a DottedVariableNode, ret_count: u16) -> Self {
        Self {
            pre_dot: node.get_pre_dot(),
            post_dots: node.get_post_dots(),
            ret_count,
        }
    }
}

impl<'a> ConverterBase for DotConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut pre_converter = self.pre_dot.test_converter(1);
        let mut bytes = pre_converter.convert(info)?;
        let mut previous = pre_converter.return_type(info)?;
        for dot in self.post_dots {
            if previous.is_empty() {
                return Err(CompilerException::of(
                    "Expected at least 1 return, got 0",
                    dot.get_post_dot(),
                )
                .into());
            }
            let prev = first(previous);
            previous = match dot.get_dot_prefix() {
                DotPrefix::None => self.convert_normal(info, prev, &mut bytes, dot)?,
                DotPrefix::Question => self.convert_null_dot(info, prev, &mut bytes, dot)?,
                DotPrefix::DoubleBang => self.convert_not_null_dot(info, prev, &mut bytes, dot)?,
            }
        }
        let ret_count = self.ret_count as usize;
        if previous.len() < ret_count {
            Err(CompilerException::of(
                format!(
                    "Cannot convert: {} returns is less than the required {}",
                    previous.len(),
                    ret_count
                ),
                self.pre_dot,
            )
            .into())
        } else {
            bytes.extend_with(repeat_with(Bytecode::PopTop).take(previous.len() - ret_count));
            Ok(bytes)
        }
    }
}

impl<'a> ConverterTest for DotConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        let mut result = TestConverter::return_type(info, self.pre_dot, 1)?;
        for (i, dot) in self.post_dots.iter().enumerate() {
            if result.is_empty() {
                return Err(CompilerException::of(
                    "Dot does not return a value, expected at least 1",
                    if i == 0 {
                        self.pre_dot.line_info()
                    } else {
                        self.post_dots[i - 1].line_info()
                    },
                )
                .into());
            }
            result = self.dot_return_type(info, first(result), dot)?;
        }
        if self.ret_count as usize > result.len() {
            Err(CompilerException::of(
                format!(
                    "Expected at least {} returns, but only got {}",
                    self.ret_count,
                    result.len()
                ),
                self.post_dots.last().unwrap(),
            )
            .into())
        } else {
            result.truncate(self.ret_count as usize);
            Ok(result)
        }
    }
}

impl<'a> DotConverter<'a> {
    pub fn except_last(
        node: &DottedVariableNode,
        ret_count: u16,
    ) -> CompileResult<(TestConverter, &str)> {
        let post_dots = node.get_post_dots();
        let post_dot = match node.get_last().get_post_dot() {
            NameNode::Variable(var) => var,
            last => {
                return Err(CompilerInternalError::of(
                    "DotConverter::except_last does not work where the last dot is not plain",
                    last,
                )
                .into())
            }
        };
        if post_dots.len() == 1 {
            let converter = node.get_pre_dot().test_converter(ret_count);
            Ok((converter, post_dot.get_name()))
        } else {
            let converter = DotConverter {
                pre_dot: node.get_pre_dot(),
                post_dots: &post_dots[..post_dots.len() - 1],
                ret_count,
            };
            Ok((converter.into(), post_dot.get_name()))
        }
    }

    pub fn except_last_index(
        node: &DottedVariableNode,
        ret_count: u16,
    ) -> CompileResult<(TestConverter, &[TestNode])> {
        // var last = node.getLast();
        // var postDots = node.getPostDots();
        // if (!(last.getPostDot() instanceof IndexNode postDot)) {
        //     throw CompilerInternalError.of(
        //             "DotConverter.exceptLastIndex does not work where the last dot is not an index", last
        //     );
        // }
        // var newPostDots = Arrays.copyOf(postDots, postDots.length);
        // var dot = new DottedVar(last.getLineInfo(), last.getDotPrefix(), (NameNode) postDot.getVar());
        // newPostDots[newPostDots.length - 1] = dot;
        // var newNode = new DottedVariableNode(node.getPreDot(), newPostDots);
        // var converter = TestConverter.of(info, newNode, retCount);
        // return Pair.of(converter, postDot.getIndices());
        let post_dots = node.get_post_dots();
        let post_dot = match <&IndexNode>::try_from(node.get_last().get_post_dot()) {
            Ok(x) => x,
            Err(_) => return Err(CompilerInternalError::of(
                "DotConverter::except_last_index does not work where the last dot is not an index",
                node.get_last(),
            )
            .into()),
        };
        todo!("Properly convert the pre-index into a DVN")
    }

    fn convert_normal(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        dot: &DottedVar,
    ) -> CompileTypes {
        debug_assert!(dot.get_dot_prefix().is_empty());
        self.convert_post_dot(info, prev, bytes, dot.get_post_dot())
    }

    fn convert_null_dot(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        dot: &DottedVar,
    ) -> CompileTypes {
        // NOTE: This (and all other instances) can soon be replaced by assert_matches
        // (#82775), feature(assert_matches)
        debug_assert!(matches!(dot.get_dot_prefix(), DotPrefix::Question));
        let post_dot = dot.get_post_dot();
        match OptionTypeObject::try_from(prev) {
            Result::Err(prev) => {
                warning::warn(
                    format!("Using ?. operator on non-optional type {}", prev.name()),
                    WarningType::TrivialValue,
                    info,
                    dot,
                )?;
                if prev.same_base_type(info.builtins().null_type()) {
                    bytes.add(Bytecode::PopTop());
                    bytes.add(Bytecode::LoadNull());
                    Ok(vec![info.builtins().null_type().clone()])
                } else {
                    self.convert_post_dot(info, prev.strip_null(), bytes, post_dot)
                }
            }
            Result::Ok(prev) => {
                bytes.add(Bytecode::DupTop());
                let jump_lbl = Label::new();
                bytes.add(Bytecode::JumpNull(jump_lbl.clone().into()));
                bytes.add(Bytecode::UnwrapOption());
                let result =
                    self.convert_post_dot(info, prev.strip_null().clone(), bytes, post_dot)?;
                bytes.add(Bytecode::MakeOption());
                bytes.add_label(jump_lbl);
                Ok(vec![TypeObject::optional(first(result))])
            }
        }
    }

    fn convert_not_null_dot(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        dot: &DottedVar,
    ) -> CompileTypes {
        debug_assert!(matches!(dot.get_dot_prefix(), DotPrefix::DoubleBang));
        if prev.same_base_type(info.builtins().null_type()) {
            Err(CompilerException::of("Cannot use !! operator on null type", dot).into())
        } else {
            match OptionTypeObject::try_from(prev) {
                Result::Err(_) => Err(CompilerException::of(
                    "Cannot use !! operator on non-optional type",
                    dot,
                )
                .into()),
                Result::Ok(prev) => {
                    let post_dot = dot.get_post_dot();
                    OperatorConverter::unwrap_option(info, bytes, ""); // FIXME: post_dot.to_string()
                    self.convert_post_dot(info, prev.strip_null().clone(), bytes, post_dot)
                }
            }
        }
    }

    fn dot_return_type(
        &self,
        info: &mut CompilerInfo,
        result: TypeObject,
        dot: &DottedVar,
    ) -> CompileTypes {
        match dot.get_dot_prefix() {
            DotPrefix::None => self.normal_dot_return_type(info, result, dot),
            DotPrefix::Question => self.optional_dot_return_type(info, result, dot),
            DotPrefix::DoubleBang => self.non_null_dot_return_type(info, result, dot),
        }
    }

    fn convert_post_dot(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        post_dot: &NameNode,
    ) -> CompileTypes {
        match post_dot {
            NameNode::Variable(post) => {
                let type_val = prev.try_attr_type(post_dot, post.get_name(), info)?;
                bytes.add(Bytecode::LoadDot(post.get_name().into()));
                Ok(vec![type_val.into_owned()])
            }
            NameNode::Function(post) => self.convert_method(info, prev, bytes, post),
            NameNode::SpecialOp(post) => {
                let op = post.get_operator();
                bytes.add(Bytecode::LoadOp(op.into()));
                prev.try_operator_info(post_dot, op, info)
                    .map(|x| vec![x.to_callable()])
            }
            NameNode::Index(post) => self.convert_index(info, prev, bytes, post),
            _ => Err(CompilerInternalError::of(
                "This kind of post-dot not yet supported",
                post_dot,
            )
            .into()),
        }
    }

    fn normal_dot_return_type(
        &self,
        info: &mut CompilerInfo,
        result: TypeObject,
        dot: &DottedVar,
    ) -> CompileTypes {
        self.deconstructed_ret_type(info, result, dot.get_dot_prefix(), dot.get_post_dot())
    }

    fn deconstructed_ret_type(
        &self,
        info: &mut CompilerInfo,
        result: TypeObject,
        prefix: DotPrefix,
        post_dot: &NameNode,
    ) -> CompileTypes {
        assert!(prefix.is_empty() || !result.is_superclass(info.builtins().null_type()));
        match post_dot {
            NameNode::Variable(var) => result
                .try_attr_type(var, var.get_name(), info)
                .map(|x| vec![x.into_owned()]),
            NameNode::Function(post) => {
                let attr_type = match post.get_caller() {
                    TestNode::Name(NameNode::SpecialOp(caller)) => result
                        .try_operator_info(post, caller.get_operator(), info)?
                        .to_callable(),
                    TestNode::Name(NameNode::Variable(var)) => result
                        .try_attr_type(post, var.get_name(), info)?
                        .into_owned(),
                    TestNode::Name(name) => {
                        let pre_call_type =
                            self.deconstructed_ret_type(info, result, prefix, name)?;
                        return pre_call_type[0].try_operator_return_type(
                            name,
                            OpSpTypeNode::Call,
                            info,
                        );
                    }
                    _ => {
                        return Err(
                            CompilerInternalError::of("Invalid function-call prefix", post).into(),
                        )
                    }
                };
                let op_info = attr_type.try_operator_info(post, OpSpTypeNode::Call, info)?;
                if op_info.get_returns().is_empty() {
                    return Ok(Vec::new());
                }
                let params = post.get_parameters();
                FunctionCallConverter::generify_returns(&op_info, info, params, post, None)
            }
            NameNode::SpecialOp(op) => result
                .try_operator_info(post_dot, op.get_operator(), info)
                .map(|x| vec![x.to_callable()]),
            NameNode::Index(index) => {
                let attr_type = first(self.deconstructed_ret_type(
                    info,
                    result,
                    prefix,
                    index.get_var().try_into().unwrap(),
                )?);
                let operator = if IndexConverter::is_slice(index.get_indices()).is_some() {
                    OpSpTypeNode::GetSlice
                } else {
                    OpSpTypeNode::GetAttr
                };
                attr_type
                    .try_operator_return_type(self.pre_dot, operator, info)
                    .map(|mut x| {
                        x.truncate(1);
                        x
                    })
            }
            name => Err(CompilerInternalError::of("Unimplemented post-dot type", name).into()),
        }
    }

    fn optional_dot_return_type(
        &self,
        info: &mut CompilerInfo,
        result: TypeObject,
        dot: &DottedVar,
    ) -> CompileTypes {
        assert!(matches!(dot.get_dot_prefix(), DotPrefix::Question));
        if !result.is_option() {
            return self.normal_dot_return_type(info, result, dot);
        }
        match dot.get_post_dot() {
            NameNode::Variable(var) => {
                let stripped = result.strip_null();
                let ret_type = stripped.try_attr_type(var, var.get_name(), info)?;
                Ok(vec![TypeObject::optional(ret_type.into_owned())])
            }
            NameNode::Function(func) => {
                let caller = func.get_caller();
                let attr_type = match caller {
                    TestNode::Name(NameNode::SpecialOp(op)) => result
                        .strip_null()
                        .try_operator_info(func, op.get_operator(), info)?
                        .to_callable(),
                    TestNode::Name(NameNode::Variable(var)) => result
                        .strip_null()
                        .try_attr_type(func, var.get_name(), info)?
                        .into_owned(),
                    _ => return Err(CompilerInternalError::of("Unexpected caller", caller).into()),
                };
                let end_type =
                    first(attr_type.try_operator_return_type(func, OpSpTypeNode::Call, info)?);
                Ok(vec![TypeObject::optional(end_type)])
            }
            dot => Err(CompilerInternalError::of("Unimplemented post-dot type", dot).into()),
        }
    }

    fn non_null_dot_return_type(
        &self,
        info: &mut CompilerInfo,
        result: TypeObject,
        dot: &DottedVar,
    ) -> CompileTypes {
        let bang_type = if result.is_option() {
            result.strip_null()
        } else {
            result
        };
        self.normal_dot_return_type(info, bang_type, dot)
    }

    fn convert_method(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        post: &FunctionCallNode,
    ) -> CompileTypes {
        match post.get_caller() {
            TestNode::Name(NameNode::SpecialOp(op)) => {
                self.convert_operator(info, prev, bytes, op, post.get_parameters())
            }
            TestNode::Name(NameNode::Variable(var)) => {
                self.convert_name_method(info, prev, bytes, var, post.get_parameters())
            }
            TestNode::Name(name) => {
                self.convert_other_method(info, prev, bytes, name, post.get_parameters())
            }
            caller => Err(CompilerInternalError::of("Invalid function-call prefix", caller).into()),
        }
    }

    fn convert_index(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        post: &IndexNode,
    ) -> CompileTypes {
        let pre_index = <&NameNode>::try_from(post.get_var()).unwrap();
        let type_val = first(self.convert_post_dot(info, prev, bytes, pre_index)?);
        let indices = post.get_indices();
        if let Option::Some(slice) = IndexConverter::is_slice(indices) {
            debug_assert_eq!(indices.len(), 1);
            let result = type_val.try_operator_info(self.pre_dot, OpSpTypeNode::GetSlice, info)?;
            bytes.extend(TestConverter::bytes(slice, info, 1)?);
            bytes.add(Bytecode::CallOp(OpSpTypeNode::GetSlice.into(), 1.into()));
            Ok(result.get_returns().to_owned())
        } else {
            let result = type_val.try_operator_info(self.pre_dot, OpSpTypeNode::GetAttr, info)?;
            IndexConverter::convert_indices(info, bytes, indices)?;
            Ok(result.get_returns().to_owned())
        }
    }

    fn convert_operator(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        post: &SpecialOpNameNode,
        params: &[ArgumentNode],
    ) -> CompileTypes {
        let type_val = prev.try_operator_info(post, post.get_operator(), info)?;
        let (bytecode, argc) =
            FunctionCallConverter::convert_args(info, post, &type_val.to_callable(), params)?;
        bytes.extend(bytecode);
        bytes.add(Bytecode::CallOp(post.get_operator().into(), argc.into()));
        Ok(type_val.get_returns().to_owned())
    }

    fn convert_name_method(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        post: &VariableNode,
        params: &[ArgumentNode],
    ) -> CompileTypes {
        let type_val = prev.try_attr_type(post, post.get_name(), info)?;
        let (bytecode, argc) = FunctionCallConverter::convert_args(info, post, &type_val, params)?;
        bytes.extend(bytecode);
        bytes.add(Bytecode::CallMethod(post.get_name().into(), argc.into()));
        type_val.try_operator_return_type(post, OpSpTypeNode::Call, info)
    }

    fn convert_other_method(
        &self,
        info: &mut CompilerInfo,
        prev: TypeObject,
        bytes: &mut BytecodeList,
        post: &NameNode,
        params: &[ArgumentNode],
    ) -> CompileTypes {
        let type_val = first(self.convert_post_dot(info, prev, bytes, post)?);
        let (bytecode, argc) = FunctionCallConverter::convert_args(info, post, &type_val, params)?;
        bytes.extend(bytecode);
        bytes.add(Bytecode::CallTos(argc.into()));
        type_val.try_operator_return_type(post, OpSpTypeNode::Call, info)
    }
}

test_convertible!(DottedVariableNode, DotConverter);
