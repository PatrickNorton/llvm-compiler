use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::iter::repeat_with;

use derive_new::new;
use itertools::Itertools;

use crate::parser::argument::ArgumentNode;
use crate::parser::fn_call::FunctionCallNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator_fn::EscapedOperatorNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::first;

use super::argument::{ArgPosition, Argument};
use super::assign::AssignConverter;
use super::bytecode::{ArgcBytecode, Bytecode};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{LangConstant, OptionConstant};
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::{CompilerException, CompilerInternalError, CompilerTodoError};
use super::fn_info::FunctionInfo;
use super::operator::OperatorConverter;
use super::test_converter::TestConverter;
use super::type_loader::TypeLoader;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct FunctionCallConverter<'a> {
    node: &'a FunctionCallNode,
    ret_count: u16,
}

impl<'a> ConverterBase for FunctionCallConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_inner(info, false)
    }
}

impl<'a> ConverterTest for FunctionCallConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        if let Result::Ok(node) = <&EscapedOperatorNode>::try_from(self.node.get_caller()) {
            return self.escaped_op_return(info, node);
        }
        let ret_type = first(TestConverter::return_type(info, self.node.get_caller(), 1)?);
        let ret_info = ret_type.try_operator_info(self.node, OpSpTypeNode::Call, info)?;
        Self::generify_returns(
            &ret_info,
            info,
            self.node.get_parameters(),
            self.node,
            Some(self.ret_count),
        )
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        if let Result::Ok(op) = <&EscapedOperatorNode>::try_from(self.node.get_caller()) {
            let op = op.get_operator().get_operator();
            OperatorConverter::of_components(
                op,
                self.node.get_parameters(),
                self.node.line_info().clone(),
                self.ret_count,
            )
            .constant_return(info)
        } else if self.is_determined_function(info, self.node.get_caller()) {
            self.determined_function_constant(info)
        } else {
            Ok(None)
        }
    }
}

impl<'a> FunctionCallConverter<'a> {
    pub fn convert_tail(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_inner(info, true)
    }

    fn convert_inner(&mut self, info: &mut CompilerInfo, tail: bool) -> CompileBytes {
        if let Option::Some(constant) = self.constant_return(info)? {
            return Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())));
        }
        if let Result::Ok(op) = <&EscapedOperatorNode>::try_from(self.node.get_caller()) {
            return self.convert_op(info, op);
        }
        let mut call_converter = self.node.get_caller().test_converter(1);
        let ret_types = call_converter.return_type(info)?;
        let (fn_info, needs_make_option) =
            Self::ensure_types_match(info, self.node, &ret_types[0], self.node.get_parameters())?;
        if self.is_determined_function(info, self.node.get_caller()) {
            return self.convert_optimized(info, &fn_info, &needs_make_option, tail);
        }
        let mut bytes = call_converter.convert(info)?;
        let argc = self.convert_arguments(info, &mut bytes, &fn_info, &needs_make_option)?;
        bytes.add(if tail {
            Bytecode::TailTos(argc.into())
        } else {
            Bytecode::CallTos(argc.into())
        });
        let ret_type = self.return_type(info)?;
        bytes.extend_with(
            repeat_with(Bytecode::PopTop)
                .take(ret_type.len().saturating_sub(self.ret_count as usize)),
        );
        Ok(bytes)
    }

    pub fn generify_returns(
        fn_info: &FunctionInfo,
        info: &mut CompilerInfo,
        params: &[ArgumentNode],
        node: impl Lined,
        ret_count: Option<u16>,
    ) -> CompileTypes {
        let args = get_args(info, params)?;
        let generics = fn_info.generify_args(&args)?.0;
        if generics.is_empty() {
            let returns = fn_info.get_returns();
            return if ret_count.map_or_else(|| false, |ret| returns.len() < ret as usize) {
                Err(CompilerException::of(
                    format!(
                        "Cannot call function {} with {} returns: only returns {} values",
                        fn_info.get_name(),
                        ret_count.unwrap(),
                        returns.len()
                    ),
                    node,
                )
                .into())
            } else {
                Ok(returns.to_owned())
            };
        }
        let cls = fn_info.to_callable();
        let returns = fn_info.get_returns();
        let gen = turn_map_to_list(generics);
        if ret_count.map_or_else(|| false, |ret| returns.len() < ret as usize) {
            return Err(CompilerInternalError::of(
                format!(
                    "Length {} less than length {}",
                    returns.len(),
                    ret_count.unwrap()
                ),
                node,
            )
            .into());
        }
        let result = returns.iter().map(|x| x.generify_with(&cls, gen.clone()));
        Ok(if let Option::Some(ret) = ret_count {
            result.take(ret as usize).collect()
        } else {
            result.collect()
        })
    }

    pub fn convert_args(
        info: &mut CompilerInfo,
        node: impl Lined,
        caller: &TypeObject,
        args: &[ArgumentNode],
    ) -> CompileResult<(BytecodeList, u16)> {
        let (fn_info, swaps) = Self::ensure_types_match(info, node, caller, args)?;
        let arg_positions = fn_info.get_args().arg_positions(&get_args(info, args)?)?;
        let mut bytes = BytecodeList::new();
        let argc = Self::convert_inner_args(info, &mut bytes, args, &arg_positions, &swaps)?;
        Ok((bytes, argc))
    }

    fn convert_arguments(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        fn_info: &FunctionInfo,
        needs_make_option: &HashSet<u16>,
    ) -> CompileResult<u16> {
        let params = self.node.get_parameters();
        let arg_positions = fn_info.get_args().arg_positions(&get_args(info, params)?)?;
        Self::convert_inner_args(info, bytes, params, &arg_positions, needs_make_option)
    }

    fn convert_inner_args(
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        params: &[ArgumentNode],
        arg_positions: &[ArgPosition],
        needs_make_option: &HashSet<u16>,
    ) -> CompileResult<u16> {
        let mut place_in_vararg = 0;
        let (mut i, mut j) = (0, 0);
        while let Option::Some(arg_pos) = arg_positions.get(i) {
            match arg_pos {
                ArgPosition::Default(def) => {
                    def.load_bytes(bytes, info);
                    i += 1;
                    continue;
                }
                ArgPosition::Vararg(vararg, _) if vararg.is_empty() => {
                    i += 1;
                    continue;
                }
                _ => {}
            }
            let param = &params[j];
            let mut converter = param.get_argument().test_converter(1);
            bytes.extend(converter.convert(info)?);
            if param.is_vararg() {
                match first(converter.return_type(info)?) {
                    TypeObject::Tuple(ret) => {
                        bytes.add(Bytecode::UnpackTuple());
                        let gen_count = ret.get_generics().len();
                        for k in 0..gen_count {
                            if gen_count - k - 1 != 0 {
                                add_swap(bytes, 0, (gen_count - k - 1) as u16);
                                bytes.add(Bytecode::MakeOption());
                                add_swap(bytes, 0, (gen_count - k - 1) as u16);
                            } else {
                                bytes.add(Bytecode::MakeOption());
                            }
                        }
                    }
                    ret_type => {
                        if ret_type.operator_info(OpSpTypeNode::Iter, info).is_some() {
                            return Err(CompilerTodoError::of(
                                "Unpacking iterables in function calls",
                                param,
                            )
                            .into());
                        } else {
                            return Err(CompilerException::of(
                                format!(
                                    "Illegal parameter expansion: \
                                     Value must be a tuple, instead '{}'",
                                    ret_type.name()
                                ),
                                param,
                            )
                            .into());
                        }
                    }
                };
            } else if needs_make_option.contains(&(i as u16)) {
                bytes.add(Bytecode::MakeOption());
            }
            j += 1;
            // FIXME: Doesn't work for unpacked tuples
            match arg_pos {
                ArgPosition::Vararg(var, _) if place_in_vararg != var.len() - 1 => {
                    place_in_vararg = 0;
                    i += 1;
                }
                _ => place_in_vararg += 1,
            }
        }
        Self::add_swaps(bytes, params.len().try_into().unwrap(), arg_positions, info)?;
        Ok(arg_positions.len().try_into().unwrap())
    }

    fn add_swaps(
        bytes: &mut BytecodeList,
        param_len: u16,
        arg_positions: &[ArgPosition],
        info: &mut CompilerInfo,
    ) -> CompileResult<()> {
        assert!(arg_positions.len() <= u16::MAX.into());
        let arg_len = arg_positions.len() as u16;
        let swaps = if let Option::Some(vararg_pos) = get_vararg_pos(arg_positions) {
            // Phases of argument-repositioning:
            // 1. Move all variadic arguments to top of stack
            // 2. Load type of list
            // 3. Pack arguments into list
            // 4. Do standard swapping from there
            let (varargs, vararg_ty) = match &arg_positions[vararg_pos as usize] {
                ArgPosition::Vararg(x, y) => (x, y),
                _ => panic!("get_vararg_pos didn't work correctly"),
            };
            assert!(varargs.len() <= u16::MAX.into());
            let vararg_len: u16 = varargs.len().try_into().unwrap();
            for (i, &location) in varargs.iter().enumerate() {
                // FIXME? This assumes the list is sorted (values which are
                //  unsorted won't "pass" the next ones)
                // Reasoning: The distance of each parameter from the top is
                // paramLen - location (SWAP_N is 1-based, at least for now),
                // and the + i is there to compensate for the fact that each
                // value that is brought up "passes" all the others on the
                // stack, and thus shifts them 1 further away from the top.
                let default_count = count_defaults(vararg_pos.into(), arg_positions) as u16;
                let stack_loc = param_len - location + default_count + i as u16;
                AssignConverter::bring_to_top(bytes, stack_loc - 1);
            }
            // FIXME: Get line info
            bytes.extend(TypeLoader::new(LineInfo::empty(), vararg_ty.clone()).convert(info)?);
            bytes.add(Bytecode::ListCreate(vararg_len.into()));
            let current_state = arg_positions
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    match param {
                        ArgPosition::Standard(pos) => {
                            // Any positional arguments which got packed into a vararg
                            // "crossed" our value, so we need to adjust for them.
                            pos - varargs.iter().filter(|&x| x < pos).count() as u16
                        }
                        ArgPosition::Default(_) => {
                            // If the vararg is supposed to be below the default value,
                            // it's been "passed up" past this value in order to be
                            // packed, so we need to adjust our parameter accordingly.
                            // NOTE: This doesn't need the Util.countLessThan call
                            // because `i` is based on the *final* position of the
                            // argument (we can assume it was placed correctly in that
                            // regard, thanks to convertInnerArgs), therefore it doesn't
                            // need adjusting for any of the pre-vararg positions.
                            let i = i as u16;
                            let adjust_for_vararg = if i > vararg_pos { 1 } else { 0 };
                            i - adjust_for_vararg
                        }
                        ArgPosition::Vararg(_, _) => arg_positions.len() as u16 - 1,
                    }
                })
                .collect_vec();
            swaps_to_order(current_state)
        } else {
            // If there is no vararg, we can just use the same stuff we've been
            // doing before--swap everything around until it's all in the right
            // place
            let current_state = arg_positions
                .iter()
                .enumerate()
                .map(|(i, param)| match param {
                    ArgPosition::Standard(val) => Ok(*val),
                    ArgPosition::Default(_) => Ok(i as u16),
                    ArgPosition::Vararg(_, _) => Err(CompilerInternalError::of(
                        "swaps_to_order() called with variadic argument",
                        LineInfo::empty(),
                    )),
                })
                .collect::<Result<_, _>>()?;
            swaps_to_order(current_state)
        };
        for (a, b) in swaps {
            let d1 = arg_len - a - 1;
            let d2 = arg_len - b - 1;
            add_swap(bytes, d1, d2);
        }
        Ok(())
    }

    fn escaped_op_return(
        &self,
        info: &mut CompilerInfo,
        escaped_op: &EscapedOperatorNode,
    ) -> CompileTypes {
        let op = escaped_op.get_operator().get_operator();
        OperatorConverter::of_components(
            op,
            self.node.get_parameters(),
            self.node.line_info().clone(),
            self.ret_count,
        )
        .return_type(info)
    }

    fn is_determined_function(&self, info: &mut CompilerInfo, name: &TestNode) -> bool {
        let variable = match <&VariableNode>::try_from(name) {
            Result::Ok(x) => x,
            Result::Err(_) => return false,
        };
        // FIXME: Ensure name isn't being overwritten by user-defined variable
        let str_name = variable.get_name();
        info.fn_info(str_name).is_some()
            || (info.builtins().has_name(str_name)
                && (builtin_operator(str_name).is_some()
                    || builtin_bytecode(str_name).is_some()
                    || builtin_container_bytecode(str_name).is_some()))
    }

    fn determined_function_constant(&self, info: &mut CompilerInfo) -> CompileConstant {
        debug_assert!(self.is_determined_function(info, self.node.get_caller()));
        if self.node.get_parameters().len() != 1
            || !self.node.get_parameters()[0].get_vararg().is_empty()
        {
            return Ok(None);
        }
        let str_name = <&VariableNode>::try_from(self.node.get_caller())
            .unwrap()
            .get_name();
        if let Option::Some(op) = builtin_operator(str_name) {
            let arg = self.node.get_parameters()[0].get_argument();
            if let Option::Some(ret) = TestConverter::constant_return(arg, info, 1)? {
                Ok(self.constant_op(op, ret))
            } else {
                Ok(None)
            }
        } else if str_name == "option" {
            let arg = self.node.get_parameters()[0].get_argument();
            if let Option::Some(ret) = TestConverter::constant_return(arg, info, 1)? {
                Ok(Some(OptionConstant::new(ret).into()))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn convert_op(&self, info: &mut CompilerInfo, operator: &EscapedOperatorNode) -> CompileBytes {
        let op = operator.get_operator().get_operator();
        OperatorConverter::of_components(
            op,
            self.node.get_parameters(),
            self.node.line_info().clone(),
            self.ret_count,
        )
        .convert(info)
    }

    fn constant_op(&self, op: OpSpTypeNode, constant: LangConstant) -> Option<LangConstant> {
        match op {
            OpSpTypeNode::Str => {
                if let LangConstant::String(_) = &constant {
                    Some(constant)
                } else {
                    constant.str_value().map(Into::into)
                }
            }
            OpSpTypeNode::Bool => {
                if let LangConstant::Bool(_) = &constant {
                    Some(constant)
                } else {
                    constant.bool_value().map(Into::into)
                }
            }
            OpSpTypeNode::Repr => constant.repr_value().map(Into::into),
            OpSpTypeNode::Int => match constant {
                LangConstant::Bigint(_) | LangConstant::Int(_) => Some(constant),
                LangConstant::Bool(b) => Some(if b.bool_value() { 1 } else { 0 }.into()),
                _ => None,
            },
            _ => None,
        }
    }

    fn ensure_types_match<'t>(
        info: &mut CompilerInfo,
        line_info: impl Lined,
        caller_type: &'t TypeObject,
        args: &[ArgumentNode],
    ) -> CompileResult<(Cow<'t, FunctionInfo>, HashSet<u16>)> {
        let args = get_args(info, args)?;
        let operator_info = caller_type.try_operator_info(line_info, OpSpTypeNode::Call, info)?;
        let op_generics = operator_info.generify_args(&args)?;
        Ok((operator_info, op_generics.1))
    }

    fn convert_optimized(
        &self,
        info: &mut CompilerInfo,
        fn_info: &FunctionInfo,
        needs_make_option: &HashSet<u16>,
        tail: bool,
    ) -> CompileBytes {
        let str_name = <&VariableNode>::try_from(self.node.get_caller())
            .unwrap()
            .get_name();
        if info.builtins().has_name(str_name) {
            self.convert_builtin(info, str_name)
        } else {
            self.convert_call_fn(info, str_name, fn_info, needs_make_option, tail)
        }
    }

    fn convert_builtin(&self, info: &mut CompilerInfo, str_name: &str) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        if let Option::Some(operator) = builtin_operator(str_name) {
            let params = self.node.get_parameters();
            if params.len() != 1 {
                // No operator needs more than self
                let args = get_args(info, params)?;
                let builtins = info.builtins();
                let builtin = builtins.get_name(str_name).unwrap().get_type(builtins);
                let fn_info = builtin.operator_info(OpSpTypeNode::Call, info).unwrap();
                return Err(
                    arg_error(self.node, &builtin.name(), &args, pos_args(&fn_info)).into(),
                );
            }
            let argument = params[0].get_argument();
            let mut arg_converter = argument.test_converter(1);
            first(arg_converter.return_type(info)?).try_operator_info(argument, operator, info)?;
            bytes.extend(arg_converter.convert(info)?);
            bytes.add(Bytecode::CallOp(operator.into(), 0.into()));
            Ok(bytes)
        } else if let Option::Some(bytecode) = builtin_bytecode(str_name) {
            let params = self.node.get_parameters();
            if params.len() != 1 {
                return Err(CompilerException::of(
                    format!(
                        "'{}' can only be called with 1 argument, not {}",
                        str_name,
                        params.len()
                    ),
                    self.node,
                )
                .into());
            }
            bytes.extend(TestConverter::bytes(params[0].get_argument(), info, 1)?);
            bytes.add(bytecode);
            Ok(bytes)
        } else if let Option::Some((empty, cap)) = builtin_container_bytecode(str_name) {
            // TODO: Load generic type properly
            if str_name != "dict" {
                bytes.add(Bytecode::LoadConst(
                    info.builtins().object_const().clone().into(),
                ))
            }
            if self.node.get_parameters().is_empty() {
                bytes.add(empty(0.into()));
                Ok(bytes)
            } else if self.node.get_parameters().len() == 1
                && argument_list_is_cap(self.node.get_parameters())
            {
                let param = self.node.get_parameters()[0].get_argument();
                let mut converter = param.test_converter(1);
                let ret_type = first(converter.return_type(info)?);
                assert!(info.builtins().int_type().is_superclass(&ret_type));
                // TODO: Proper error message
                bytes.extend(converter.convert(info)?);
                bytes.add(cap);
                Ok(bytes)
            } else {
                Err(
                    CompilerTodoError::of("Invalid conversion for optimized container", self.node)
                        .into(),
                )
            }
        } else {
            Err(CompilerInternalError::of(
                format!("Invalid builtin function name {}", str_name),
                self.node,
            )
            .into())
        }
    }

    fn convert_call_fn(
        &self,
        info: &mut CompilerInfo,
        str_name: &str,
        fn_info: &FunctionInfo,
        needs_make_option: &HashSet<u16>,
        tail: bool,
    ) -> CompileBytes {
        let fn_index = info.fn_index(str_name).unwrap();
        if fn_info.is_deprecated() {
            warning::warn(
                format!("Function '{}' is deprecated", str_name),
                WarningType::Deprecated,
                info,
                self.node,
            )?;
        }
        if fn_info.must_use() && (self.ret_count as usize) < fn_info.get_returns().len() {
            let val = if fn_info.get_returns().len() - (self.ret_count as usize) == 1 {
                "value"
            } else {
                "values"
            };
            let message = fn_info.get_must_use_message();
            if message.is_empty() {
                warning::warn(
                    format!("Unused return {} of '{}' that must be used", val, str_name),
                    WarningType::Unused,
                    info,
                    self.node,
                )?;
            } else {
                warning::warn(
                    format!(
                        "Unused return {} of '{}' that must be used\nNote: {}",
                        val, str_name, message
                    ),
                    WarningType::Unused,
                    info,
                    self.node,
                )?;
            }
        }
        let mut bytes = BytecodeList::new();
        let argc = self.convert_arguments(info, &mut bytes, fn_info, needs_make_option)?;
        bytes.add(if tail {
            Bytecode::TailFn
        } else {
            Bytecode::CallFn
        }(fn_index.into(), argc.into()));
        Ok(bytes)
    }
}

fn get_args(info: &mut CompilerInfo, args: &[ArgumentNode]) -> CompileResult<Vec<Argument>> {
    args.iter()
        .map(|arg| {
            let type_val = first(TestConverter::return_type(info, arg.get_argument(), 1)?);
            let line_info = if arg.get_variable().is_empty() {
                arg.get_argument().line_info()
            } else {
                arg.get_variable().line_info()
            };
            Ok(Argument::new_full(
                arg.get_variable().get_name().to_string(),
                type_val,
                arg.is_vararg(),
                line_info.clone(),
            ))
        })
        .collect()
}

fn turn_map_to_list(values: HashMap<u16, TypeObject>) -> Vec<TypeObject> {
    let mut result = Vec::new();
    for (index, value) in values {
        let index = index as usize;
        match index.cmp(&result.len()) {
            Ordering::Less => result.push(Some(value)),
            Ordering::Equal => {
                assert_eq!(result[index], None);
                result[index] = Some(value);
            }
            Ordering::Greater => {
                result.resize_with(index, || None);
                result[index] = Some(value);
            }
        }
    }
    result.into_iter().map(|x| x.unwrap()).collect()
}

fn builtin_operator(name: &str) -> Option<OpSpTypeNode> {
    match name {
        "int" => Some(OpSpTypeNode::Int),
        "str" => Some(OpSpTypeNode::Str),
        "bool" => Some(OpSpTypeNode::Bool),
        "repr" => Some(OpSpTypeNode::Repr),
        "reversed" => Some(OpSpTypeNode::Reversed),
        "iter" => Some(OpSpTypeNode::Iter),
        "hash" => Some(OpSpTypeNode::Hash),
        _ => None,
    }
}

fn builtin_bytecode(name: &str) -> Option<Bytecode> {
    match name {
        "type" => Some(Bytecode::GetType()),
        "option" => Some(Bytecode::MakeOption()),
        _ => None,
    }
}

type ContainerFn = fn(ArgcBytecode) -> Bytecode;

fn builtin_container_bytecode(name: &str) -> Option<(ContainerFn, Bytecode)> {
    match name {
        "list" => Some((Bytecode::ListCreate, Bytecode::ListCap())),
        "set" => Some((Bytecode::SetCreate, Bytecode::SetCap())),
        "dict" => Some((Bytecode::DictCreate, Bytecode::DictCap())),
        _ => None,
    }
}

fn arg_error<'a>(
    node: impl Lined,
    name: &str,
    args: &[Argument],
    expected: impl Iterator<Item = &'a Argument>,
) -> CompilerException {
    let args_str = args.iter().map(|x| x.get_type().name()).format(", ");
    let expected_str = expected.map(|x| x.get_type().name()).format(", ");
    CompilerException::of(
        format!(
            "Cannot call object of type '{}': arguments given \
             do not match the arguments of the function\n\
             Arguments received: {}\nArguments expected: {}",
            name, args_str, expected_str
        ),
        node,
    )
}

fn pos_args(fn_info: &FunctionInfo) -> impl Iterator<Item = &Argument> {
    let arg_info = fn_info.get_args();
    arg_info
        .get_position_args()
        .iter()
        .chain(arg_info.get_normal_args().iter())
}

fn argument_list_is_cap(args: &[ArgumentNode]) -> bool {
    args.len() == 1 && !args[0].is_vararg() && args[0].get_variable().get_name() == "cap"
}

fn add_swap(bytes: &mut BytecodeList, d1: u16, d2: u16) {
    assert_ne!(d1, d2);
    let (d1, d2) = if d1 < d2 { (d1, d2) } else { (d2, d1) };
    if d1 == 0 && d2 == 1 {
        bytes.add(Bytecode::Swap2())
    } else {
        bytes.add(Bytecode::SwapStack(d1.into(), d2.into()))
    }
}

fn get_vararg_pos(arg_positions: &[ArgPosition]) -> Option<u16> {
    arg_positions
        .iter()
        .enumerate()
        .find(|(_, x)| x.is_vararg())
        .map(|(i, _)| i.try_into().unwrap())
}

fn count_defaults(start: usize, args: &[ArgPosition]) -> usize {
    args[start..]
        .iter()
        .filter(|x| matches!(x, ArgPosition::Default(_)))
        .count()
}

fn swaps_to_order(mut current_state: Vec<u16>) -> Vec<(u16, u16)> {
    assert!(current_state.len() <= u16::MAX.into());
    let mut swaps = Vec::new();
    for i in 0..current_state.len() {
        let mut ptr = i;
        while ptr != current_state[ptr].into() {
            swaps.push((current_state[ptr], ptr.try_into().unwrap()));
            let idx = current_state[ptr];
            current_state.swap(idx.into(), ptr);
            ptr = current_state[ptr].into()
        }
    }
    swaps
}

test_convertible!(FunctionCallNode, FunctionCallConverter);
