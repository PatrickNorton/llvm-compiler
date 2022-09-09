use core::slice;
use std::iter::zip;

use derive_new::new;

use crate::parser::fn_call::FunctionCallNode;
use crate::parser::line_info::Lined;
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::typed_arg::VarargType;
use crate::util::first;

use super::bytecode::{ArgcBytecode, Bytecode};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{ConverterBase, ConverterTest, TestConvertible};
use super::error::{CompilerException, CompilerTodoError};
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct ReturnListConverter<'a> {
    values: &'a TestListNode,
    ret_types: Vec<TypeObject>,
    value: RetListBytecode,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum RetListBytecode {
    Return,
    Yield,
}

impl<'a> ConverterBase for ReturnListConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_types.len() > 1 && self.values.len() == 1 {
            self.convert_single(info)
        } else if self.can_tail_call(info)? {
            self.convert_tail_call(info)
        } else {
            self.convert_multiple(info)
        }
    }
}

impl<'a> ReturnListConverter<'a> {
    fn can_tail_call(&self, info: &mut CompilerInfo) -> CompileResult<bool> {
        Ok(self.value == RetListBytecode::Return
            && self.ret_types.len() == 1
            && self.values.len() == 1
            && self.values.get_vararg(0).unwrap().is_empty()
            && <&FunctionCallNode>::try_from(&self.values[0]).is_ok()
            && !self.first_needs_option(info)?)
    }

    fn first_needs_option(&self, info: &mut CompilerInfo) -> CompileResult<bool> {
        Ok(OptionTypeObject::needs_make_option(
            &self.ret_types[0],
            &TestConverter::return_type(info, &self.values[0], 1)?[0],
        ))
    }

    fn convert_multiple(&self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        self.check_return_types(info)?;
        if self.ret_types.len() == 1 {
            bytes.extend(TestConverter::bytes_maybe_option(
                &self.values[0],
                info,
                1,
                &self.ret_types[0],
            )?)
        } else if !self.values.is_empty() {
            for (ret_type, (vararg, val)) in zip(&self.ret_types, self.values.pairs()) {
                bytes.extend(self.convert_inner(info, val, *vararg, ret_type)?)
            }
        }
        bytes.add(self.value.bytecode((self.ret_types.len() as u16).into()));
        Ok(bytes)
    }

    fn convert_inner(
        &self,
        info: &mut CompilerInfo,
        stmt: &TestNode,
        vararg: VarargType,
        ret_type: &TypeObject,
    ) -> CompileBytes {
        match vararg {
            VarargType::None => TestConverter::bytes_maybe_option(stmt, info, 1, ret_type),
            VarargType::Single => {
                let return_type = first(TestConverter::return_type(info, stmt, 1)?);
                if return_type.same_base_type(info.builtins().tuple_type()) {
                    Err(CompilerTodoError::of(
                        format!("Cannot convert {} with varargs yet", self.return_name()),
                        stmt,
                    )
                    .into())
                } else if info.builtins().iterable().is_superclass(&return_type) {
                    Err(CompilerException::of(
                        format!("Cannot unpack iterable in {} statement", self.return_name()),
                        stmt,
                    )
                    .into())
                } else {
                    Err(CompilerException::of(
                        format!(
                            "Can only unpack tuples in {} stateemnt, not '{}'",
                            self.return_name(),
                            return_type.name()
                        ),
                        stmt,
                    )
                    .into())
                }
            }
            VarargType::Double => Err(CompilerException::of(
                format!(
                    "Cannot unpack dictionaries in {} statement",
                    self.return_name()
                ),
                stmt,
            )
            .into()),
        }
    }

    fn convert_single(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert_eq!(self.values.len(), 1);
        let mut converter = self.values[0].test_converter(self.ret_types.len() as u16);
        let ret_types = converter.return_type(info)?;
        self.check_single_return(info, &ret_types)?;
        let mut bytes = converter.convert(info)?;
        for (i, (expected, ret)) in zip(&self.ret_types, &ret_types).enumerate().rev() {
            if OptionTypeObject::needs_make_option(expected, ret) {
                let dist_from_top = ret_types.len() - i - 1;
                add_swap(&mut bytes, dist_from_top);
                bytes.add(Bytecode::MakeOption());
                add_swap(&mut bytes, dist_from_top);
            }
        }
        bytes.add(self.value.bytecode((ret_types.len() as u16).into()));
        Ok(bytes)
    }

    fn convert_tail_call(&self, info: &mut CompilerInfo) -> CompileBytes {
        debug_assert!(self.can_tail_call(info)?);
        let node = <&FunctionCallNode>::try_from(&self.values[0]).unwrap();
        let mut converter = node.test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        if !self.ret_types[0].is_superclass(&ret_type) {
            return Err(self
                .type_error(node, 0, &self.ret_types[0], &ret_type)
                .into());
        }
        let mut bytes = converter.convert_tail(info)?;
        bytes.add(Bytecode::Return(1.into())); // Necessary b/c tail-call may delegate to normal call at runtime
        Ok(bytes)
    }

    fn check_return_types(&self, info: &mut CompilerInfo) -> CompileResult<()> {
        assert!(!self.values.is_empty());
        if self.ret_types.len() != self.values.len() {
            return Err(CompilerException::of(
                format!(
                    "Incorrect number of values {}ed: expected {}, got {}",
                    self.return_name(),
                    self.ret_types.len(),
                    self.values.len()
                ),
                &self.values[0],
            )
            .into());
        }
        for (i, (node, expected)) in zip(self.values, &self.ret_types).enumerate() {
            let mut converter = node.test_conv_expected(1, slice::from_ref(expected));
            let ret_type = first(converter.return_type(info)?);
            if bad_type(expected, &ret_type) {
                return Err(self.type_error(node, i, &ret_type, expected).into());
            }
        }
        Ok(())
    }

    fn check_single_return(
        &self,
        info: &mut CompilerInfo,
        returns: &[TypeObject],
    ) -> CompileResult<()> {
        assert_eq!(self.values.len(), 1);
        let ret_info = info.get_fn_returns();
        let fn_returns = ret_info.current_fn_returns();
        if returns.len() < fn_returns.len() {
            return Err(CompilerException::of(
                format!(
                    "Value given does not {} enough values: expected at least {}, got {}",
                    self.return_name(),
                    fn_returns.len(),
                    returns.len()
                ),
                &self.values[0],
            )
            .into());
        }
        for (i, (fn_ret, ret_type)) in zip(fn_returns, returns).enumerate() {
            if bad_type(fn_ret, ret_type) {
                return Err(self.type_error(&self.values[0], i, ret_type, fn_ret).into());
            }
        }
        Ok(())
    }

    fn return_name(&self) -> &'static str {
        match self.value {
            RetListBytecode::Return => "return",
            RetListBytecode::Yield => "yield",
        }
    }

    fn type_error(
        &self,
        lined: impl Lined,
        index: usize,
        ret_type: &TypeObject,
        fn_ret: &TypeObject,
    ) -> CompilerException {
        CompilerException::of(
            format!(
                "Value {}ed in position {}, of type '{}', \
                 is not a subclass of the required return '{}'",
                self.return_name(),
                index,
                ret_type.name(),
                fn_ret.name()
            ),
            lined,
        )
    }
}

impl RetListBytecode {
    pub fn bytecode(&self, argc: ArgcBytecode) -> Bytecode {
        match self {
            RetListBytecode::Return => Bytecode::Return(argc),
            RetListBytecode::Yield => Bytecode::Yield(argc),
        }
    }
}

fn add_swap(bytes: &mut BytecodeList, dist_from_top: usize) {
    match dist_from_top {
        0 => {}
        1 => bytes.add(Bytecode::Swap2()),
        x => bytes.add(Bytecode::SwapStack(0.into(), (x as u16).into())),
    }
}

fn bad_type(fn_ret: &TypeObject, ret_type: &TypeObject) -> bool {
    if fn_ret.is_superclass(ret_type) {
        false
    } else if OptionTypeObject::needs_make_option(fn_ret, ret_type) {
        !OptionTypeObject::super_with_option(fn_ret, ret_type)
    } else {
        true
    }
}
