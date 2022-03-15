use derive_new::new;
use num::bigint::Sign;
use num::{BigInt, One, Signed, Zero};

use crate::converter::argument::Argument;
use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::constant::NumberConstant;
use crate::converter::convertible::{ConverterBase, ConverterTest, TestConvertible};
use crate::converter::error::{CompilerException, CompilerTodoError};
use crate::converter::operator::{is_mandatory, operator_bytecode};
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::TypeObject;
use crate::converter::warning::WarningType;
use crate::converter::{warning, CompileBytes, CompileConstant, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator::OperatorTypeNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::util::first;

use super::{as_exception, default_constant};

#[derive(Debug, new)]
pub(super) struct NormalOperatorConverter<'a> {
    op: OperatorTypeNode,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> NormalOperatorConverter<'a> {
    pub fn constant_return(&self, info: &mut CompilerInfo) -> CompileConstant {
        default_constant(self.op, info, self.args)
    }

    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        let op_type = TestConverter::return_type(info, self.args[0].get_argument(), 1)?;
        let translated = OpSpTypeNode::translate(self.op);
        let ret_type = first(op_type).operator_return_type(translated, info)?;
        Ok(ret_type.unwrap_or_else(|| vec![info.builtins().throws_type().clone()]))
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        if let Option::Some(constant) = self.constant_return(info)? {
            Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())))
        } else if self.op == OperatorTypeNode::NotEquals {
            self.convert_not_equals(info)
        } else if let Option::Some(x) = self.optimize_constant(info)? {
            Ok(x)
        } else {
            self.convert_inner(info)
        }
    }

    pub fn convert_with_as(&self) -> CompileResult<(BytecodeList, TypeObject)> {
        Err(as_exception(&self.line_info).into())
    }

    fn convert_inner(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert!(self.args.len() > 2);
        assert_ne!(self.op, OperatorTypeNode::BoolNot);
        let op_count = self.args.len();
        let mut first_converter = self.args[0].get_argument().test_converter(1);
        let mut op_type = first(first_converter.return_type(info)?);
        let mut bytes = first_converter.convert(info)?;
        let (mut previous_arg, args) = self.args.split_first().unwrap();
        for arg in args {
            let ret_type = first(TestConverter::return_type(info, arg.get_argument(), 1)?);
            let op = OpSpTypeNode::translate(self.op);
            if op_type.operator_return_type(op, info)?.is_some() {
                return Err(CompilerException::of(
                    format!(
                        "'{}' returns type '{}', which has no overloaded '{}'",
                        "", // FIXME: previous_arg,
                        op_type.name(),
                        self.op.sequence(),
                    ),
                    previous_arg,
                )
                .into());
            } else if op_type
                .operator_info(op, info)?
                .unwrap()
                .matches(&[Argument::new(String::new(), ret_type.clone())])
            {
                return Err(CompilerException::of(
                    format!(
                        "Cannot call '{}'.{} on type '{}'",
                        op_type.name(),
                        op.sequence(),
                        ret_type.name()
                    ),
                    previous_arg,
                )
                .into());
            }
            op_type = first(op_type.operator_return_type(op, info)?.unwrap());
            previous_arg = arg;
            bytes.extend(TestConverter::bytes(arg.get_argument(), info, 1)?);
        }
        let bytecode = operator_bytecode(self.op);
        if op_count == self.op_argc() {
            bytes.add(bytecode);
        } else if is_mandatory(self.op) {
            return Err(CompilerException::of(
                format!(
                    "Cannot call operator '{}' with {} operands (expected exactly {})",
                    self.op.sequence(),
                    op_count,
                    self.op_argc()
                ),
                &self.line_info,
            )
            .into());
        } else {
            return Err(CompilerTodoError::of(
                "Operators with > 2 operands not yet supported",
                &self.line_info,
            )
            .into());
        }
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }

    fn convert_not_equals(&self, info: &mut CompilerInfo) -> CompileBytes {
        let op_count = self.args.len();
        assert_eq!((op_count, self.op), (2, OperatorTypeNode::NotEquals));
        let mut bytes = TestConverter::bytes(self.args[0].get_argument(), info, 1)?;
        bytes.extend(TestConverter::bytes(self.args[1].get_argument(), info, 1)?);
        if op_count == self.op_argc() {
            bytes.add(Bytecode::Equal());
        } else {
            return Err(CompilerTodoError::of(
                "Operators with > 2 operands not yet supported",
                &self.line_info,
            )
            .into());
        }
        bytes.add(Bytecode::BoolNot());
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }

    fn optimize_constant(&self, info: &mut CompilerInfo) -> CompileResult<Option<BytecodeList>> {
        if self.args.len() != 2 {
            return Ok(None);
        }
        let first_arg = self.args[0].get_argument();
        let second_arg = self.args[1].get_argument();
        let first_type = TestConverter::return_type(info, first_arg, 1)?;
        let second_type = TestConverter::return_type(info, second_arg, 1)?;
        let int_type = info.builtins().int_type();
        if !int_type.is_superclass(&first(first_type))
            || !int_type.is_superclass(&first(second_type))
        {
            Ok(None)
        } else if let Option::Some(second) =
            TestConverter::constant_return(second_arg, info, 1)?.and_then(|x| x.try_into().ok())
        {
            self.second_constant(info, first_arg, second).map(Some)
        } else if let Option::Some(first) =
            TestConverter::constant_return(first_arg, info, 1)?.and_then(|x| x.try_into().ok())
        {
            self.first_constant(info, second_arg, first).map(Some)
        } else {
            Ok(None)
        }
    }

    fn second_constant(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        match self.op {
            OperatorTypeNode::Add => self.convert_addition(info, argument, constant),
            OperatorTypeNode::Subtract => self.convert_subtraction(info, argument, constant),
            OperatorTypeNode::Multiply => self.convert_multiply(info, argument, constant),
            OperatorTypeNode::Divide => self.convert_divide(info, argument, constant),
            OperatorTypeNode::FloorDiv => self.convert_floor_div(info, argument, constant),
            OperatorTypeNode::Power => self.convert_power(info, argument, constant),
            OperatorTypeNode::LeftBitshift => self.convert_left_bs(info, argument, constant),
            OperatorTypeNode::RightBitshift => self.convert_right_bs(info, argument, constant),
            OperatorTypeNode::BitwiseAnd => self.convert_bw_and(info, argument, constant),
            OperatorTypeNode::BitwiseOr => self.convert_bw_or(info, argument, constant),
            OperatorTypeNode::BitwiseXor => self.convert_bw_xor(info, argument, constant),
            OperatorTypeNode::Modulo => self.convert_mod(info, argument, constant),
            _ => self.one_constant(info, argument, constant),
        }
    }

    fn first_constant(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        match self.op {
            OperatorTypeNode::Add => self.convert_addition(info, argument, constant),
            OperatorTypeNode::Multiply => self.convert_multiply(info, argument, constant),
            OperatorTypeNode::Power => self.convert_power(info, argument, constant),
            OperatorTypeNode::BitwiseAnd => self.convert_bw_and(info, argument, constant),
            OperatorTypeNode::BitwiseOr => self.convert_bw_or(info, argument, constant),
            OperatorTypeNode::BitwiseXor => self.convert_bw_xor(info, argument, constant),
            _ => self.convert_inner(info),
        }
    }

    // FIXME: Run code for side effects

    fn convert_addition(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        if constant.big_value().is_zero() {
            TestConverter::bytes(argument, info, 1)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_subtraction(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        if constant.big_value().is_zero() {
            TestConverter::bytes(argument, info, 1)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_multiply(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            let mut bytes = BytecodeList::new();
            bytes.add(Bytecode::LoadConst(0u32.into()));
            Ok(bytes)
        } else if big_value.is_one() {
            TestConverter::bytes(argument, info, 1)
        } else if is_minus_one(&big_value) {
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::UMinus());
            Ok(bytes)
        } else if big_value.is_positive() && big_value.magnitude().count_ones() == 1 {
            // TODO: big_value.magnitude().is_power_of_two() (num_bigint#212)
            let power_of_two = big_value.magnitude().bits();
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::LoadConst(power_of_two.into()));
            bytes.add(Bytecode::LBitshift());
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_divide(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            warning::warn(
                "Division by the constant 0 will always result in an error",
                WarningType::ZeroDivision,
                info,
                &self.line_info,
            )?;
            self.convert_zero_div(info, argument, "divide")
        } else if big_value.is_one() {
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::LoadConst(
                info.builtins().dec_constant().clone().into(),
            ));
            bytes.add(Bytecode::CallTos(1.into()));
            Ok(bytes)
        } else if is_minus_one(&big_value) {
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::UMinus());
            bytes.add(Bytecode::LoadConst(
                info.builtins().dec_constant().clone().into(),
            ));
            bytes.add(Bytecode::CallTos(1.into()));
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_floor_div(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            warning::warn(
                "Division by the constant 0 will always result in an error",
                WarningType::ZeroDivision,
                info,
                &self.line_info,
            )?;
            self.convert_zero_div(info, argument, "divide")
        } else if big_value.is_one() {
            TestConverter::bytes(argument, info, 1)
        } else if is_minus_one(&big_value) {
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::UMinus());
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_power(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            let mut bytes = BytecodeList::new();
            bytes.add(Bytecode::LoadConst(1.into()));
            Ok(bytes)
        } else if big_value.is_one() {
            TestConverter::bytes(argument, info, 1)
        } else if big_value.is_negative() {
            let mut bytes = BytecodeList::new();
            bytes.add(Bytecode::LoadConst(0.into()));
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_left_bs(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            TestConverter::bytes(argument, info, 1)
        } else if big_value.is_positive() && big_value.magnitude().bits() >= 64 {
            warning::warn(
                "Shift too big to compute properly, will fail at runtime",
                WarningType::NoType,
                info,
                &self.line_info,
            )?;
            self.one_constant(info, argument, constant)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_right_bs(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            TestConverter::bytes(argument, info, 1)
        } else if big_value.is_negative() && big_value.magnitude().bits() >= 64 {
            warning::warn(
                "Shift too big to compute properly, will fail at runtime",
                WarningType::NoType,
                info,
                &self.line_info,
            )?;
            self.one_constant(info, argument, constant)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_bw_and(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            let mut bytes = BytecodeList::new();
            bytes.add(Bytecode::LoadConst(1.into()));
            Ok(bytes)
        } else if is_minus_one(&big_value) {
            TestConverter::bytes(argument, info, 1)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_bw_or(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            TestConverter::bytes(argument, info, 1)
        } else if is_minus_one(&big_value) {
            let mut bytes = BytecodeList::new();
            bytes.add(Bytecode::LoadConst((-1).into()));
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_bw_xor(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            TestConverter::bytes(argument, info, 1)
        } else if is_minus_one(&big_value) {
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::BitwiseNot());
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn convert_mod(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        let big_value = constant.big_value();
        if big_value.is_zero() {
            warning::warn(
                "Modulo by the constant 0 will always result in an error",
                WarningType::ZeroDivision,
                info,
                LineInfo::empty(),
            )?;
            self.convert_zero_div(info, argument, "modulo")
        } else if big_value.is_one() {
            let mut bytes = BytecodeList::new();
            bytes.add(Bytecode::LoadConst(0.into()));
            Ok(bytes)
        } else if big_value.is_positive() && big_value.magnitude().count_ones() == 1 {
            // TODO: big_value.magnitude().is_power_of_two() (num_bigint#212)
            let less_one: BigInt = big_value.into_owned() - 1;
            let mut bytes = TestConverter::bytes(argument, info, 1)?;
            bytes.add(Bytecode::LoadConst(less_one.into()));
            bytes.add(Bytecode::BitwiseAnd());
            Ok(bytes)
        } else {
            self.one_constant(info, argument, constant)
        }
    }

    fn one_constant(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        constant: NumberConstant,
    ) -> CompileBytes {
        debug_assert!(!self.op.is_unary());
        let mut bytes = TestConverter::bytes(argument, info, 1)?;
        bytes.add(Bytecode::LoadConst(constant.into()));
        bytes.add(operator_bytecode(self.op));
        Ok(bytes)
    }

    fn convert_zero_div(
        &self,
        info: &mut CompilerInfo,
        argument: &TestNode,
        message: &str,
    ) -> CompileBytes {
        let mut bytes = TestConverter::bytes(argument, info, 1)?;
        bytes.add(Bytecode::PopTop());
        let arith_err = info.builtins().arith_error_const();
        bytes.add(Bytecode::LoadConst(arith_err.clone().into()));
        let message = format!("Cannot {} by zero", message);
        bytes.add(Bytecode::LoadConst(message.into()));
        bytes.add(Bytecode::ThrowQuick(1.into()));
        Ok(bytes)
    }

    fn op_argc(&self) -> usize {
        if self.op.is_unary() {
            1
        } else {
            2
        }
    }
}

fn is_minus_one(value: &BigInt) -> bool {
    value.sign() == Sign::Minus && value.magnitude().is_one()
}
