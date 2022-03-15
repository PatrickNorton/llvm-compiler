use derive_new::new;

use crate::converter::bytecode::{Bytecode, ConstantBytecode, Label};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::TypeObject;
use crate::converter::{CompileBytes, CompileConstant, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator::OperatorTypeNode;

use super::as_exception;

#[derive(Debug, new)]
pub(super) struct BoolOpConverter<'a> {
    op: OperatorTypeNode,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> BoolOpConverter<'a> {
    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().bool_type().clone()])
    }

    pub fn constant_return(&self, info: &mut CompilerInfo) -> CompileConstant {
        match self.op {
            OperatorTypeNode::BoolAnd => Ok(self
                .bool_values(info)?
                .map(|x| x.into_iter().all(|x| x).into())),
            OperatorTypeNode::BoolOr => Ok(self
                .bool_values(info)?
                .map(|x| x.into_iter().any(|x| x).into())),
            OperatorTypeNode::BoolNot => Ok(if self.args.len() == 1 {
                self.bool_value(info, &self.args[0])?.map(|x| (!x).into())
            } else {
                None
            }),
            OperatorTypeNode::BoolXor => Ok(if self.args.len() == 2 {
                self.bool_values(info)?
                    .map(|x| x.into_iter().fold(false, |x, y| x ^ y).into())
            } else {
                None
            }),
            _ => unreachable!("Unexpected boolean operator: {}", self.op.sequence()),
        }
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        match self.op {
            OperatorTypeNode::BoolAnd | OperatorTypeNode::BoolOr => self.convert_bool_op(info),
            OperatorTypeNode::BoolNot => self.convert_bool_not(info),
            OperatorTypeNode::BoolXor => self.convert_bool_xor(info),
            _ => unreachable!("Unexpected boolean operator: {}", self.op.sequence()),
        }
    }

    pub fn convert_with_as(&self) -> CompileResult<(BytecodeList, TypeObject)> {
        Err(as_exception(&self.line_info).into())
    }

    fn convert_bool_op(&self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        bytes.add(Bytecode::LoadConst(ConstantBytecode::new(
            info.builtins().bool_constant().clone(),
        )));
        bytes.extend(TestConverter::bytes(self.args[0].get_argument(), info, 1)?);
        bytes.add(Bytecode::DupTop());
        let label = Label::new();
        bytes.add(Bytecode::jump_if(
            self.op == OperatorTypeNode::BoolOr,
            label.clone(),
        ));
        bytes.add(Bytecode::PopTop());
        bytes.extend(TestConverter::bytes(self.args[1].get_argument(), info, 1)?);
        bytes.add_label(label);
        bytes.add(Bytecode::CallTos(1.into()));
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }

    fn convert_bool_not(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.args.len() > 1 {
            Err(CompilerException::of(
                format!(
                    "'not' operator cannot have multiple operands, got {}",
                    self.args.len()
                ),
                &self.line_info,
            )
            .into())
        } else {
            let mut bytes = TestConverter::bytes(self.args[0].get_argument(), info, 1)?;
            bytes.add(Bytecode::BoolNot());
            Ok(bytes)
        }
    }

    fn convert_bool_xor(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.args.len() != 2 {
            Err(CompilerException::of(
                format!(
                    "'xor' operator must have 2 operands, got {}",
                    self.args.len()
                ),
                &self.line_info,
            )
            .into())
        } else {
            let mut bytes = TestConverter::bytes(self.args[0].get_argument(), info, 1)?;
            bytes.extend(TestConverter::bytes(self.args[1].get_argument(), info, 1)?);
            bytes.add(Bytecode::BoolXor());
            Ok(bytes)
        }
    }

    fn bool_values(&self, info: &mut CompilerInfo) -> CompileResult<Option<Vec<bool>>> {
        self.args
            .iter()
            .map(|arg| self.bool_value(info, arg))
            .collect()
    }

    fn bool_value(
        &self,
        info: &mut CompilerInfo,
        arg: &ArgumentNode,
    ) -> CompileResult<Option<bool>> {
        Ok(TestConverter::constant_return(arg.get_argument(), info, 1)?
            .and_then(|x| x.bool_value()))
    }
}
