use derive_new::new;

use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::{OptionTypeObject, TypeObject};
use crate::converter::warning::{self, WarningType};
use crate::converter::{builtins, CompileBytes, CompileConstant, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator::OperatorTypeNode;
use crate::parser::variable::VariableNode;
use crate::util::first;

use super::{as_exception, default_constant};

#[derive(Debug, new)]
pub(super) struct IsConverter<'a> {
    is_type: bool,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> IsConverter<'a> {
    pub fn constant_return(&self, info: &mut CompilerInfo) -> CompileConstant {
        default_constant(
            if self.is_type {
                OperatorTypeNode::Is
            } else {
                OperatorTypeNode::IsNot
            },
            info,
            self.args,
        )
    }

    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().bool_type().clone()])
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count > 1 {
            Err(CompilerException::of(
                format!("'is' only returns 1 value, {} expected", self.ret_count),
                &self.line_info,
            )
            .into())
        } else {
            match self.args.len() {
                0 | 1 => self.convert_0(info),
                2 => self.convert_2(info),
                _ => self.convert_many(info),
            }
        }
    }

    pub fn convert_with_as(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, TypeObject)> {
        if self.args.len() != 2 {
            return Err(CompilerException::with_note(
                format!(
                    "'is' comparison with 'as' clause may only have 2 parameters, not {}",
                    self.args.len()
                ),
                "Only statements of the form 'x is not null' are allowed in an 'as' clause",
                &self.line_info,
            )
            .into());
        }
        if self.is_type {
            return Err(as_exception(&self.line_info).into());
        }
        let arg0 = self.args[0].get_argument();
        let arg1 = self.args[1].get_argument();
        if !<&VariableNode>::try_from(arg1).map_or_else(|_| false, |x| x.get_name() == "null") {
            return Err(CompilerException::with_note(
                "Cannot use 'as' here",
                "'is not' comparison must be done to null when with an 'as' clause",
                arg1,
            )
            .into());
        }
        let cond_type = first(TestConverter::return_type(info, arg0, 1)?);
        if OptionTypeObject::try_from(cond_type.clone()).is_err() {
            warning::warn(
                "Using 'is not null' comparison on non-nullable variable",
                WarningType::TrivialValue,
                info,
                arg0,
            )?;
            let mut bytes = TestConverter::bytes(arg0, info, 1)?;
            bytes.add(Bytecode::LoadConst(builtins::TRUE.into()));
            Ok((bytes, cond_type))
        } else if cond_type == *info.builtins().null_type() {
            warning::warn_note(
                "Using 'is not null' comparison on variable that must be null",
                "This variable has type 'null', so 'is not null' cannot be true",
                WarningType::TrivialValue,
                info,
                arg0,
            )?;
            let mut bytes = TestConverter::bytes(arg0, info, 1)?;
            bytes.add(Bytecode::LoadConst(builtins::FALSE.into()));
            Ok((bytes, cond_type))
        } else {
            let as_type = cond_type.strip_null();
            let mut bytes = TestConverter::bytes(arg0, info, 1)?;
            bytes.add(Bytecode::DupTop());
            bytes.extend(TestConverter::bytes(arg1, info, 1)?);
            bytes.add(Bytecode::Identical());
            bytes.add(Bytecode::BoolNot());
            Ok((bytes, as_type))
        }
    }

    fn convert_0(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert!(self.args.is_empty() || self.args.len() == 1);
        warning::warn(
            format!(
                "'{}' with fewer than 2 operands will always be {}",
                self.is_name(),
                self.is_type
            ),
            WarningType::TrivialValue,
            info,
            &self.line_info,
        )?;
        let mut bytes = if self.args.is_empty() {
            BytecodeList::new()
        } else {
            TestConverter::bytes(self.args[0].get_argument(), info, 1)?
        };
        bytes.add(Bytecode::LoadConst(self.is_type.into()));
        Ok(bytes)
    }

    fn convert_2(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert_eq!(self.args.len(), 2);
        if let Option::Some(constant) = self.get_constant(info)? {
            return Ok(constant);
        }
        let mut bytes = TestConverter::bytes(self.args[0].get_argument(), info, 1)?;
        bytes.extend(TestConverter::bytes(self.args[1].get_argument(), info, 1)?);
        bytes.add(Bytecode::Identical());
        if !self.is_type {
            bytes.add(Bytecode::BoolNot());
        }
        Ok(bytes)
    }

    fn convert_many(&self, info: &mut CompilerInfo) -> CompileBytes {
        if !self.is_type {
            return Err(CompilerException::of(
                format!(
                    "'is not' requires 2 or fewer values (got {})",
                    self.args.len()
                ),
                &self.line_info,
            )
            .into());
        }
        if let Option::Some(bytes) = self.get_constant(info)? {
            return Ok(bytes);
        }
        // Since object identity is transitive (and non-overloadable, so
        // transitivity is guaranteed), it's much easier if we simply compare
        // everything to the first object given. Since nothing in life is ever
        // simple, we have to do some shenanigans with the stack to ensure
        // everything winds up in the right place.
        let mut bytes = BytecodeList::new();
        bytes.add(Bytecode::LoadConst(builtins::TRUE.into()));
        bytes.extend(TestConverter::bytes(self.args[0].get_argument(), info, 1)?);
        let last_index = self.args.len() - 1;
        for arg in &self.args[1..last_index] {
            bytes.add(Bytecode::DupTop());
            bytes.extend(TestConverter::bytes(arg.get_argument(), info, 1)?);
            bytes.add(Bytecode::Identical()); // Compare the values
            bytes.add(Bytecode::Swap3()); // Bring up the next one (below result & operands[0])
            bytes.add(Bytecode::BoolAnd()); // 'and' them together
            bytes.add(Bytecode::Swap2()); // Put operands[0] back on top}
        }
        // Last one is special b/c cleanup...
        // No need to duplicate operands[0], and thus no need to swap to get
        // around it
        bytes.extend(TestConverter::bytes(
            self.args[last_index].get_argument(),
            info,
            1,
        )?);
        bytes.add(Bytecode::Identical());
        bytes.add(Bytecode::BoolAnd());
        Ok(bytes)
    }

    fn get_constant(&self, info: &mut CompilerInfo) -> CompileResult<Option<BytecodeList>> {
        Ok(self
            .constant_return(info)?
            .map(|x| BytecodeList::of(Bytecode::LoadConst(x.into()))))
    }

    fn is_name(&self) -> &'static str {
        if self.is_type {
            "is"
        } else {
            "is not"
        }
    }
}
