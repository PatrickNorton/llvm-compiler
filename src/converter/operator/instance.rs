use derive_new::new;

use crate::converter::bytecode::Bytecode;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::{TypeObject, TypeTypeObject};
use crate::converter::{CompileBytes, CompileResult, CompileTypes};
use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::LineInfo;
use crate::util::first;

#[derive(Debug, new)]
pub(super) struct InstanceConverter<'a> {
    instance_type: bool,
    args: &'a [ArgumentNode],
    line_info: LineInfo,
    ret_count: u16,
}

impl<'a> InstanceConverter<'a> {
    pub fn return_type(&self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().bool_type().clone()])
    }

    pub fn convert(&self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_inner(info, false).map(|x| x.0)
    }

    pub fn convert_with_as(
        &self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, TypeObject)> {
        self.convert_inner(info, true)
    }

    fn convert_inner(
        &self,
        info: &mut CompilerInfo,
        dup_first: bool,
    ) -> CompileResult<(BytecodeList, TypeObject)> {
        if self.args.len() != 2 {
            return Err(CompilerException::of(
                format!(
                    "'instanceof' operator requirest 2 arguments, not {}",
                    self.args.len()
                ),
                &self.line_info,
            )
            .into());
        } else if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!(
                    "'instanceof' operator returns 1 value, not {}",
                    self.ret_count
                ),
                &self.line_info,
            )
            .into());
        }
        let arg0 = self.args[0].get_argument();
        let arg1 = self.args[1].get_argument();
        let instance_cls =
            match TypeTypeObject::try_from(first(TestConverter::return_type(info, arg1, 1)?)) {
                Result::Ok(x) => x.represented_type().clone(),
                Result::Err(_) => return Err(CompilerException::of(
                    "'instanceof' operator requires second argument to be an instance of 'type'",
                    arg1,
                )
                .into()),
            };
        let mut bytes = TestConverter::bytes(arg0, info, 1)?;
        if dup_first {
            bytes.add(Bytecode::DupTop());
        }
        bytes.extend(TestConverter::bytes(arg1, info, 1)?);
        bytes.add(Bytecode::Instanceof());
        if !self.instance_type {
            bytes.add(Bytecode::BoolNot());
        }
        Ok((bytes, instance_cls))
    }
}
