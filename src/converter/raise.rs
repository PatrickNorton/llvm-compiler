use derive_new::new;

use crate::parser::raise_stmt::RaiseStatementNode;
use crate::util::first;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::error::CompilerTodoError;
use super::if_converter::IfConverter;
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct RaiseConverter<'a> {
    node: &'a RaiseStatementNode,
    ret_count: u16,
}

impl<'a> ConverterTest for RaiseConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        if self.ret_count > 0 && !self.node.get_cond().is_empty() {
            Err(self.return_error().into())
        } else {
            let throws = info.builtins().throws_type().clone();
            Ok(vec![throws; self.ret_count as usize])
        }
    }
}

impl<'a> ConverterBase for RaiseConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        if !self.node.get_from().is_empty() {
            return Err(
                CompilerTodoError::of("'from' clauses in raise statements", self.node).into(),
            );
        }
        if self.ret_count > 0 && !self.node.get_cond().is_empty() {
            return Err(self.return_error().into());
        }
        let cond_loc = IfConverter::add_jump(&mut bytes, self.node.get_cond(), info)?;
        assert!(cond_loc.is_none() || self.ret_count == 0);
        let mut converter = self.node.get_raised().test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        if !info.builtins().throwable().is_superclass(&ret_type) {
            return Err(CompilerException::of(
                format!(
                    "Expected subclass of 'Throwable' in raise statement body, got '{}'",
                    ret_type.name()
                ),
                self.node,
            )
            .into());
        }
        bytes.extend(converter.convert(info)?);
        bytes.add(Bytecode::Throw()); // TODO: ThrowQuick
        if let Option::Some(cond_loc) = cond_loc {
            bytes.add_label(cond_loc)
        }
        Ok(bytes)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut diverge = DivergingInfo::new();
        if self.node.get_cond().is_empty() {
            diverge.known_return()
        } else {
            diverge.possible_return()
        }
        Ok((self.convert(info)?, diverge))
    }
}

impl<'a> RaiseConverter<'a> {
    fn return_error(&self) -> CompilerException {
        CompilerException::of(
            "'raise' statement with trailing 'if' cannot be used as an expression",
            self.node,
        )
    }
}

test_convertible!(RaiseStatementNode, RaiseConverter);
