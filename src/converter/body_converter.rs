use crate::parser::stmt_body::StatementBodyNode;

use super::base_converter::BaseConverter;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase};
use super::diverge::DivergingInfo;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult};

#[derive(Debug)]
pub struct BodyConverter<'a> {
    node: &'a StatementBodyNode,
}

impl<'a> BodyConverter<'a> {
    pub fn new(node: &'a StatementBodyNode) -> Self {
        Self { node }
    }
}

impl<'a> ConverterBase for BodyConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.convert_with_return(info).map(|x| x.0)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        info.add_stack_frame();
        let mut returned = DivergingInfo::new();
        let mut warned = false;
        let mut bytes = BytecodeList::new();
        for statement in self.node {
            if !warned && returned.will_diverge() {
                warning::warn(
                    "Unreachable statement",
                    WarningType::Unreachable,
                    info,
                    statement,
                )?;
                warned = true;
            }
            let (bytecode, rets) = BaseConverter::bytes_with_return(statement, info)?;
            if !returned.will_diverge() {
                // When diverging is inevitable, don't add more information
                // This helps analysis with infinite loops and 'continue'
                returned.or_with(rets);
            }
            bytes.extend(bytecode);
        }
        info.remove_stack_frame();
        Ok((bytes, returned))
    }
}

base_convertible!(StatementBodyNode, BodyConverter);
