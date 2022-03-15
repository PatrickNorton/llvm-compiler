use derive_new::new;

use crate::converter::error::CompilerException;
use crate::converter::type_loader::TypeLoader;
use crate::parser::line_info::Lined;
use crate::parser::typedef::TypedefStatementNode;

use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase};
use super::CompileBytes;

#[derive(Debug, new)]
pub struct TypedefConverter<'a> {
    node: &'a TypedefStatementNode,
}

impl<'a> ConverterBase for TypedefConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let ty = info
            .convert_type(self.node.get_type())?
            .typedef_as(self.node.get_name().str_name().to_string());
        info.add_type(ty.clone());
        info.check_definition(&ty.name(), self.node)?;
        let constant = TypeLoader::type_constant(self.node.line_info().clone(), &ty, info)?
            .ok_or_else(|| CompilerException::of("Cannot typedef local types", self.node))?;
        info.add_constant_variable(
            ty.name().to_string(),
            ty,
            constant,
            self.node.line_info().clone(),
        );
        Ok(BytecodeList::new())
    }
}

base_convertible!(TypedefStatementNode, TypedefConverter);
