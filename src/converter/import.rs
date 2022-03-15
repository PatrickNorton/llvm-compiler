use derive_new::new;

use crate::parser::import::{ImportExportNode, ImportExportType};
use crate::parser::line_info::Lined;

use super::builtins::OBJECT;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::ImportConstant;
use super::convertible::{base_convertible, ConverterBase};
use super::error::CompilerException;
use super::CompileBytes;

#[derive(Debug, new)]
pub struct ImportExportConverter<'a> {
    node: &'a ImportExportNode,
}

impl<'a> ConverterBase for ImportExportConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        match self.node.get_type() {
            ImportExportType::Import | ImportExportType::Typeget => self.add_import(info),
            ImportExportType::Export => Err(CompilerException::of(
                "Invalid position for export statement",
                self.node,
            )
            .into()),
        }
    }
}

impl<'a> ImportExportConverter<'a> {
    fn add_import(&self, info: &mut CompilerInfo) -> CompileBytes {
        assert!(matches!(
            self.node.get_type(),
            ImportExportType::Import | ImportExportType::Typeget
        ));
        let imports = info.import_handler_mut().add_import(self.node)?;
        for (name, index) in imports {
            let constant = ImportConstant::new(index, name.clone());
            info.check_definition(&name, self.node)?;
            let import_type = OBJECT.into(); // FIXME: Variable should have a type
            info.add_constant_variable(
                name,
                import_type,
                constant.into(),
                self.node.line_info().clone(),
            );
        }
        Ok(BytecodeList::new())
    }
}

base_convertible!(ImportExportNode, ImportExportConverter);
