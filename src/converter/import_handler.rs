use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::iter::zip;
use std::path::{Path, PathBuf};

use derive_new::new;
use either::Either;
use indexmap::IndexSet;

use crate::arguments::CLArgs;
use crate::parser::import::{ImportExportNode, ImportExportType};
use crate::parser::line_info::{LineInfo, Lined};
use crate::util::levenshtein;

use super::constant::LangConstant;
use super::error::CompilerException;
use super::file_types::FileTypes;
use super::global_info::GlobalCompilerInfo;
use super::linker::Linker;
use super::permission::PermissionLevel;
use super::type_obj::TypeObject;
use super::{stdlib_path, CompileResult};

// TODO: Put some of these PathBufs in Arcs

#[derive(Debug)]
pub struct ImportHandler {
    path: PathBuf,
    permissions: PermissionLevel,
    exports: HashMap<String, Option<TypeObject>>,
    from_exports: HashMap<String, PathBuf>,
    export_constants: HashMap<String, Option<LangConstant>>,
    imports: HashMap<PathBuf, ImportInfo>,
    import_strings: IndexSet<String>,
    wildcard_exports: HashSet<PathBuf>,
}

#[derive(Debug, new)]
pub struct ImportInfo {
    line_info: LineInfo,
    module_name: String,
    index: u32,
    names: Vec<String>,
    as_names: Option<Vec<String>>,
}

#[derive(Debug, new)]
pub struct ExportInfo {
    path: PathBuf,
    exports: HashMap<String, Option<TypeObject>>,
    from_exports: HashMap<String, PathBuf>,
    export_constants: HashMap<String, Option<LangConstant>>,
    wildcard_exports: HashSet<PathBuf>,
}

impl ImportHandler {
    pub fn new(path: PathBuf, permissions: PermissionLevel) -> Self {
        Self {
            path,
            permissions,
            exports: HashMap::new(),
            from_exports: HashMap::new(),
            export_constants: HashMap::new(),
            imports: HashMap::new(),
            import_strings: IndexSet::new(),
            wildcard_exports: HashSet::new(),
        }
    }

    pub fn permission_level(&self) -> PermissionLevel {
        self.permissions
    }

    pub fn get_path(&self) -> &Path {
        &self.path
    }

    pub fn export_info(&self) -> ExportInfo {
        ExportInfo::new(
            self.path.clone(),
            self.exports.clone(),
            self.from_exports.clone(),
            self.export_constants.clone(),
            self.wildcard_exports.clone(),
        )
    }

    pub fn from_file_types(file_types: &FileTypes) -> CompileResult<Self> {
        // FIXME: Get export types
        let mut imports = HashMap::with_capacity(file_types.imports.len());
        let mut import_strings = IndexSet::new();
        for (path, import) in &file_types.imports {
            let mut names = Vec::with_capacity(import.len());
            let mut as_names: Option<Vec<_>> = None;
            let mut import_info = None;
            // TODO: Double-definition check
            for info in import {
                if import_info.is_none() {
                    import_info = Some(info.line_info().clone())
                }
                match (&mut as_names, &info.as_name) {
                    (Some(as_names), Some(as_name)) => as_names.push(as_name.clone()),
                    (Some(as_names), None) => as_names.push(info.name.clone()),
                    (None, Some(as_name)) => {
                        let as_names = as_names.insert(names.clone());
                        as_names.push(as_name.clone());
                    }
                    (None, None) => {}
                }
                names.push(info.name.clone());
                import_strings.insert(info.name.clone());
            }
            let info = ImportInfo::new(
                import_info.unwrap_or_default(),
                String::new(),
                0,
                names,
                as_names,
            );
            imports.insert(path.clone(), info);
        }
        let mut exports = HashMap::new();
        let mut from_exports = HashMap::new();
        for (name, info) in &file_types.exports {
            match info {
                Either::Left(ty) => {
                    exports.insert(
                        name.clone(),
                        ty.clone()
                            .or_else(|| file_types.types.get(name).map(|x| x.0.clone())),
                    );
                }
                Either::Right(path) => {
                    from_exports.insert(name.clone(), path.clone());
                }
            };
        }
        Ok(Self {
            path: file_types.path.clone(),
            permissions: file_types.permissions,
            exports,
            from_exports,
            export_constants: HashMap::new(),
            imports,
            import_strings,
            wildcard_exports: file_types.wildcard_exports.clone(),
        })
    }

    pub fn get_exports(&self) -> &HashMap<String, Option<TypeObject>> {
        &self.exports
    }

    pub fn set_export_type(&mut self, name: &str, ty: TypeObject) {
        *self.exports.get_mut(name).unwrap() = Some(ty);
    }

    pub fn import_infos(&self) -> &HashMap<PathBuf, ImportInfo> {
        &self.imports
    }

    pub fn add_import(&mut self, node: &ImportExportNode) -> CompileResult<HashMap<String, u16>> {
        assert!(matches!(
            node.get_type(),
            ImportExportType::Import | ImportExportType::Typeget
        ));
        todo!()
    }

    pub fn imported_types(
        &self,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<HashMap<String, (TypeObject, LineInfo)>> {
        let mut imported_types = HashMap::new();
        for (path, import_info) in &self.imports {
            let strings = &import_info.names;
            let as_names = import_info.as_names.as_ref().unwrap_or(&import_info.names);
            let export_handler = global_info.export_info(path);
            if strings == &["*"] {
                for (name, value) in export_handler.exported_types(import_info, global_info)? {
                    imported_types.insert(name, (value, import_info.line_info.clone()));
                }
            } else {
                for (string, as_name) in zip(strings, as_names) {
                    let type_val = export_handler.exported_type(
                        string,
                        import_info.line_info(),
                        global_info,
                        &mut Vec::new(),
                    )?;
                    if let Option::Some(ty) = type_val {
                        imported_types
                            .insert(as_name.clone(), (ty, import_info.line_info().clone()));
                    }
                }
            }
        }
        Ok(imported_types)
    }

    pub fn imported_constant(
        &self,
        line_info: impl Lined,
        file: &Path,
        name: &str,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Option<LangConstant>> {
        let handler = global_info.export_info(file);
        handler.exported_const(name, line_info.line_info(), global_info, &mut Vec::new())
    }

    pub fn imported_type(
        &self,
        line_info: impl Lined,
        file: &Path,
        name: &str,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<TypeObject> {
        let handler = global_info.export_info(file);
        handler.type_of_export(name, line_info.line_info(), global_info, &mut Vec::new())
    }

    pub fn set_from_linker(&mut self, linker: Linker) -> CompileResult<()> {
        for (name, ty) in linker.globals {
            if let Option::Some(export) = self.exports.get_mut(&name) {
                *export = Some(ty);
                let constant = linker.constants.get(&name).cloned();
                self.export_constants.insert(name, constant);
            }
        }
        for export_name in self.exports.keys() {
            if let Entry::Vacant(entry) = self.export_constants.entry(export_name.clone()) {
                entry.insert(linker.constants.get(export_name).cloned());
            }
        }
        Ok(())
    }
}

impl ImportInfo {
    pub fn get_names(&self) -> &[String] {
        &self.names
    }

    pub fn get_module_name(&self) -> &str {
        &self.module_name
    }

    pub fn get_as_names(&self) -> &Option<Vec<String>> {
        &self.as_names
    }

    pub fn merge(&mut self, names: Vec<String>, as_names: Option<Vec<String>>) {
        if self.as_names.is_none() && as_names.is_none() {
            self.names.extend(names);
        } else {
            self.as_names
                .get_or_insert_with(|| self.names.clone())
                .extend(as_names.unwrap_or_else(|| names.clone()));
            self.names.extend(names);
        }
    }
}

impl ExportInfo {
    pub fn get_exports(&self) -> &HashMap<String, Option<TypeObject>> {
        &self.exports
    }

    fn exported_types(
        &self,
        line_info: impl Lined,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<HashMap<String, TypeObject>> {
        // FIXME: self.exports is not a superset of self.from_exports
        let mut result = HashMap::new();
        let line_info = line_info.line_info();
        for (name, type_val) in &self.exports {
            if let Option::Some(ty) = type_val {
                result.insert(name.clone(), ty.clone());
            } else if let Option::Some(type_obj) =
                self.exported_type(name, line_info, global_info, &mut Vec::new())?
            {
                result.insert(name.clone(), type_obj);
            }
        }
        for name in self.from_exports.keys() {
            if let Option::Some(type_obj) =
                self.exported_type(name, line_info, global_info, &mut Vec::new())?
            {
                result.insert(name.clone(), type_obj);
            }
        }
        for path in &self.wildcard_exports {
            let exports = global_info.export_info(path);
            result.extend(exports.exported_types(line_info, global_info)?);
        }
        Ok(result)
    }

    fn exported_type<'a, 'b>(
        &self,
        name: &'b str,
        line_info: &'a LineInfo,
        global_info: &GlobalCompilerInfo,
        previous_files: &mut Vec<(&'a LineInfo, &'b str)>,
    ) -> CompileResult<Option<TypeObject>> {
        assert_ne!(name, "*");
        self.check_circular(name, previous_files)?;
        if let Option::Some(export) = self.exports.get(name) {
            if let Option::Some(TypeObject::Type(ty)) = export {
                Ok(Some(ty.represented_type().clone()))
            } else if let Option::Some(path) = self.from_exports.get(name) {
                previous_files.push((line_info, name));
                global_info.export_info(path).exported_type(
                    name,
                    line_info,
                    global_info,
                    previous_files,
                )
            } else {
                Ok(None)
            }
        } else {
            previous_files.push((line_info, name));
            for path in &self.wildcard_exports {
                let handler = global_info.export_info(path);
                if let Result::Ok(ty) =
                    handler.exported_type(name, line_info, global_info, previous_files)
                {
                    return Ok(ty);
                }
            }
            Err(self.export_error(line_info, name, global_info).into())
        }
    }

    fn exported_const<'a, 'b>(
        &self,
        name: &'b str,
        line_info: &'a LineInfo,
        global_info: &GlobalCompilerInfo,
        previous_files: &mut Vec<(&'a LineInfo, &'b str)>,
    ) -> CompileResult<Option<LangConstant>> {
        assert_ne!(name, "*");
        self.check_circular(name, previous_files)?;
        if !self.exports.contains_key(name) {
            previous_files.push((line_info, name));
            for path in &self.wildcard_exports {
                let handler = global_info.export_info(path);
                match handler.exported_const(name, line_info, global_info, previous_files) {
                    Result::Ok(res) => return Ok(res),
                    Result::Err(_) => {}
                }
            }
            return Err(self.export_error(line_info, name, global_info).into());
        }
        let export = self.export_constants.get(name);
        if let Option::Some(Option::Some(export)) = export {
            Ok(Some(export.clone()))
        } else if let Option::Some(path) = self.from_exports.get(name) {
            previous_files.push((line_info, name));
            global_info.export_info(path).exported_const(
                name,
                line_info,
                global_info,
                previous_files,
            )
        } else if export.is_some() {
            Ok(None)
        } else {
            Err(self.export_error(line_info, name, global_info).into())
        }
    }

    fn type_of_export<'a, 'b>(
        &self,
        name: &'b str,
        line_info: &'a LineInfo,
        global_info: &GlobalCompilerInfo,
        previous_files: &mut Vec<(&'a LineInfo, &'b str)>,
    ) -> CompileResult<TypeObject> {
        assert_ne!(name, "*");
        self.check_circular(name, previous_files)?;
        if !self.exports.contains_key(name) {
            previous_files.push((line_info, name));
            for path in &self.wildcard_exports {
                let handler = global_info.export_info(path);
                match handler.type_of_export(name, line_info, global_info, previous_files) {
                    Result::Ok(res) => return Ok(res),
                    Result::Err(_) => {}
                }
            }
            return Err(self.export_error(line_info, name, global_info).into());
        }
        let export = self.exports.get(name);
        if let Option::Some(export) = export {
            Ok(export.clone().unwrap())
        } else if let Option::Some(path) = self.from_exports.get(name) {
            previous_files.push((line_info, name));
            global_info.export_info(path).type_of_export(
                name,
                line_info,
                global_info,
                previous_files,
            )
        } else {
            Err(self.export_error(line_info, name, global_info).into())
        }
    }

    fn export_error(
        &self,
        line_info: impl Lined,
        name: &str,
        global_info: &GlobalCompilerInfo,
    ) -> CompilerException {
        if let Option::Some(closest) =
            self.closest_exported_name(name, &mut HashSet::new(), global_info)
        {
            CompilerException::of(
                format!(
                    "No value '{}' was exported from file '{}'\nDid you mean {}?",
                    name,
                    self.path.display(),
                    closest
                ),
                line_info,
            )
        } else {
            CompilerException::of(
                format!(
                    "No value '{}' was exported from file '{}'",
                    name,
                    self.path.display()
                ),
                line_info,
            )
        }
    }

    fn closest_exported_name<'a>(
        &'a self,
        name: &str,
        previous_files: &mut HashSet<&'a Path>,
        global_info: &'a GlobalCompilerInfo,
    ) -> Option<&'a str> {
        if let Option::Some(export) = levenshtein::closest_name(name, self.exports.keys()) {
            return Some(export);
        }
        for path in &self.wildcard_exports {
            if !previous_files.contains(path.as_path()) {
                previous_files.insert(path);
                let handler = global_info.export_info(path);
                if let Option::Some(exported) =
                    handler.closest_exported_name(name, previous_files, global_info)
                {
                    return Some(exported);
                }
            }
        }
        None
    }

    fn check_circular(
        &self,
        name: &str,
        previous_files: &[(&LineInfo, &str)],
    ) -> CompileResult<()> {
        for (info, prev_name) in previous_files {
            if info.get_path() == self.path && prev_name == &name {
                return Err(CompilerException::of(
                    format!("Circular import of '{}': not defined in any file", name),
                    info,
                )
                .into());
            }
        }
        Ok(())
    }
}

pub fn builtins_file(args: &CLArgs) -> PathBuf {
    let mut path = stdlib_path(args);
    path.push("__builtins__.newlang");
    path
}

impl Lined for ImportInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
