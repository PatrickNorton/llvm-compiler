use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::iter::zip;
use std::mem::take;
use std::path::{Path, PathBuf};

use derive_new::new;
use either::Either;
use indexmap::IndexSet;

use crate::arguments::CLArgs;
use crate::converter;
use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::import::{ImportExportNode, ImportExportType};
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::parse::TopNode;
use crate::parser::variable::VariableNode;
use crate::parser::Parser;
use crate::util::{levenshtein, reborrow_option};

use super::builtins::{self, ParsedBuiltins};
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::error::{CompilerError, CompilerException};
use super::generic::GenericInfo;
use super::global_info::GlobalCompilerInfo;
use super::linker::Linker;
use super::permission::PermissionLevel;
use super::type_obj::{InterfaceType, StdTypeObject, TypeObject, UnionTypeObject, UserType};
use super::{annotation, find_path, local_module_path, stdlib_path, CompileResult};

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

#[derive(Debug)]
pub struct ImportInfo {
    line_info: LineInfo,
    module_name: String,
    index: u32,
    names: Vec<String>,
    as_names: Option<Vec<String>>,
}

#[derive(Debug)]
struct FileTypes {
    path: PathBuf,
    permissions: PermissionLevel,
    types: HashMap<String, (TypeObject, LineInfo)>,
    imports: HashMap<PathBuf, Vec<SingleImportInfo>>,
    wildcard_imports: HashSet<PathBuf>,
    exports: HashMap<String, Either<Option<TypeObject>, PathBuf>>,
    wildcard_exports: HashSet<PathBuf>,
}

#[derive(Debug, new)]
struct SingleImportInfo {
    line_info: LineInfo,
    name: String,
    as_name: Option<String>,
}

pub fn compile_all(
    global_info: &GlobalCompilerInfo,
    root_path: PathBuf,
) -> Result<(), Box<dyn Error>> {
    // File-finding algorithm:
    //   Each file returns list of dependent files
    //     Push list of required files to HashMap of files->FileTypes (new struct)
    //   For file in files:
    //     Create ImportHandler from FileTypes
    //   For file in files:
    //     Load default interfaces (use RefCell<ImportHandler>)?
    //   Turn file->ImportHandler map to file->CompilerInfo
    //   Link all files
    //   Compile all files
    // TODO? Turn PathBufs into Arc<Path>
    let builtin_path = builtins_file(global_info.get_arguments());
    let mut default_interfaces = builtins::auto_interfaces();
    parse_builtins(global_info, builtin_path, &mut default_interfaces)?;
    let mut all_files = HashMap::new();
    let mut new_files = FileTypes::find_dependents(
        root_path,
        &mut all_files,
        &mut default_interfaces,
        global_info.get_arguments(),
        PermissionLevel::Normal,
        None,
    )?;
    while !new_files.is_empty() {
        for (file, is_stdlib) in take(&mut new_files) {
            if !all_files.contains_key(&file) {
                new_files.extend(FileTypes::find_dependents(
                    file.clone(),
                    &mut all_files,
                    &mut default_interfaces,
                    global_info.get_arguments(),
                    if is_stdlib {
                        PermissionLevel::Stdlib
                    } else {
                        PermissionLevel::Normal
                    },
                    None,
                )?);
            }
        }
    }
    global_info.set_default_interfaces(default_interfaces);
    // NOTE: feature(iterator_try_collect) (#94047) would improve this
    let import_handlers = all_files
        .iter()
        .map(|(file, (_, types))| {
            let handler = ImportHandler::from_file_types(types)?;
            Ok((file.clone(), handler))
        })
        .collect::<CompileResult<HashMap<_, _>>>()?;
    let mut all_infos = import_handlers
        .into_iter()
        .map(|(file, handler)| {
            // TODO? Remove clone here
            let predeclared = all_files[&file].1.types.clone();
            let info = CompilerInfo::with_handler(global_info, handler, predeclared)?;
            Ok((file, info))
        })
        .collect::<CompileResult<HashMap<_, _>>>()?;
    for (file, info) in &mut all_infos {
        let defaults = info.link(&all_files[file].0)?;
        if let Option::Some(defaults) = defaults {
            defaults.compile(info)?;
        }
    }
    for (file, info) in &mut all_infos {
        info.compile(&all_files[file].0)?
    }
    Ok(())
}

fn parse_builtins(
    global_info: &GlobalCompilerInfo,
    path: PathBuf,
    default_interfaces: &mut HashSet<InterfaceType>,
) -> Result<(), Box<dyn Error>> {
    let mut builtins = ParsedBuiltins::new();
    let mut all_files = HashMap::with_capacity(1);
    let new_files = FileTypes::find_dependents(
        path.clone(),
        &mut all_files,
        default_interfaces,
        global_info.get_arguments(),
        PermissionLevel::Builtin,
        Some(&mut builtins),
    )?;
    assert!(
        new_files.is_empty(),
        "Builtins file cannot import other files"
    );
    debug_assert_eq!(all_files.len(), 1);
    let (node, file_types) = all_files.remove(&path).unwrap();
    drop(all_files); // Should be empty now; this prevents accidental reuse
    let import_handler = ImportHandler::from_file_types(&file_types)?;
    let mut info =
        CompilerInfo::new_builtins(global_info, &mut builtins, import_handler, file_types.types)?;
    let defaults = info
        .link(&node)?
        .expect("Builtins file should not be linked yet");
    defaults.compile(&mut info)?;
    global_info.set_builtins(builtins.into());
    Ok(())
}

impl FileTypes {
    fn new(path: PathBuf, permissions: PermissionLevel) -> Self {
        FileTypes {
            path,
            permissions,
            types: HashMap::new(),
            imports: HashMap::new(),
            wildcard_imports: HashSet::new(),
            exports: HashMap::new(),
            wildcard_exports: HashSet::new(),
        }
    }

    fn find_dependents(
        path: PathBuf,
        all_files: &mut HashMap<PathBuf, (TopNode, FileTypes)>,
        default_interfaces: &mut HashSet<InterfaceType>,
        args: &CLArgs,
        permissions: PermissionLevel,
        mut builtins: Option<&mut ParsedBuiltins>,
    ) -> Result<Vec<(PathBuf, bool)>, Box<dyn Error>> {
        if all_files.contains_key(&path) {
            return Ok(Vec::new());
        }
        let mut to_compile = Vec::new();
        let node = Parser::parse_file(path.clone())??;
        let mut file_types = FileTypes::new(path.clone(), permissions);
        for value in &node {
            if let Result::Ok(ie_node) = <&ImportExportNode>::try_from(value) {
                match ie_node.get_type() {
                    ImportExportType::Import | ImportExportType::Typeget => {
                        to_compile.extend(file_types.add_imports(&path, ie_node, all_files, args)?);
                    }
                    ImportExportType::Export => {
                        to_compile.extend(file_types.add_exports(&path, ie_node, all_files, args)?);
                    }
                }
            } else if let Result::Ok(node) = BaseClassRef::try_from(value) {
                file_types.register_class(
                    node,
                    default_interfaces,
                    reborrow_option(&mut builtins),
                )?;
            }
        }
        all_files.insert(path, (node, file_types));
        Ok(to_compile)
    }

    fn add_imports(
        &mut self,
        path: &Path,
        node: &ImportExportNode,
        all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
        args: &CLArgs,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        assert!(matches!(
            node.get_type(),
            ImportExportType::Import | ImportExportType::Typeget
        ));
        if node.is_wildcard() {
            self.add_wildcard_import(path, node, all_files, args)
        } else if node.get_from().is_empty() {
            check_as(node)?;
            let mut result = Vec::with_capacity(node.get_values().len());
            for (i, val) in node.get_values().iter().enumerate() {
                let pre_dot = <&VariableNode>::try_from(val.get_pre_dot())
                    .unwrap()
                    .get_name();
                assert!(val.get_post_dots().is_empty());
                let (path, is_stdlib) = load_file(pre_dot, node, args, path)?;
                let val_str = val.name_string();
                let as_str = node.get_as().get(i).map(|x| x.name_string());
                self.imports
                    .entry(path.clone())
                    .or_default()
                    .push(SingleImportInfo::new(
                        node.line_info().clone(),
                        val_str,
                        as_str,
                    ));
                if !all_files.contains_key(&path) {
                    result.push((path, is_stdlib));
                }
            }
            Ok(result)
        } else {
            self.add_import_from(path, node, all_files, args)
        }
    }

    fn add_wildcard_import(
        &mut self,
        path: &Path,
        node: &ImportExportNode,
        all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
        args: &CLArgs,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let module_name = module_name(node, 0);
        let (path, is_stdlib) = load_file(module_name, node, args, path)?;
        self.wildcard_imports.insert(path.clone());
        Ok(if !all_files.contains_key(&path) {
            vec![(path, is_stdlib)]
        } else {
            Vec::new()
        })
    }

    fn add_import_from(
        &mut self,
        path: &Path,
        node: &ImportExportNode,
        all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
        args: &CLArgs,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let module_name = module_name(node, 0);
        let (path, is_stdlib) = load_file(module_name, node, args, path)?;
        for (i, name) in node.get_values().iter().enumerate() {
            // FIXME? 'as' imports
            assert!(name.get_post_dots().is_empty());
            let name = <&VariableNode>::try_from(name.get_pre_dot())
                .unwrap()
                .get_name();
            let as_name = node.get_as().get(i).map(|x| {
                <&VariableNode>::try_from(x.get_pre_dot())
                    .unwrap()
                    .get_name()
                    .to_string()
            });
            let import_info =
                SingleImportInfo::new(node.line_info().clone(), name.to_string(), as_name);
            self.imports
                .entry(path.clone())
                .or_default()
                .push(import_info);
        }
        Ok(if !all_files.contains_key(&path) {
            vec![(path, is_stdlib)]
        } else {
            Vec::new()
        })
    }

    fn add_exports(
        &mut self,
        path: &Path,
        node: &ImportExportNode,
        all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
        args: &CLArgs,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let not_renamed = node.get_as().is_empty();
        let is_from = !node.get_from().is_empty();
        if node.is_wildcard() {
            if !is_from {
                return Err(CompilerException::of(
                    "Cannot 'export *' without a 'from' clause",
                    node,
                )
                .into());
            }
            let module_name = module_name(node, 0);
            self.add_wildcard_exports(module_name, node, args)
        } else {
            let result = if is_from {
                self.add_from_exports(node, args)?;
                let (path, is_stdlib) = load_file(module_name(node, 0), node, args, path)?;
                if !all_files.contains_key(&path) {
                    vec![(path, is_stdlib)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };
            for (i, value) in node.get_values().iter().enumerate() {
                let as_stmt = if not_renamed {
                    value
                } else {
                    &node.get_as()[i]
                };
                if <&VariableNode>::try_from(value.get_pre_dot()).is_err()
                    || !value.get_post_dots().is_empty()
                {
                    return Err(CompilerException::of(
                        format!("Illegal export {}", value.name_string()),
                        node,
                    )
                    .into());
                }
                let predot = <&VariableNode>::try_from(value.get_pre_dot()).unwrap();
                let name = predot.get_name();
                let as_name = if as_stmt.is_empty() {
                    name
                } else {
                    predot.get_name()
                };
                if self.exports.contains_key(as_name) {
                    return Err(CompilerException::of(
                        format!("Name {} already exported", as_name),
                        node,
                    )
                    .into());
                } else {
                    self.exports.insert(
                        name.to_string(),
                        if is_from {
                            // FIXME: Is this necessary?
                            let (path, _) = load_file(module_name(node, 0), node, args, path)?;
                            Either::Right(path)
                        } else {
                            Either::Left(None)
                        },
                    );
                }
            }
            Ok(result)
        }
    }

    fn add_wildcard_exports(
        &mut self,
        module_name: &str,
        node: &ImportExportNode,
        args: &CLArgs,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let (path, is_stdlib) = load_file(module_name, node, args, &self.path)?;
        self.wildcard_exports.insert(path.clone());
        Ok(vec![(path, is_stdlib)])
    }

    fn add_from_exports(
        &mut self,
        node: &ImportExportNode,
        args: &CLArgs,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let module_name = module_name(node, 0);
        let (path, is_stdlib) = load_file(module_name, node, args, &self.path)?;
        for (i, name) in node.get_values().iter().enumerate() {
            let value = <&VariableNode>::try_from(name.get_pre_dot()).unwrap();
            let import_info = SingleImportInfo::new(
                LineInfo::empty(),
                value.get_name().to_string(),
                node.get_as().get(i).map(|x| {
                    <&VariableNode>::try_from(x.get_pre_dot())
                        .unwrap()
                        .get_name()
                        .to_string()
                }),
            );
            self.imports
                .entry(path.clone())
                .or_default()
                .push(import_info);
            let export = node.get_as().get(i).unwrap_or(name);
            let export_name = <&VariableNode>::try_from(export.get_pre_dot())
                .unwrap()
                .get_name();
            self.exports
                .insert(export_name.to_string(), Either::Right(path.clone()));
        }
        Ok(vec![(path, is_stdlib)])
    }

    fn register_class(
        &mut self,
        stmt: BaseClassRef<'_>,
        default_interfaces: &mut HashSet<InterfaceType>,
        builtins: Option<&mut ParsedBuiltins>,
    ) -> CompileResult<()> {
        let str_name = stmt.get_name().str_name();
        if let Option::Some((_, line_info)) = self.types.get(str_name) {
            return Err(
                CompilerException::double_def(str_name, stmt.line_info(), line_info).into(),
            );
        }
        let generics = GenericInfo::parse_no_types(stmt.get_name().get_subtypes())?;
        let type_val: UserType = match stmt {
            BaseClassRef::Class(_) | BaseClassRef::Enum(_) => {
                StdTypeObject::new_predefined(str_name.to_string(), generics).into()
            }
            BaseClassRef::Union(_) => {
                UnionTypeObject::new_predefined(str_name.to_string(), generics).into()
            }
            BaseClassRef::Interface(i) => {
                let ty = InterfaceType::new_predefined(str_name.to_string(), generics);
                if i.get_descriptors().contains(&DescriptorNode::Auto) {
                    default_interfaces.insert(ty.clone());
                }
                ty.into()
            }
        };
        if let Option::Some(builtin) =
            annotation::is_builtin(stmt, self.permissions, stmt.get_annotations())?
        {
            let builtins = builtins.expect("Cannot set builtins with no builtins passed");
            builtins.set_builtin(
                builtin.name,
                builtin.index,
                builtin.hidden,
                type_val.clone().into(),
            );
        }
        self.types.insert(
            str_name.to_string(),
            (type_val.into(), stmt.line_info().clone()),
        );
        Ok(())
    }
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

    fn from_file_types(file_types: &FileTypes) -> CompileResult<Self> {
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
                    exports.insert(name.clone(), ty.clone());
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

    pub fn imported_types(&self) -> CompileResult<HashMap<String, (TypeObject, LineInfo)>> {
        let mut imported_types = HashMap::new();
        for (path, import_info) in &self.imports {
            let strings = &import_info.names;
            let as_names = import_info.as_names.as_ref().unwrap_or(&import_info.names);
            let import_handler = get_import_handler(path);
            if strings.len() == 1 && strings[0] == "*" {
                for (name, value) in import_handler.exported_types(import_info)? {
                    imported_types.insert(name, (value, import_info.line_info.clone()));
                }
            } else {
                for (string, as_name) in zip(strings, as_names) {
                    let type_val = import_handler.exported_type(
                        string,
                        import_info.line_info(),
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
    ) -> CompileResult<Option<LangConstant>> {
        let handler = get_import_handler(file);
        handler.exported_const(name, line_info.line_info(), &mut Vec::new())
    }

    pub fn imported_type(
        &self,
        line_info: impl Lined,
        file: &Path,
        name: &str,
    ) -> CompileResult<TypeObject> {
        let handler = get_import_handler(file);
        handler.type_of_export(name, line_info.line_info(), &mut Vec::new())
    }

    fn exported_const<'a, 'b>(
        &self,
        name: &'b str,
        line_info: &'a LineInfo,
        previous_files: &mut Vec<(&'a LineInfo, &'b str)>,
    ) -> CompileResult<Option<LangConstant>> {
        assert_ne!(name, "*");
        self.check_circular(name, previous_files)?;
        if !self.exports.contains_key(name) {
            previous_files.push((line_info, name));
            for path in &self.wildcard_exports {
                let handler = get_import_handler(path);
                match handler.exported_const(name, line_info, previous_files) {
                    Result::Ok(res) => return Ok(res),
                    Result::Err(_) => {}
                }
            }
            return Err(self.export_error(line_info, name).into());
        }
        let export = self.export_constants.get(name);
        if let Option::Some(Option::Some(export)) = export {
            Ok(Some(export.clone()))
        } else if let Option::Some(path) = self.from_exports.get(name) {
            previous_files.push((line_info, name));
            get_import_handler(path).exported_const(name, line_info, previous_files)
        } else if export.is_some() {
            Ok(None)
        } else {
            Err(self.export_error(line_info, name).into())
        }
    }

    fn type_of_export<'a, 'b>(
        &self,
        name: &'b str,
        line_info: &'a LineInfo,
        previous_files: &mut Vec<(&'a LineInfo, &'b str)>,
    ) -> CompileResult<TypeObject> {
        assert_ne!(name, "*");
        self.check_circular(name, previous_files)?;
        if !self.exports.contains_key(name) {
            previous_files.push((line_info, name));
            for path in &self.wildcard_exports {
                let handler = get_import_handler(path);
                match handler.type_of_export(name, line_info, previous_files) {
                    Result::Ok(res) => return Ok(res),
                    Result::Err(_) => {}
                }
            }
            return Err(self.export_error(line_info, name).into());
        }
        let export = self.exports.get(name);
        if let Option::Some(export) = export {
            Ok(export.clone().unwrap())
        } else if let Option::Some(path) = self.from_exports.get(name) {
            previous_files.push((line_info, name));
            get_import_handler(path).type_of_export(name, line_info, previous_files)
        } else {
            Err(self.export_error(line_info, name).into())
        }
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

    fn exported_types(&self, line_info: impl Lined) -> CompileResult<HashMap<String, TypeObject>> {
        let mut result = HashMap::new();
        let line_info = line_info.line_info();
        for (name, type_val) in &self.exports {
            if let Option::Some(ty) = type_val {
                result.insert(name.clone(), ty.clone());
            } else if let Option::Some(type_obj) =
                self.exported_type(name, line_info, &mut Vec::new())?
            {
                result.insert(name.clone(), type_obj);
            }
        }
        Ok(result)
    }

    fn exported_type<'a, 'b>(
        &self,
        name: &'b str,
        line_info: &'a LineInfo,
        previous_files: &mut Vec<(&'a LineInfo, &'b str)>,
    ) -> CompileResult<Option<TypeObject>> {
        assert_ne!(name, "*");
        self.check_circular(name, previous_files)?;
        if !self.exports.contains_key(name) {
            previous_files.push((line_info, name));
            for path in &self.wildcard_exports {
                let handler = get_import_handler(path);
                if let Result::Ok(ty) = handler.exported_type(name, line_info, previous_files) {
                    return Ok(ty);
                }
            }
            return Err(self.export_error(line_info, name).into());
        }
        let export = &self.exports[name];
        if let Option::Some(TypeObject::Type(ty)) = export {
            Ok(Some(ty.represented_type().clone()))
        } else if let Option::Some(path) = self.from_exports.get(name) {
            previous_files.push((line_info, name));
            get_import_handler(path).exported_type(name, line_info, previous_files)
        } else {
            Ok(None)
        }
    }

    fn export_error(&self, line_info: impl Lined, name: &str) -> CompilerException {
        if let Option::Some(closest) = self.closest_exported_name(name, &mut HashSet::new()) {
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
    ) -> Option<&'a str> {
        if let Option::Some(export) = levenshtein::closest_name(name, self.exports.keys()) {
            return Some(export);
        }
        for path in &self.wildcard_exports {
            if !previous_files.contains(path.as_path()) {
                previous_files.insert(path);
                let handler = get_import_handler(path);
                if let Option::Some(exported) = handler.closest_exported_name(name, previous_files)
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

impl ImportInfo {
    pub fn new(
        line_info: LineInfo,
        module_name: String,
        index: u32,
        names: Vec<String>,
        as_names: Option<Vec<String>>,
    ) -> Self {
        Self {
            line_info,
            module_name,
            index,
            names,
            as_names,
        }
    }

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

fn check_as(node: &ImportExportNode) -> CompileResult<()> {
    if !node.get_as().is_empty() && node.get_as().len() != node.get_values().len() {
        Err(
            CompilerException::of(
                format!(
                    "'{}' statement had {} 'as' clauses, expected {} (equal to number of imported names)",
                    node.get_type(), node.get_as().len(), node.get_values().len()
                ),
                node
            ).into()
        )
    } else {
        Ok(())
    }
}

pub fn builtins_file(args: &CLArgs) -> PathBuf {
    let mut path = stdlib_path(args);
    path.push("__builtins__.newlang");
    path
}

fn module_name(node: &ImportExportNode, i: usize) -> &str {
    if !node.get_from().is_empty() {
        <&VariableNode>::try_from(node.get_from().get_pre_dot())
            .unwrap()
            .get_name()
    } else {
        <&VariableNode>::try_from(node.get_values()[i].get_pre_dot())
            .unwrap()
            .get_name()
    }
}

fn load_file(
    module_name: &str,
    node: &ImportExportNode,
    args: &CLArgs,
    current_path: &Path,
) -> CompileResult<(PathBuf, bool)> {
    let (path, is_stdlib) = if node.get_pre_dots() > 0 {
        let mut parent_path = current_path;
        for _ in 0..node.get_pre_dots() {
            // FIXME: Error message here
            parent_path = parent_path.parent().unwrap();
        }
        let local_path = local_module_path(parent_path, module_name, node.line_info())?;
        (local_path, false)
    } else {
        find_path(module_name, node, args)?
    };
    Ok((path, is_stdlib))
}

pub fn get_import_handler(path: &Path) -> &ImportHandler {
    todo!()
}

impl Lined for ImportInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for SingleImportInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
