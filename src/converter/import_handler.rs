use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::iter::zip;
use std::mem::take;
use std::path::{Path, PathBuf};

use dashmap::DashMap;
use indexmap::IndexSet;
use once_cell::sync::Lazy;

use crate::arguments::CLArgs;
use crate::converter;
use crate::parser::base::IndependentNode;
use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::import::{ImportExportNode, ImportExportType};
use crate::parser::interface::InterfaceDefinitionNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::parse::TopNode;
use crate::parser::typedef::TypedefStatementNode;
use crate::parser::variable::VariableNode;
use crate::util::levenshtein;

use super::builtins::Builtins;
use super::compiler_info::CompilerInfo;
use super::constant::LangConstant;
use super::error::{CompilerException, CompilerInternalError};
use super::generic::GenericInfo;
use super::linker::Linker;
use super::permission::PermissionLevel;
use super::type_obj::{
    BaseType, InterfaceType, StdTypeObject, TypeObject, TypeTypeObject, UnionTypeObject, UserType,
};
use super::variable_holder::VariableHolder;
use super::warning::WarningHolder;
use super::{annotation, stdlib_path, CompileResult};

#[derive(Debug)]
pub struct ImportHandler {
    path: PathBuf,
    permissions: PermissionLevel,
    // TODO: Link to CompilerInfo
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

// TODO: Remove static from this
static ALL_FILES: Lazy<DashMap<PathBuf, ()>> = Lazy::new(DashMap::new);

pub fn compile_all(info: &mut CompilerInfo, node: &TopNode) -> CompileResult<()> {
    // Possible new algorithm:
    // While there are things to compile,
    //  Parse all files in compile_all (can be parallelized w/o issue)
    //  Swap compile_all with empty list
    //  For each value in next_compile_round:
    //      Link them (pass ref to compile_all to add child file info to)
    //      Compile their default interfaces
    //      Compile them
    let mut compile_all = Vec::new();
    load_default_interfaces(&mut compile_all);
    while !compile_all.is_empty() {
        let next_compile_round = take(&mut compile_all);
        // We need to split next_compile_round in order to deal with borrowing
        // issues: We need to borrow the infos as mutable at the same time as we
        // borrow the nodes as immutable from the previous pass.
        let (mut infos, _, nodes) = split_triple(next_compile_round);
        let mut compile_defaults = Vec::with_capacity(infos.len());
        let info_defaults = info.link(node)?;
        for (compiler_info, node) in zip(&mut infos, &nodes) {
            compile_defaults.push(compiler_info.link(node)?);
        }
        info_defaults.map(|x| x.compile(info));
        for (compiler_info, defaults) in zip(&mut infos, compile_defaults) {
            defaults.map(|x| x.compile(compiler_info));
        }
        info.compile(node)?;
        for (compiler_info, node) in zip(&mut infos, &nodes) {
            compiler_info.compile(node)?;
        }
    }
    Ok(())
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

    pub fn get_exports(&self) -> &HashMap<String, Option<TypeObject>> {
        &self.exports
    }

    pub fn set_export_type(&mut self, name: &str, ty: TypeObject) {
        *self.exports.get_mut(name).unwrap() = Some(ty);
    }

    pub fn import_infos(&self) -> &HashMap<PathBuf, ImportInfo> {
        &self.imports
    }

    pub fn register_dependents(
        &mut self,
        var_holder: &mut VariableHolder,
        builtins: Option<&Builtins>,
        warnings: &WarningHolder,
        node: &TopNode,
        args: &CLArgs,
    ) -> CompileResult<()> {
        let mut types = HashMap::new();
        let mut line_infos = HashMap::new();
        let mut defined_in_file = HashSet::new();
        let mut is_module = false;
        let mut has_auto = Option::None;
        let mut typedefs = VecDeque::new();
        self.load_info(
            builtins_file(args),
            "__builtins__",
            PermissionLevel::Builtin,
        );
        for statement in node {
            if let Result::Ok(ie_node) = <&ImportExportNode>::try_from(statement) {
                match ie_node.get_type() {
                    ImportExportType::Import | ImportExportType::Typeget => {
                        self.register_imports(ie_node)?
                    }
                    ImportExportType::Export => {
                        is_module = true;
                        self.register_exports(ie_node, args)?
                    }
                }
            } else if let Result::Ok(cls) = <&InterfaceDefinitionNode>::try_from(statement) {
                let interface = self.register_class(
                    &mut types,
                    &mut line_infos,
                    &mut defined_in_file,
                    statement,
                )?;
                if cls.get_descriptors().contains(&DescriptorNode::Auto) {
                    // TODO: Put in default interfaces
                    has_auto = Some(cls.line_info().clone())
                }
            } else if BaseClassRef::try_from(statement).is_ok() {
                self.register_class(&mut types, &mut line_infos, &mut defined_in_file, statement)?;
            } else if let Result::Ok(stmt) = <&TypedefStatementNode>::try_from(statement) {
                typedefs.push_back(stmt);
            }
        }
        // When stabilized, #[feature(if_let_chains)] (#53667) would be nice here
        // `if !is_module && let Option::Some(has_auto) = has_auto { ... }`
        match has_auto {
            Option::Some(has_auto) if !is_module => {
                return Err(CompilerException::of(
                    "Cannot (yet?) have 'auto' interfaces in non-module file",
                    has_auto,
                )
                .into())
            }
            _ => {}
        }
        for stmt in typedefs {
            let ty = stmt.get_type();
            let name = stmt.get_name();
            // NOTE: The builtins file doesn't have typedefs, so this shouldn't be an issue (yet)
            let cls = var_holder
                .convert_type(ty, builtins.unwrap(), warnings)?
                .typedef_as(name.str_name().to_string());
            types.insert(name.str_name().to_string(), (cls, stmt.line_info().clone()));
        }
        for (name, type_value) in &mut self.exports {
            if types.contains_key(name) {
                *type_value = Some(TypeTypeObject::new(types[name].0.clone()).into())
            }
        }
        var_holder.add_predeclared_types(types)?;
        var_holder
            .access_handler_mut()
            .set_defined_in_file(defined_in_file.into_iter().map(BaseType::new).collect());
        Ok(())
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

    fn load_info(&mut self, path: PathBuf, module_name: &str, level: PermissionLevel) {
        todo!()
    }

    fn register_imports(&mut self, node: &ImportExportNode) -> CompileResult<()> {
        assert!(matches!(
            node.get_type(),
            ImportExportType::Import | ImportExportType::Typeget
        ));
        if node.is_wildcard() {
            self.register_wildcard_import(module_name(node, 0).to_string(), node)
        } else if node.get_from().is_empty() {
            check_as(node)?;
            for val in node.get_values() {
                let pre_dot = <&VariableNode>::try_from(val.get_pre_dot())
                    .unwrap()
                    .get_name();
                assert!(val.get_post_dots().is_empty());
                let path = load_file(pre_dot, node)?;
                let val_str = val.name_string();
                self.imports.insert(
                    path,
                    ImportInfo::new(
                        node.line_info().clone(),
                        module_name(node, 0).to_string(),
                        self.imports.len() as u32,
                        Vec::new(),
                        None,
                    ),
                );
                self.import_strings.insert(val_str);
            }
            Ok(())
        } else {
            self.register_from(node)
        }
    }

    fn register_from(&mut self, node: &ImportExportNode) -> CompileResult<()> {
        check_as(node)?;
        let from = node.get_from();
        let mut values = Vec::new();
        for value in node.get_values() {
            let value_str = value.name_string();
            let val_str = format!("{}.{}", from.name_string(), value_str);
            values.push(value_str);
            self.import_strings.insert(val_str);
        }
        let as_names = if !node.get_as().is_empty() {
            Option::Some(
                node.get_as()
                    .iter()
                    .map(|x| {
                        <&VariableNode>::try_from(x.get_pre_dot())
                            .unwrap()
                            .get_name()
                            .to_string()
                    })
                    .collect(),
            )
        } else {
            Option::None
        };
        let module_name = from.name_string();
        let path = load_file(&module_name, node)?;
        let import_len = self.imports.len() as u32;
        match self.imports.entry(path) {
            Entry::Vacant(v) => {
                v.insert(ImportInfo::new(
                    node.line_info().clone(),
                    module_name,
                    import_len,
                    values,
                    as_names,
                ));
            }
            Entry::Occupied(mut o) => o.get_mut().merge(values, as_names),
        };
        Ok(())
    }

    fn register_from_exports(&mut self, node: &ImportExportNode) -> CompileResult<()> {
        assert!(matches!(node.get_type(), ImportExportType::Export) && !node.get_from().is_empty());
        self.register_from(node)
    }

    fn register_wildcard_import(
        &mut self,
        module_name: String,
        node: &ImportExportNode,
    ) -> CompileResult<()> {
        let path = load_file(&module_name, node)?;
        self.imports.insert(
            path,
            ImportInfo::new(
                node.line_info().clone(),
                module_name,
                self.imports.len() as u32,
                vec!["*".to_owned()],
                None,
            ),
        );
        Ok(())
    }

    fn register_exports(&mut self, node: &ImportExportNode, args: &CLArgs) -> CompileResult<()> {
        assert!(matches!(node.get_type(), ImportExportType::Export));
        let not_renamed = node.get_as().is_empty();
        let is_from = !node.get_from().is_empty();
        if node.is_wildcard() {
            if node.get_from().is_empty() {
                return Err(CompilerException::of(
                    "Cannot 'export *' without a 'from' clause",
                    node,
                )
                .into());
            }
            let module_name = module_name(node, 0);
            self.register_wildcard_export(module_name, node, args)?;
        } else {
            for (i, value) in node.get_values().iter().enumerate() {
                if is_from {
                    self.register_from_exports(node)?;
                }
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
                    self.exports.insert(name.to_string(), None);
                    if is_from {
                        let path = load_file(module_name(node, 0), node)?;
                        self.from_exports.insert(name.to_string(), path);
                    }
                }
            }
        }
        Ok(())
    }

    fn register_wildcard_export(
        &mut self,
        module_name: &str,
        node: &ImportExportNode,
        args: &CLArgs,
    ) -> CompileResult<()> {
        if node.get_pre_dots() > 0 {
            let path =
                converter::local_module_path(self.path.parent().unwrap(), module_name, node)?;
            self.load_info(path.clone(), module_name, self.permissions);
            self.wildcard_exports.insert(path);
        } else {
            let (path, is_stdlib) = converter::find_path(module_name, node, args)?;
            self.load_info(
                path.clone(),
                module_name,
                if is_stdlib {
                    PermissionLevel::Stdlib
                } else {
                    self.permissions
                },
            );
            self.wildcard_exports.insert(path);
        }
        Ok(())
    }

    fn register_class(
        &mut self,
        types: &mut HashMap<String, (TypeObject, LineInfo)>,
        line_infos: &mut HashMap<String, LineInfo>,
        defined_in_file: &mut HashSet<TypeObject>,
        stmt: &IndependentNode,
    ) -> CompileResult<UserType> {
        let cls = BaseClassRef::try_from(stmt).unwrap();
        let str_name = cls.get_name().str_name();
        if types.contains_key(str_name) {
            return Err(CompilerException::double_def(
                str_name,
                stmt.line_info(),
                &line_infos[str_name],
            )
            .into());
        }
        let generics = GenericInfo::parse_no_types(cls.get_name().get_subtypes())?;
        let type_val: UserType = match stmt {
            IndependentNode::ClassDef(_) | IndependentNode::Enum(_) => {
                StdTypeObject::new_predefined(str_name.to_string(), generics).into()
            }
            IndependentNode::Union(_) => {
                UnionTypeObject::new_predefined(str_name.to_string(), generics).into()
            }
            IndependentNode::Interface(_) => {
                InterfaceType::new_predefined(str_name.to_string(), generics).into()
            }
            _ => {
                return Err(CompilerInternalError::of(
                    format!("Unknown class type {:?}", stmt),
                    stmt,
                )
                .into())
            }
        };
        type_val.set_generic_parent();
        if let Option::Some(builtin) =
            annotation::is_builtin(cls, self.permissions, cls.get_annotations())?
        {
            todo!()
        } else {
            types.insert(
                str_name.to_string(),
                (type_val.clone().into(), cls.line_info().clone()),
            );
            line_infos.insert(str_name.to_string(), cls.line_info().clone());
            defined_in_file.insert(type_val.clone().into());
        }
        Ok(type_val)
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
        if let Option::Some(export) =
            levenshtein::closest_name(name, self.exports.keys().map(|x| x.as_str()))
        {
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

fn load_default_interfaces(to_compile: &mut Vec<(CompilerInfo, File, TopNode)>) {
    todo!()
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

fn load_file(module_name: &str, node: &ImportExportNode) -> CompileResult<PathBuf> {
    todo!()
}

pub fn get_import_handler(path: &Path) -> &ImportHandler {
    todo!()
}

fn split_triple<T, U, V>(val: Vec<(T, U, V)>) -> (Vec<T>, Vec<U>, Vec<V>) {
    let (mut x, mut y, mut z) = (
        Vec::with_capacity(val.len()),
        Vec::with_capacity(val.len()),
        Vec::with_capacity(val.len()),
    );
    for (a, b, c) in val {
        x.push(a);
        y.push(b);
        z.push(c);
    }
    (x, y, z)
}

impl Lined for ImportInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
