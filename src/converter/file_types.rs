use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::{Path, PathBuf};

use derive_new::new;
use either::Either;

use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::import::{ImportExportNode, ImportExportType};
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::parse::{parse_file, TopNode};
use crate::parser::typedef::TypedefStatementNode;
use crate::parser::variable::VariableNode;
use crate::util::reborrow_option;

use super::builtins::ParsedBuiltins;
use super::error::CompilerException;
use super::generic::GenericInfo;
use super::global_info::GlobalCompilerInfo;
use super::permission::PermissionLevel;
use super::type_obj::{InterfaceType, StdTypeObject, TypeObject, UnionTypeObject, UserType};
use super::{annotation, find_path, local_module_path, CompileResult};

#[derive(Debug)]
pub struct FileTypes {
    pub(super) path: PathBuf,
    pub(super) permissions: PermissionLevel,
    pub(super) types: HashMap<String, (TypeObject, LineInfo)>,
    pub(super) imports: HashMap<PathBuf, Vec<SingleImportInfo>>,
    pub(super) wildcard_imports: HashSet<PathBuf>,
    pub(super) exports: HashMap<String, Either<Option<TypeObject>, PathBuf>>,
    pub(super) wildcard_exports: HashSet<PathBuf>,
}

#[derive(Debug, new)]
pub struct SingleImportInfo {
    pub(super) line_info: LineInfo,
    pub(super) name: String,
    pub(super) as_name: Option<String>,
}

impl FileTypes {
    pub fn new(path: PathBuf, permissions: PermissionLevel) -> Self {
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

    pub fn find_dependents(
        path: PathBuf,
        all_files: &mut HashMap<PathBuf, (TopNode, FileTypes)>,
        default_interfaces: &mut HashMap<PathBuf, Vec<(InterfaceType, usize)>>,
        global_info: &GlobalCompilerInfo,
        permissions: PermissionLevel,
        mut builtins: Option<&mut ParsedBuiltins>,
    ) -> Result<Vec<(PathBuf, bool)>, Box<dyn Error>> {
        if all_files.contains_key(&path) {
            return Ok(Vec::new());
        }
        let mut to_compile = Vec::new();
        let node = parse_file(path.clone())??;
        let mut file_types = FileTypes::new(path.clone(), permissions);
        let mut typedefs = Vec::new();
        let mut autos = Vec::new();
        for (i, value) in (&node).into_iter().enumerate() {
            if let Result::Ok(ie_node) = <&ImportExportNode>::try_from(value) {
                match ie_node.get_type() {
                    ImportExportType::Import | ImportExportType::Typeget => {
                        to_compile.extend(file_types.add_imports(
                            &path,
                            ie_node,
                            all_files,
                            global_info,
                        )?);
                    }
                    ImportExportType::Export => {
                        to_compile.extend(file_types.add_exports(
                            &path,
                            ie_node,
                            all_files,
                            global_info,
                        )?);
                    }
                }
            } else if let Result::Ok(node) = BaseClassRef::try_from(value) {
                if let Option::Some(auto) =
                    file_types.register_class(node, reborrow_option(&mut builtins))?
                {
                    autos.push((auto, i));
                }
            } else if let Result::Ok(typedef) = <&TypedefStatementNode>::try_from(value) {
                typedefs.push(typedef);
            }
        }
        default_interfaces.insert(path.clone(), autos);
        // FIXME: Add typedefs to file_types.types
        all_files.insert(path, (node, file_types));
        Ok(to_compile)
    }

    fn add_imports(
        &mut self,
        path: &Path,
        node: &ImportExportNode,
        all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        assert!(matches!(
            node.get_type(),
            ImportExportType::Import | ImportExportType::Typeget
        ));
        if node.is_wildcard() {
            self.add_wildcard_import(path, node, all_files, global_info)
        } else if node.get_from().is_empty() {
            check_as(node)?;
            let as_strings = node.get_as().unwrap_or(&[]);
            let mut result = Vec::with_capacity(node.get_values().len());
            for (i, val) in node.get_values().iter().enumerate() {
                let pre_dot = <&VariableNode>::try_from(val.get_pre_dot())
                    .unwrap()
                    .get_name();
                assert!(val.get_post_dots().is_empty());
                let (path, is_stdlib) = load_file(pre_dot, node, global_info, path)?;
                let val_str = val.name_string();
                let as_str = as_strings.get(i).map(|x| x.name_string());
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
            self.add_import_from(path, node, all_files, global_info)
        }
    }

    fn add_wildcard_import(
        &mut self,
        path: &Path,
        node: &ImportExportNode,
        all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let module_name = module_name(node, 0);
        let (path, is_stdlib) = load_file(module_name, node, global_info, path)?;
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
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let module_name = module_name(node, 0);
        let (path, is_stdlib) = load_file(module_name, node, global_info, path)?;
        for (i, name) in node.get_values().iter().enumerate() {
            // FIXME? 'as' imports
            assert!(name.get_post_dots().is_empty());
            let name = <&VariableNode>::try_from(name.get_pre_dot())
                .unwrap()
                .get_name();
            let as_name = node.get_as().and_then(|x| x.get(i)).map(|x| {
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
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
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
            self.add_wildcard_exports(module_name, node, global_info)
        } else {
            let result = if is_from {
                self.add_from_exports(node, global_info)?;
                let (path, is_stdlib) = load_file(module_name(node, 0), node, global_info, path)?;
                if !all_files.contains_key(&path) {
                    vec![(path, is_stdlib)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };
            for (i, value) in node.get_values().iter().enumerate() {
                let as_stmt = node.get_as().map_or(value, |x| &x[i]);
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
                        format!("Name {as_name} already exported"),
                        node,
                    )
                    .into());
                } else {
                    self.exports.insert(
                        name.to_string(),
                        if is_from {
                            // FIXME: Is this necessary?
                            let (path, _) =
                                load_file(module_name(node, 0), node, global_info, path)?;
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
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let (path, is_stdlib) = load_file(module_name, node, global_info, &self.path)?;
        self.wildcard_exports.insert(path.clone());
        Ok(vec![(path, is_stdlib)])
    }

    fn add_from_exports(
        &mut self,
        node: &ImportExportNode,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<Vec<(PathBuf, bool)>> {
        let module_name = module_name(node, 0);
        let (path, is_stdlib) = load_file(module_name, node, global_info, &self.path)?;
        for (i, name) in node.get_values().iter().enumerate() {
            let value = <&VariableNode>::try_from(name.get_pre_dot()).unwrap();
            let import_info = SingleImportInfo::new(
                LineInfo::empty(),
                value.get_name().to_string(),
                node.get_as().and_then(|x| x.get(i)).map(|x| {
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
        }
        Ok(vec![(path, is_stdlib)])
    }

    fn register_class(
        &mut self,
        stmt: BaseClassRef<'_>,
        builtins: Option<&mut ParsedBuiltins>,
    ) -> CompileResult<Option<InterfaceType>> {
        let mut default_interface = None;
        let str_name = stmt.get_name().str_name();
        if let Option::Some((_, line_info)) = self.types.get(str_name) {
            return Err(
                CompilerException::double_def(str_name, stmt.line_info(), line_info).into(),
            );
        }
        let generics = GenericInfo::parse_no_types(stmt.get_name().get_subtypes())?;
        let type_val: UserType = match stmt {
            BaseClassRef::Class(_) | BaseClassRef::Enum(_) => StdTypeObject::new_predefined(
                str_name.to_string(),
                generics,
                stmt.line_info().clone(),
            )
            .into(),
            BaseClassRef::Union(_) => UnionTypeObject::new_predefined(
                str_name.to_string(),
                generics,
                stmt.line_info().clone(),
            )
            .into(),
            BaseClassRef::Interface(i) => {
                let ty = InterfaceType::new_predefined(
                    str_name.to_string(),
                    generics,
                    stmt.line_info().clone(),
                );
                if i.get_descriptors().contains(&DescriptorNode::Auto) {
                    default_interface = Some(ty.clone());
                }
                ty.into()
            }
        };
        type_val
            .get_generic_info()
            .set_parent(type_val.clone().into());
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
        Ok(default_interface)
    }
}

impl Lined for SingleImportInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
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
    global_info: &GlobalCompilerInfo,
    current_path: &Path,
) -> CompileResult<(PathBuf, bool)> {
    let (path, is_stdlib) = if node.get_pre_dots() > 0 {
        let mut parent_path = current_path;
        for _ in 0..node.get_pre_dots() {
            parent_path = parent_path.parent().ok_or_else(|| {
                CompilerException::of(
                    "Cannot load parent: file referred to is beyond the root of the filesystem",
                    node,
                )
            })?;
        }
        let local_path = local_module_path(parent_path, module_name, node.line_info())?;
        (local_path, false)
    } else {
        find_path(module_name, node, global_info)?
    };
    Ok((path, is_stdlib))
}

fn check_as(node: &ImportExportNode) -> CompileResult<()> {
    match node.get_as() {
        Some(as_stmt) if as_stmt.len() != node.get_values().len() => {
            Err(CompilerException::with_note(
                format!(
                    "'{}' statement had {} 'as' clauses, expected {}",
                    node.get_type(),
                    as_stmt.len(),
                    node.get_values().len()
                ),
                format!(
                    "In {art} {ty} with an 'as' clause, the number of values in that clause \
                     must be equal to the number of {ty}s",
                    ty = node.get_type(),
                    art = node.get_type().article(),
                ),
                node,
            )
            .into())
        }
        _ => Ok(()),
    }
}
