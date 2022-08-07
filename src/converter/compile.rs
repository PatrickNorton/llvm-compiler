use std::collections::HashMap;
use std::error::Error;
use std::mem::take;
use std::path::{Path, PathBuf};

use either::Either;
use itertools::Itertools;

use crate::converter::builtins::ParsedBuiltins;
use crate::macros::hash_map;
use crate::parser::base::IndependentNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::parse::TopNode;
use crate::parser::type_node::TypeNode;
use crate::parser::typedef::TypedefStatementNode;

use super::compiler_info::CompilerInfo;
use super::convertible::BaseConvertible;
use super::default_holder::DefaultHolder;
use super::error::{CompilerException, CompilerInternalError, CompilerTodoError};
use super::file_types::FileTypes;
use super::global_info::GlobalCompilerInfo;
use super::import_handler::{builtins_file, ImportHandler};
use super::lang_obj::LangObject;
use super::permission::PermissionLevel;
use super::type_obj::{InterfaceType, TypeObject};
use super::{builtins, CompileResult};

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
    //     Load default interfaces
    //   Turn file->ImportHandler map to file->CompilerInfo
    //   Link all files
    //   Compile all files
    // TODO? Turn PathBufs into Arc<Path>
    let builtin_path = builtins_file(global_info.get_arguments());
    let auto_interfaces = builtins::auto_interfaces()
        .into_iter()
        .map(|x| (x, usize::MAX))
        .collect_vec();
    let mut default_interfaces = hash_map!(
        PathBuf::new() => auto_interfaces
    );
    parse_builtins(global_info, builtin_path.clone(), &mut default_interfaces)?;
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
    global_info.set_default_interfaces(
        default_interfaces
            .iter()
            .flat_map(|(_, x)| x)
            .map(|(x, _)| x.clone())
            .collect(),
    );
    // TODO? See if it's possible to calculate typedefs in an earlier pass
    let addl_typedefs = all_files
        .iter()
        .map(|(file, (node, _))| {
            let typedefs = determine_typedefs(node, global_info, file, &all_files)?;
            Ok((file.clone(), typedefs))
        })
        .filter(|x| x.as_ref().map_or(true, |(_, x)| !x.is_empty()))
        .collect::<CompileResult<HashMap<_, _>>>()?;
    for (path, typedefs) in addl_typedefs {
        let (_, types) = all_files.get_mut(&path).unwrap();
        types.types.extend(typedefs)
    }
    // NOTE: feature(iterator_try_collect) (#94047) would improve this
    let import_handlers = all_files
        .iter()
        .map(|(file, (_, types))| {
            let handler = ImportHandler::from_file_types(types)?;
            Ok((file, handler))
        })
        .collect::<CompileResult<HashMap<_, _>>>()?;
    let mut all_infos = import_handlers
        .into_iter()
        .map(|(file, handler)| {
            let predeclared = predeclared_types(file, &all_files);
            let info = CompilerInfo::with_handler(global_info, handler, predeclared)?;
            Ok((file, info))
        })
        .collect::<CompileResult<HashMap<_, _>>>()?;
    let mut default_holders = default_interfaces
        .into_iter()
        // builtins::auto_interfaces (which we don't want to load) is in an
        // empty file, so we filter that out
        // Additionally, __builtins__.newlang has already had its auto
        // interfaces loaded, so we filter that out too
        .filter(|(file, _)| !file.as_os_str().is_empty() && file != &builtin_path)
        .map(|(file, vals)| {
            let mut defaults = DefaultHolder::new();
            if !vals.is_empty() {
                let info = all_infos.get_mut(&file).unwrap();
                let node = &all_files[&file].0;
                load_default_interfaces(info, vals, &mut defaults, node)?;
            }
            Ok((file, defaults))
        })
        .collect::<CompileResult<HashMap<_, _>>>()?;
    for (&file, info) in &mut all_infos {
        let mut defaults = default_holders.remove(file).unwrap();
        info.link(&all_files[file].0, &mut defaults)?;
        defaults.compile(info)?;
    }
    let export_infos = all_infos
        .iter()
        .map(|(&path, info)| (path.clone(), info.import_handler().export_info()))
        .collect::<HashMap<_, _>>();
    global_info.set_export_infos(export_infos);
    for (&file, info) in &mut all_infos {
        info.compile(&all_files[file].0)?
    }
    Ok(())
}

fn parse_builtins(
    global_info: &GlobalCompilerInfo,
    path: PathBuf,
    default_interfaces: &mut HashMap<PathBuf, Vec<(InterfaceType, usize)>>,
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
    let mut defaults = DefaultHolder::new();
    load_default_interfaces(
        &mut info,
        // Can't remove here b/c it's needed for set_default_interfaces
        default_interfaces[&path].clone(),
        &mut defaults,
        &node,
    )?;
    info.link(&node, &mut defaults)?;
    defaults.compile(&mut info)?;
    info.compile(&node)?;
    global_info.set_builtins(builtins.into());
    Ok(())
}

fn load_default_interfaces<'a>(
    info: &mut CompilerInfo,
    vals: Vec<(InterfaceType, usize)>,
    defaults: &mut DefaultHolder<'a>,
    node: &'a TopNode,
) -> CompileResult<()> {
    for (ty, index) in vals {
        let node = match &node[index] {
            IndependentNode::Interface(node) => node,
            x => {
                return Err(CompilerInternalError::of(
                    "Expected given node to be an interface definition",
                    x,
                )
                .into())
            }
        };
        node.base_converter()
            .complete_without_reserving(info, &ty, defaults, true)?;
    }
    Ok(())
}

fn predeclared_types(
    file: &Path,
    all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
) -> HashMap<String, (TypeObject, LineInfo)> {
    // TODO? Typedefs
    let (_, file_types) = &all_files[file];
    let mut predeclared = file_types.types.clone();
    for (path, import_info) in &file_types.imports {
        let (_, import_file) = &all_files[path];
        for import in import_info {
            if let Option::Some((ty, info)) = imported_ty(import_file, all_files, &import.name) {
                predeclared.insert(
                    import
                        .as_name
                        .clone()
                        .unwrap_or_else(|| import.name.clone()),
                    (ty.clone(), info.clone()),
                );
            }
        }
    }
    predeclared
}

fn imported_ty<'a>(
    import_file: &'a FileTypes,
    all_files: &'a HashMap<PathBuf, (TopNode, FileTypes)>,
    name: &str,
) -> Option<&'a (TypeObject, LineInfo)> {
    if let Option::Some(pair) = import_file.types.get(name) {
        Option::Some(pair)
    } else if let Option::Some(Either::Right(file)) = import_file.exports.get(name) {
        imported_ty(&all_files[file].1, all_files, name)
    } else {
        for path in &import_file.wildcard_exports {
            if let Option::Some(pair) = imported_ty(&all_files[path].1, all_files, name) {
                return Some(pair);
            }
        }
        None
    }
}

fn determine_typedefs(
    node: &TopNode,
    global_info: &GlobalCompilerInfo,
    file: &Path,
    all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
) -> CompileResult<HashMap<String, (TypeObject, LineInfo)>> {
    let mut typedefs = HashMap::new();
    for stmt in node {
        if let IndependentNode::Typedef(typedef) = stmt {
            let ty = parse_typedef(typedef, global_info, file, all_files)?;
            let name = typedef.get_name().str_name().to_string();
            typedefs.insert(name, (ty, typedef.line_info().clone()));
        }
    }
    Ok(typedefs)
}

// FIXME/TODO: Merge with other type-deduction functions
fn parse_typedef(
    node: &TypedefStatementNode,
    global_info: &GlobalCompilerInfo,
    path: &Path,
    all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
) -> CompileResult<TypeObject> {
    node_to_ty(node.get_type(), global_info, path, all_files)
}

fn node_to_ty(
    node: &TypeNode,
    global_info: &GlobalCompilerInfo,
    path: &Path,
    all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
) -> CompileResult<TypeObject> {
    if !node.get_name().get_post_dots().is_empty() {
        return Err(CompilerTodoError::of("Conversion of typedefs to dotted types", node).into());
    }
    let ty_name = node.str_name();
    let base = name_to_ty(node, ty_name, global_info, path, all_files)?;
    if !node.get_subtypes().is_empty() {
        let subtypes = node
            .get_subtypes()
            .iter()
            .map(|x| node_to_ty(x, global_info, path, all_files))
            .collect::<CompileResult<Vec<_>>>()?;
        base.generify(node, subtypes)
    } else {
        Ok(base)
    }
}

fn name_to_ty(
    node: impl Lined,
    ty_name: &str,
    global_info: &GlobalCompilerInfo,
    path: &Path,
    all_files: &HashMap<PathBuf, (TopNode, FileTypes)>,
) -> CompileResult<TypeObject> {
    let (_, types) = &all_files[path];
    if let Option::Some((ty, _)) = types.types.get(ty_name) {
        return Ok(ty.clone());
    }
    // TODO: Check from-imports that (may) refer to other types

    let builtins = global_info
        .global_builtins()
        .ok_or_else(|| CompilerInternalError::of("Builtins should be set by now", &node))?;
    if let Option::Some(LangObject::Type(ty)) = builtins.get_name(ty_name) {
        return Ok(ty.clone());
    }
    Err(CompilerException::of(format!("Could not find type '{}'", ty_name), node).into())
}
