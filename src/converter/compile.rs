use std::collections::HashMap;
use std::error::Error;
use std::mem::take;
use std::path::PathBuf;

use itertools::Itertools;

use crate::converter::builtins::ParsedBuiltins;
use crate::macros::hash_map;
use crate::parser::base::IndependentNode;
use crate::parser::parse::TopNode;

use super::compiler_info::CompilerInfo;
use super::convertible::BaseConvertible;
use super::default_holder::DefaultHolder;
use super::error::CompilerInternalError;
use super::file_types::FileTypes;
use super::global_info::GlobalCompilerInfo;
use super::import_handler::{builtins_file, ImportHandler};
use super::permission::PermissionLevel;
use super::type_obj::InterfaceType;
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
    for (file, info) in &mut all_infos {
        let mut defaults = default_holders.remove(file).unwrap();
        info.link(&all_files[file].0, &mut defaults)?;
        defaults.compile(info)?;
    }
    let export_infos = all_infos
        .iter()
        .map(|(path, info)| (path.clone(), info.import_handler().export_info()))
        .collect::<HashMap<_, _>>();
    global_info.set_export_infos(export_infos);
    for (file, info) in &mut all_infos {
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
            .complete_without_reserving(info, &ty, defaults)?;
    }
    Ok(())
}
