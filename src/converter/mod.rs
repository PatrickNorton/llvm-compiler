mod access_handler;
mod annotation;
mod argument;
mod assert;
mod assign;
mod aug_assign;
mod base_converter;
mod body_converter;
mod break_converter;
mod builtins;
mod bytecode;
mod bytecode_list;
mod class;
mod compiler_info;
mod comprehension;
mod config;
mod constant;
mod continue_conv;
mod convertible;
mod declare;
mod declared_assign;
mod default_holder;
mod delete;
mod dict_comp;
mod dict_literal;
mod diverge;
mod do_while;
mod dotimes;
mod dotted_var;
mod error;
mod f_string;
mod file_writer;
mod fn_call;
mod fn_def;
mod fn_info;
mod fn_return;
mod for_converter;
mod function;
mod generic;
mod global_info;
mod if_converter;
mod import;
mod import_handler;
mod inc_dec;
mod index;
mod int_arithmetic;
mod lambda;
mod lang_obj;
mod linker;
mod literal;
mod loop_converter;
mod mutable;
mod number;
mod operator;
mod permission;
mod raise;
mod range;
mod ret;
mod ret_list;
mod slice;
mod str_arithmetic;
mod string;
mod switch;
mod switch_table;
mod syscalls;
mod ternary;
mod test_converter;
mod test_fn;
mod try_stmt;
mod type_loader;
mod type_obj;
mod typedef;
mod variable;
mod variable_holder;
mod warning;
mod while_converter;
mod with;
mod yield_stmt;

use std::env;
use std::error::Error;
use std::ffi::OsStr;
use std::fs::{canonicalize, read_dir, DirEntry};
use std::path::{Path, PathBuf};
use std::time::Instant;

use walkdir::WalkDir;

use crate::arguments::{CLArgs, Optimization};
use crate::parser::line_info::Lined;
use crate::parser::parse::TopNode;
use crate::util::{EXPORTS_FILENAME, FILE_EXTENSION};

use self::bytecode_list::BytecodeList;
use self::compiler_info::CompilerInfo;
use self::constant::LangConstant;
use self::error::{CompilerError, CompilerException};
use self::file_writer::write_to_file;
use self::global_info::GlobalCompilerInfo;
use self::import_handler::builtins_file;
use self::permission::PermissionLevel;
use self::type_obj::TypeObject;

type CompileResult<T> = Result<T, CompilerError>;

type CompileBytes = CompileResult<BytecodeList>;
type CompileTypes = CompileResult<Vec<TypeObject>>;
type CompileConstant = CompileResult<Option<LangConstant>>;

pub fn convert_to_file(file: PathBuf, node: TopNode, args: CLArgs) -> Result<(), Box<dyn Error>> {
    let start = Instant::now();
    let dest_file = file.parent().expect("File must have parent").to_owned();
    let builtin_path = builtins_file(&args);
    let mut global_info = GlobalCompilerInfo::new(dest_file, args);
    global_info.parse_builtins(builtin_path)?;
    let builtins = global_info.global_builtins().get_local();
    let mut info = CompilerInfo::new(
        &global_info,
        file.clone(),
        builtins,
        PermissionLevel::Normal,
    );
    import_handler::compile_all(&mut info, &node)?;
    run_optimization_passes(&global_info);
    write_to_file(&mut global_info, file)?;
    let end = Instant::now();
    let elapsed = end.duration_since(start);
    let counter = global_info.get_warnings();
    println!(
        "Compilation finished in {:.2}s with {} errors and {} warnings",
        elapsed.as_secs_f64(),
        counter.get_errors(),
        counter.get_warnings(),
    );
    Ok(())
}

fn find_path(name: &str, info: &dyn Lined, args: &CLArgs) -> CompileResult<(PathBuf, bool)> {
    let path = env::var("NEWLANG_PATH").unwrap();
    for filename in path.split(':') {
        if !filename.is_empty() {
            let result = WalkDir::new(filename).into_iter().find(|x| match x {
                Result::Ok(x) => {
                    has_extension(x.path(), FILE_EXTENSION) || x.path().ends_with(name)
                }
                Result::Err(_) => true,
            });
            if let Option::Some(r) = result {
                return Ok((
                    get_path(vec![r.unwrap().path().to_owned()], name, info)?,
                    false,
                ));
            }
        }
    }
    for file in read_dir(stdlib_path(args)).unwrap() {
        let builtin = file.unwrap().path();
        if is_module(&builtin) {
            // TODO? name_matches
            return Ok((get_path(vec![builtin], name, info)?, true));
        }
    }
    Err(CompilerException::of(format!("Cannot find module {}", name), info).into())
}

fn local_module_path(
    parent_path: &Path,
    name: &str,
    line_info: &dyn Lined,
) -> CompileResult<PathBuf> {
    let mut result = Vec::new();
    for file in read_dir(parent_path).unwrap() {
        let path = file.unwrap().path();
        if is_module(&path) && has_extension(&path, FILE_EXTENSION) || path.ends_with(name) {
            result.push(path);
        }
    }
    if !result.is_empty() {
        get_path(result, name, line_info)
    } else {
        Err(CompilerException::of(format!("Cannot find module {}", name), line_info).into())
    }
}

fn stdlib_path(args: &CLArgs) -> PathBuf {
    args.stdlib_path()
        .clone()
        .unwrap_or_else(|| canonicalize("Lib").expect("Could not find builtin file"))
}

fn run_optimization_passes(info: &GlobalCompilerInfo) {
    if info.opt_is_enabled(Optimization::InlineFunctions) {
        todo!("Inline functions")
    }
    if info.opt_is_enabled(Optimization::DeadCode) {
        todo!("Dead-code elimination")
    }
}

fn is_module(path: &Path) -> bool {
    if path.is_file() {
        has_extension(path, FILE_EXTENSION)
    } else {
        assert!(path.is_dir());
        read_dir(path)
            .unwrap()
            .any(|x| is_export(x.as_ref().unwrap()))
    }
}

fn is_export(file: &DirEntry) -> bool {
    file.file_name() == "__exports__.newlang"
}

fn get_path(result: Vec<PathBuf>, name: &str, line_info: &dyn Lined) -> CompileResult<PathBuf> {
    let mut end_file = result.into_iter().next().unwrap();
    if end_file.is_dir() {
        end_file.push(EXPORTS_FILENAME);
        if !end_file.exists() {
            Err(
                CompilerException::of(format!("No exports file for module {}", name), line_info)
                    .into(),
            )
        } else {
            Ok(end_file)
        }
    } else {
        Ok(end_file)
    }
}

fn has_extension(path: &Path, extension: impl AsRef<OsStr>) -> bool {
    path.extension() == Some(extension.as_ref())
}
