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
mod compile;
mod compiler_info;
mod comprehension;
mod config;
mod constant;
mod continue_conv;
mod convertible;
mod dead_code;
mod declare;
mod declared_assign;
mod default_holder;
mod delete;
mod derived_op;
mod dict_comp;
mod dict_literal;
mod diverge;
mod do_while;
mod dotimes;
mod dotted_var;
mod error;
mod error_builder;
mod f_string;
mod file_types;
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
use crate::error::LineInfo;
use crate::parser::line_info::Lined;
use crate::util::{EXPORTS_FILENAME, FILE_EXTENSION};

use self::bytecode_list::BytecodeList;
use self::constant::LangConstant;
use self::error::{CompilerError, CompilerException};
use self::error_builder::ErrorBuilder;
use self::file_writer::write_to_file;
use self::global_info::GlobalCompilerInfo;
use self::type_obj::TypeObject;

type CompileResult<T> = Result<T, CompilerError>;

type CompileBytes = CompileResult<BytecodeList>;
type CompileTypes = CompileResult<Vec<TypeObject>>;
type CompileConstant = CompileResult<Option<LangConstant>>;

pub fn convert_to_file(
    file: PathBuf,
    start_file: PathBuf,
    args: CLArgs,
) -> Result<(), Box<dyn Error>> {
    let start = Instant::now();
    let dest_file = file.parent().expect("File must have parent").to_owned();
    let mut global_info = GlobalCompilerInfo::new(dest_file, args);
    compile::compile_all(&global_info, start_file)?;
    run_optimization_passes(&mut global_info);
    write_to_file(&mut global_info, file)?;
    let end = Instant::now();
    let elapsed = end.duration_since(start);
    let counter = global_info.get_warnings();
    // TODO? Print timing on error
    println!(
        "Compilation finished in {:.2}s with {} errors and {} warnings",
        elapsed.as_secs_f64(),
        counter.get_errors(),
        counter.get_warnings(),
    );
    Ok(())
}

fn find_path(
    name: &str,
    info: &dyn Lined,
    global_info: &GlobalCompilerInfo,
) -> CompileResult<(PathBuf, bool)> {
    // TODO? Make installed packages local to the project instead of global
    // (c.f. Python's packaging disaster)
    let path = match env::var("NEWLANG_PATH") {
        Ok(path) => path,
        Err(_) => {
            warning::warn_counter(
                ErrorBuilder::new(LineInfo::empty_ref())
                    .with_message("Could not find $NEWLANG_PATH environment variable")
                    .with_note(
                        "Unless $NEWLANG_PATH is defined, locally-installed packages will not work",
                    ),
                global_info.get_warnings(),
            )?;
            String::new()
        }
    };
    for filename in path.split(':') {
        if !filename.is_empty() {
            let result = WalkDir::new(filename).into_iter().find(|x| match x {
                Result::Ok(x) => {
                    has_extension(x.path(), FILE_EXTENSION) || x.path().ends_with(name)
                }
                Result::Err(_) => true,
            });
            if let Option::Some(r) = result {
                return Ok((get_path(r.unwrap().path().to_owned(), name, info)?, false));
            }
        }
    }
    for file in read_dir(stdlib_path(global_info.get_arguments())).unwrap() {
        let builtin = file.unwrap().path();
        if is_module(&builtin) && builtin.file_stem() == Some(OsStr::new(name)) {
            return Ok((get_path(builtin, name, info)?, true));
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
        if is_module(&path) && path.file_stem() == Some(OsStr::new(name)) {
            result.push(path);
        }
    }
    if let Option::Some(path) = result.into_iter().next() {
        get_path(path, name, line_info)
    } else {
        Err(CompilerException::of(format!("Cannot find module {}", name), line_info).into())
    }
}

fn stdlib_path(args: &CLArgs) -> PathBuf {
    args.stdlib_path()
        .clone()
        .unwrap_or_else(|| canonicalize("Lib").expect("Could not find builtin file"))
}

fn run_optimization_passes(info: &mut GlobalCompilerInfo) {
    if info.opt_is_enabled(Optimization::InlineFunctions) {
        todo!("Inline functions")
    }
    if info.opt_is_enabled(Optimization::DeadCode) {
        dead_code::eliminate(info)
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

#[inline]
fn is_export(file: &DirEntry) -> bool {
    file.file_name() == EXPORTS_FILENAME
}

fn get_path(mut end_file: PathBuf, name: &str, line_info: &dyn Lined) -> CompileResult<PathBuf> {
    debug_assert!(end_file.exists(), "Passed file should exist");
    if end_file.is_dir() {
        end_file.push(EXPORTS_FILENAME);
        if !end_file.exists() {
            Err(CompilerException::with_note(
                format!("No exports file for module {}", name),
                "To use a directory as a module, it must contain a file \
                 named '__exports__.newlang'",
                line_info,
            )
            .into())
        } else {
            Ok(end_file)
        }
    } else {
        Ok(end_file)
    }
}

#[inline]
fn has_extension(path: &Path, extension: impl AsRef<OsStr>) -> bool {
    path.extension() == Some(extension.as_ref())
}
