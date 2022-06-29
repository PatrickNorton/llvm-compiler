use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use dashmap::DashMap;
use derive_new::new;
use indexmap::IndexSet;
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, MutexGuard};

use crate::arguments::{CLArgs, Optimization};
use crate::parser::line_info::LineInfo;
use crate::util::error_counter::ErrorCounter;
use crate::util::int_allocator::SyncIntAllocator;

use super::builtins::Builtins;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::class::ClassInfo;
use super::constant::{FunctionConstant, LangConstant};
use super::fn_info::FunctionInfo;
use super::function::Function;
use super::import_handler::ExportInfo;
use super::switch_table::SwitchTable;
use super::test_fn::convert_test_start;
use super::type_obj::{BaseType, InterfaceType, UserType};

/// A container for compiler information that is shared across all files.
#[derive(Debug)]
pub struct GlobalCompilerInfo {
    dest_file: PathBuf,
    arguments: CLArgs,

    // NOTE: Constants won't be written until end of compilation
    tables: Mutex<Vec<SwitchTable>>,
    builtins: OnceCell<Builtins>,

    static_var_numbers: SyncIntAllocator,
    anonymous_nums: SyncIntAllocator,

    // TODO: Remove as many mutexes as possible
    // (e.g. make local copies of these & write out their data to a global only
    // when done compiling)
    default_functions: Mutex<Vec<Option<BytecodeList>>>,
    functions: Mutex<Vec<Option<Function>>>,
    classes: Mutex<Vec<Option<ClassInfo>>>,
    class_map: DashMap<BaseType, u16>,
    warnings: Arc<ErrorCounter>,
    test_functions: Mutex<Vec<FunctionConstant>>,

    default_interfaces: OnceCell<HashSet<InterfaceType>>,
    export_infos: OnceCell<HashMap<PathBuf, ExportInfo>>,
}

impl GlobalCompilerInfo {
    pub fn new(dest_file: PathBuf, args: CLArgs) -> Self {
        Self {
            dest_file,
            arguments: args,
            tables: Mutex::new(Vec::new()),
            builtins: OnceCell::new(),
            static_var_numbers: SyncIntAllocator::new(),
            anonymous_nums: SyncIntAllocator::new(),
            default_functions: Mutex::new(Vec::new()),
            functions: Mutex::new(vec![None]),
            classes: Mutex::new(Vec::new()),
            class_map: DashMap::new(),
            warnings: Arc::new(ErrorCounter::new()),
            test_functions: Mutex::new(Vec::new()),
            default_interfaces: OnceCell::new(),
            export_infos: OnceCell::new(),
        }
    }

    pub fn dest_file(&self) -> &Path {
        &self.dest_file
    }

    pub fn get_warnings(&self) -> &ErrorCounter {
        &self.warnings
    }

    pub fn global_builtins(&self) -> Option<&Builtins> {
        self.builtins.get()
    }

    pub fn get_arguments(&self) -> &CLArgs {
        &self.arguments
    }

    pub fn is_debug(&self) -> bool {
        self.arguments.is_debug()
    }

    pub fn is_test(&self) -> bool {
        self.arguments.is_test()
    }

    pub fn cfg_values(&self) -> &HashSet<String> {
        self.arguments.cfg_options()
    }

    pub fn reserve_class(&self, ty: UserType) -> u16 {
        let mut classes = self.classes.lock();
        let index = classes.len().try_into().unwrap();
        classes.push(None);
        self.class_map.insert(BaseType::from(ty), index);
        index
    }

    pub fn set_class(&self, ty: ClassInfo) -> u16 {
        let mut classes = self.classes.lock();
        let index = *self
            .class_map
            .get(&BaseType::from(ty.get_type().clone()))
            .unwrap();
        assert!(classes[index as usize].is_none());
        classes[index as usize] = Some(ty);
        index
    }

    pub fn add_class(&self, ty: ClassInfo) -> u16 {
        let mut classes = self.classes.lock();
        let class_no = classes.len().try_into().unwrap();
        let old = self
            .class_map
            .insert(BaseType::from(ty.get_type().clone()), class_no);
        assert!(old.is_none(), "Cannot set class twice");
        classes.push(Some(ty));
        class_no
    }

    pub fn get_functions_classes(
        &mut self,
    ) -> (Vec<&Function>, &Vec<Option<ClassInfo>>, &Builtins) {
        if self.functions.get_mut()[0].is_none() {
            self.init_default_function();
        }
        (
            self.functions
                .get_mut()
                .iter()
                .map(|x| {
                    x.as_ref()
                        .expect("All functions should be written before this point")
                })
                .collect(),
            self.classes.get_mut(),
            self.builtins
                .get()
                .expect("Builtins should have been written by now"),
        )
    }

    pub fn mut_functions(&mut self) -> Vec<&mut Function> {
        if self.functions.get_mut()[0].is_none() {
            self.init_default_function();
        }
        self.functions
            .get_mut()
            .iter_mut()
            .map(|x| {
                x.as_mut()
                    .expect("All functions should be written before this point")
            })
            .collect()
    }

    fn init_default_function(&mut self) {
        debug_assert!(self.functions.get_mut()[0].is_none());
        let functions = self.functions.get_mut();
        let default_fns = self.default_functions.get_mut();
        let mut bytes = single_default_pos(default_fns).map_or_else(
            || create_default_fn(default_fns, functions),
            |x| default_fns[x].clone().unwrap(),
        );
        if self.is_test() {
            let index = convert_test_start(self);
            bytes.add(Bytecode::CallFn(index.into(), 0.into()));
        }
        self.functions.get_mut()[0] = Some(Function::new(
            LineInfo::empty(),
            FunctionInfo::named("__default__"),
            bytes,
        ));
    }

    pub fn get_tables(&mut self) -> &[SwitchTable] {
        self.tables.get_mut()
    }

    pub fn class_index(&self, ty: &UserType) -> u16 {
        *self
            .class_map
            .get(&BaseType::new(ty.clone().into()))
            .unwrap()
    }

    pub fn add_table(&self, table: SwitchTable) -> u16 {
        let mut tables = self.tables.lock();
        tables.push(table);
        (tables.len() - 1)
            .try_into()
            .expect("Too many switch tables")
    }

    pub fn get_function(&self, index: u16) -> FunctionRef<'_> {
        FunctionRef::new(self.functions.lock(), index as usize)
    }

    pub fn claim_static_var(&self) -> u16 {
        self.static_var_numbers
            .next()
            .try_into()
            .expect("Too many static variables")
    }

    pub fn get_anonymous(&self) -> usize {
        self.anonymous_nums.next()
    }

    pub fn add_function(&self, func: Function) -> u16 {
        let mut functions = self.functions.lock();
        let len = functions
            .len()
            .try_into()
            .expect("Too many functions defined");
        functions.push(Some(func));
        len
    }

    pub fn add_test_function(&self, func: FunctionConstant) {
        self.test_functions.lock().push(func)
    }

    pub fn calculate_constants(&mut self) -> IndexSet<LangConstant> {
        let mut constants = IndexSet::new();
        if self.functions.get_mut()[0].is_none() {
            self.init_default_function();
        }
        for function in self.functions.get_mut() {
            function
                .as_ref()
                .unwrap()
                .get_bytes()
                .find_constants(&mut constants);
        }
        for cls in self.classes.get_mut() {
            let cls = cls.as_ref().unwrap();
            for method in cls.get_method_defs().values() {
                method.get_bytes().find_constants(&mut constants);
            }
            for static_method in cls.get_static_methods().values() {
                static_method.get_bytes().find_constants(&mut constants);
            }
            for op in cls.get_operator_defs().values() {
                op.get_bytes().find_constants(&mut constants);
            }
            for (get, set) in cls.get_properties().values() {
                get.get_bytes().find_constants(&mut constants);
                set.get_bytes().find_constants(&mut constants);
            }
        }
        constants
    }

    pub fn opt_is_enabled(&self, opt: Optimization) -> bool {
        self.arguments.opt_is_enabled(opt)
    }

    pub fn reserve_static(&self) -> usize {
        let mut default_functions = self.default_functions.lock();
        default_functions.push(None);
        default_functions.len() - 1
    }

    pub fn set_static(&self, index: usize, bytes: BytecodeList) {
        let mut default_functions = self.default_functions.lock();
        assert!(default_functions[index].is_none());
        default_functions[index] = Some(bytes);
    }

    pub fn clone_errors(&self) -> Arc<ErrorCounter> {
        self.warnings.clone()
    }

    pub fn get_test_functions(&mut self) -> &[FunctionConstant] {
        &**self.test_functions.get_mut()
    }

    pub fn get_default_interfaces(&self) -> Option<&HashSet<InterfaceType>> {
        self.default_interfaces.get()
    }

    pub fn set_default_interfaces(&self, default_interfaces: HashSet<InterfaceType>) {
        self.default_interfaces
            .set(default_interfaces)
            .expect("Cannot set default interfaces twice")
    }

    pub fn set_builtins(&self, builtins: Builtins) {
        self.builtins
            .set(builtins)
            .expect("Cannot set builtins twice")
    }

    pub fn set_export_infos(&self, infos: HashMap<PathBuf, ExportInfo>) {
        self.export_infos
            .set(infos)
            .expect("Cannot set export infos twice")
    }

    pub fn export_info(&self, path: &Path) -> &ExportInfo {
        &self.export_infos.get().expect("Export infos should be set")[path]
    }
}

fn single_default_pos(default_functions: &[Option<BytecodeList>]) -> Option<usize> {
    assert!(!default_functions.is_empty());
    if default_functions.len() == 1 {
        return Some(0);
    }
    let mut found_one = None;
    for (i, func) in default_functions.iter().enumerate() {
        if !func.as_ref().unwrap().is_empty() {
            if found_one.is_some() {
                return None;
            } else {
                found_one = Some(i);
            }
        }
    }
    Some(found_one.unwrap_or_default()) // If no non-zero functions, we can just use any empty one
}

fn create_default_fn(
    default_functions: &[Option<BytecodeList>],
    functions: &mut Vec<Option<Function>>,
) -> BytecodeList {
    let mut result = BytecodeList::new();
    for (i, func) in default_functions.iter().enumerate().rev() {
        if !func.as_ref().unwrap().is_empty() {
            let func = Function::new(
                LineInfo::empty(),
                FunctionInfo::named(format!("__default__${}", i)),
                func.clone().unwrap(),
            );
            functions.push(Some(func));
            let fn_no = functions.len() - 1;
            result.add(Bytecode::CallFn((fn_no as u16).into(), 0.into()))
        }
    }
    result
}

#[derive(Debug, new)]
pub struct FunctionRef<'a> {
    lock: MutexGuard<'a, Vec<Option<Function>>>,
    index: usize,
}

#[derive(Debug, new)]
pub struct FunctionInfoRef<'a> {
    lock: MutexGuard<'a, Vec<Option<Function>>>,
    index: usize,
}

impl<'a> Deref for FunctionRef<'a> {
    type Target = Function;

    fn deref(&self) -> &Self::Target {
        self.lock[self.index]
            .as_ref()
            .expect("Function should be set")
    }
}

impl<'a> DerefMut for FunctionRef<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.lock[self.index]
            .as_mut()
            .expect("Function should be set")
    }
}

impl<'a> Deref for FunctionInfoRef<'a> {
    type Target = FunctionInfo;

    fn deref(&self) -> &Self::Target {
        self.lock[self.index]
            .as_ref()
            .expect("Function should be set")
            .get_info()
    }
}

impl<'a> From<FunctionRef<'a>> for FunctionInfoRef<'a> {
    fn from(x: FunctionRef<'a>) -> Self {
        FunctionInfoRef::new(x.lock, x.index)
    }
}
