use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;

use either::Either;

use crate::parser::base::IndependentNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::parse::TopNode;
use crate::parser::type_node::TypeNode;
use crate::parser::variable::VariableNode;

use super::access_handler::{AccessHandler, AccessLevel};
use super::base_converter::BaseConverter;
use super::builtins::{BuiltinRef, Builtins, ParsedBuiltins};
use super::bytecode_list::BytecodeList;
use super::class::ClassInfo;
use super::constant::{LangConstant, TempConstant};
use super::default_holder::DefaultHolder;
use super::error::CompilerException;
use super::fn_return::FunctionReturnInfo;
use super::function::Function;
use super::global_info::{FunctionInfoRef, FunctionRef, GlobalCompilerInfo};
use super::import_handler::ImportHandler;
use super::lang_obj::{LangInstance, LangObject};
use super::loop_converter::LoopManager;
use super::permission::PermissionLevel;
use super::switch_table::SwitchTable;
use super::type_obj::{TypeObject, UserType};
use super::variable_holder::{VariableHolder, VariableInfo};
use super::warning::WarningHolder;
use super::{linker, CompileResult, CompileTypes};

#[derive(Debug)]
pub struct CompilerInfo<'a> {
    global_info: &'a GlobalCompilerInfo,

    import_handler: ImportHandler,
    static_index: usize,
    fn_indices: HashMap<String, u16>,
    loop_manager: LoopManager,
    warnings: WarningHolder,
    builtins: Either<&'a Builtins, &'a mut ParsedBuiltins>,
    features: HashMap<String, usize>,

    var_holder: VariableHolder,

    fn_returns: FunctionReturnInfo,

    permission_level: PermissionLevel,

    compiled: bool,
    linked: bool,
}

impl<'a> CompilerInfo<'a> {
    pub fn new(
        global_info: &'a GlobalCompilerInfo,
        path: PathBuf,
        builtins: &'a Builtins,
        perms: PermissionLevel,
    ) -> CompileResult<Self> {
        Self::new_inner(
            global_info,
            Either::Left(builtins),
            ImportHandler::new(path, perms),
            HashMap::new(),
        )
    }

    pub fn new_builtins(
        global_info: &'a GlobalCompilerInfo,
        builtins: &'a mut ParsedBuiltins,
        handler: ImportHandler,
        predeclared_types: HashMap<String, (TypeObject, LineInfo)>,
    ) -> CompileResult<Self> {
        Self::new_inner(
            global_info,
            Either::Right(builtins),
            handler,
            predeclared_types,
        )
    }

    pub fn with_handler(
        global_info: &'a GlobalCompilerInfo,
        handler: ImportHandler,
        predeclared_types: HashMap<String, (TypeObject, LineInfo)>,
    ) -> CompileResult<Self> {
        Self::new_inner(
            global_info,
            Either::Left(global_info.global_builtins().unwrap()),
            handler,
            predeclared_types,
        )
    }

    fn new_inner(
        global_info: &'a GlobalCompilerInfo,
        builtins: Either<&'a Builtins, &'a mut ParsedBuiltins>,
        import_handler: ImportHandler,
        predeclared_types: HashMap<String, (TypeObject, LineInfo)>,
    ) -> CompileResult<Self> {
        let perms = import_handler.permission_level();
        let mut var_holder = VariableHolder::new();
        var_holder.add_predeclared_types(predeclared_types)?;
        Ok(Self {
            global_info,
            import_handler,
            static_index: global_info.reserve_static(),
            fn_indices: HashMap::new(),
            loop_manager: LoopManager::new(),
            warnings: WarningHolder::new(global_info.clone_errors()),
            builtins,
            features: HashMap::new(),
            var_holder,
            fn_returns: FunctionReturnInfo::new(),
            permission_level: perms,
            compiled: false,
            linked: false,
        })
    }

    pub fn global_info(&self) -> &GlobalCompilerInfo {
        self.global_info
    }

    pub fn compile(&mut self, node: &TopNode) -> CompileResult<()> {
        assert!(self.linked);
        if self.compiled {
            return Ok(());
        }
        self.add_locals()?;
        self.add_stack_frame();
        let mut bytes = BytecodeList::new();
        for stmt in node {
            if let IndependentNode::Import(_) = stmt {
                continue;
            }
            bytes.extend(BaseConverter::bytes(stmt, self)?);
        }
        self.remove_stack_frame();
        self.global_info.set_static(self.static_index, bytes);
        self.compiled = true;
        Ok(())
    }

    pub fn link<'b>(
        &mut self,
        node: &'b TopNode,
        defaults: &mut DefaultHolder<'b>,
    ) -> CompileResult<()> {
        assert!(!self.linked);
        let linker = linker::link(self, node, defaults)?;
        self.import_handler.set_from_linker(linker)?;
        self.linked = true;
        Ok(())
    }

    pub fn add_globals(
        &mut self,
        globals: &HashMap<String, TypeObject>,
        constants: &HashMap<String, LangConstant>,
    ) {
        self.var_holder.add_globals(globals, constants)
    }

    fn add_locals(&mut self) -> CompileResult<()> {
        self.var_holder
            .add_locals(&self.import_handler, &self.warnings, self.global_info)
    }

    pub fn loop_manager(&mut self) -> &mut LoopManager {
        &mut self.loop_manager
    }

    pub fn warning_holder(&self) -> &WarningHolder {
        &self.warnings
    }

    pub fn warning_holder_mut(&mut self) -> &mut WarningHolder {
        &mut self.warnings
    }

    pub fn var_holder(&self) -> &VariableHolder {
        &self.var_holder
    }

    pub fn var_holder_mut(&mut self) -> &mut VariableHolder {
        &mut self.var_holder
    }

    pub fn import_handler(&self) -> &ImportHandler {
        &self.import_handler
    }

    pub fn import_handler_mut(&mut self) -> &mut ImportHandler {
        &mut self.import_handler
    }

    pub fn get_fn_returns(&self) -> &FunctionReturnInfo {
        &self.fn_returns
    }

    pub fn fn_returns_mut(&mut self) -> &mut FunctionReturnInfo {
        &mut self.fn_returns
    }

    pub fn permissions(&self) -> PermissionLevel {
        self.permission_level
    }

    pub fn builtins(&self) -> BuiltinRef<'_> {
        match &self.builtins {
            Either::Left(b) => BuiltinRef::Standard(b),
            Either::Right(b) => BuiltinRef::Parsed(b),
        }
    }

    pub fn class_index(&self, ty: &UserType) -> u16 {
        self.global_info.class_index(ty)
    }

    pub fn convert_type(&self, type_node: &TypeNode) -> CompileResult<TypeObject> {
        self.var_holder
            .convert_type(type_node, self.builtins(), &self.warnings)
    }

    pub fn get_constant(&self, name: &str) -> Option<Cow<'_, LangConstant>> {
        if let Option::Some(var) = self.var_holder.var_info(name) {
            var.const_value().map(Cow::Borrowed)
        } else {
            self.builtins().constant_of(name)
        }
    }

    pub fn add_function(&mut self, function: Function) -> u16 {
        let fn_name = function.get_name().to_string();
        let index = self.global_info.add_function(function);
        self.fn_indices.insert(fn_name, index);
        index
    }

    pub fn get_fn(&self, name: &str) -> Option<FunctionRef<'_>> {
        self.fn_indices
            .get(name)
            .map(|&x| self.global_info.get_function(x))
    }

    pub fn fn_info(&self, name: &str) -> Option<FunctionInfoRef<'_>> {
        self.fn_indices
            .get(name)
            .map(|&x| self.global_info.get_function(x).into())
    }

    pub fn fn_index(&self, name: &str) -> Option<u16> {
        self.fn_indices.get(name).copied()
    }

    pub fn reserve_class(&mut self, ty: UserType) -> u16 {
        self.global_info.reserve_class(ty)
    }

    pub fn set_class(&mut self, ty: ClassInfo) -> u16 {
        self.global_info.set_class(ty)
    }

    pub fn add_class(&mut self, ty: ClassInfo) -> u16 {
        self.global_info.add_class(ty)
    }

    pub fn add_type(&mut self, type_val: TypeObject) {
        self.var_holder.add_type(type_val)
    }

    pub fn has_type(&self, type_name: &str) -> bool {
        self.var_holder.has_type(type_name)
    }

    pub fn add_local_types(&mut self, parent: TypeObject, values: HashMap<String, TypeObject>) {
        self.var_holder.add_local_types(parent, values)
    }

    pub fn remove_local_types(&mut self) {
        self.var_holder.remove_local_types()
    }

    pub fn get_type_obj(&self, type_name: &str) -> Option<&TypeObject> {
        self.var_holder.get_type_obj(type_name)
    }

    pub fn get_type(&self, variable: &str) -> Option<Cow<'_, TypeObject>> {
        let info = self.var_holder.var_info(variable);
        // FIXME? Remove partially-unnecessary into_owned
        info.map(VariableInfo::get_type)
            .map(Cow::Borrowed)
            .or_else(|| {
                let builtins = self.builtins();
                builtins
                    .constant_of(variable)
                    .map(|x| Cow::Owned(Cow::into_owned(x.get_type(builtins))))
            })
    }

    pub fn class_of(&self, name: &str) -> Option<TypeObject> {
        let builtins = match &self.builtins {
            Either::Left(b) => Either::Left(*b),
            Either::Right(b) => Either::Right(&**b),
        };
        self.var_holder.class_of(builtins, name)
    }

    pub fn defined_names(&self) -> impl Iterator<Item = &str> {
        self.var_holder.defined_names()
    }

    pub fn var_is_undefined(&self, name: &str) -> bool {
        self.var_holder.var_info(name).is_none() && !self.builtins().has_name(name)
    }

    pub fn check_definition(&self, name: &str, lined: impl Lined) -> CompileResult<()> {
        if let Option::Some(info) = self.var_holder.var_def_in_current_frame(name) {
            Err(CompilerException::double_def(name, info.declaration_info(), lined).into())
        } else {
            Ok(())
        }
    }

    // TODO? RAII-based solution to this

    pub fn add_stack_frame(&mut self) {
        self.var_holder.add_stack_frame()
    }

    pub fn remove_stack_frame(&mut self) {
        self.var_holder.remove_stack_frame()
    }

    pub fn add_variable(
        &mut self,
        name: String,
        var_type: TypeObject,
        is_const: bool,
        line_info: LineInfo,
    ) -> u16 {
        self.var_holder
            .add_variable(name, var_type, is_const, line_info)
    }

    pub fn add_constant_variable(
        &mut self,
        name: String,
        var_type: TypeObject,
        constant: LangConstant,
        line_info: LineInfo,
    ) -> u16 {
        self.var_holder
            .add_constant_variable(name, var_type, constant, line_info)
    }

    pub fn add_static_var(
        &mut self,
        name: String,
        var_type: TypeObject,
        is_const: bool,
        line_info: LineInfo,
    ) -> u16 {
        let index = self.global_info.claim_static_var();
        self.var_holder.add_var_info(
            name,
            VariableInfo::new(var_type, is_const, true, None, index, line_info),
        );
        index
    }

    pub fn reserve_const_var(&mut self, name: String, var_type: TypeObject, line_info: LineInfo) {
        let constant = TempConstant::new(var_type.clone());
        self.var_holder
            .add_constant_variable(name, var_type, constant.into(), line_info);
    }

    pub fn set_reserved_var(&mut self, name: &str, value: LangConstant) {
        let var_info = self
            .var_holder
            .var_info(name)
            .expect("set_reserved_var must take an already-defined variable");
        let constant = var_info
            .const_value()
            .expect("set_reserved_var must take a variable with a TempConst");
        match constant {
            LangConstant::Temp(t) => t.set_reserved(value.clone()),
            _ => panic!("set_reserved_var must take a variable with a TempConst"),
        }
        self.var_holder.replace_var_info_const(name, value)
    }

    pub fn variable_is_constant(&self, var: &str) -> bool {
        self.var_holder
            .var_info(var)
            .map(|x| x.has_const_value())
            .unwrap_or_else(|| self.builtins().constant_of(var).is_some())
    }

    pub fn variable_is_immutable(&self, var: &str) -> bool {
        self.var_holder
            .var_info(var)
            .map(|x| x.is_const())
            .unwrap_or(true)
    }

    pub fn variable_is_static(&self, var: &str) -> bool {
        self.var_holder
            .var_info(var)
            .map(|x| x.is_static())
            .unwrap_or(false)
    }

    pub fn var_index(&self, node: &VariableNode) -> CompileResult<u16> {
        self.var_holder
            .var_info(node.get_name())
            .map(|x| x.get_location().unwrap())
            .ok_or_else(|| {
                CompilerException::of(format!("Unknown variable {}", node.get_name()), node).into()
            })
    }

    pub fn static_var_index(&self, node: &VariableNode) -> CompileResult<u16> {
        self.var_holder
            .var_info(node.get_name())
            .map(|x| x.get_static_location().unwrap())
            .ok_or_else(|| {
                CompilerException::of(format!("Unknown variable {}", node.get_name()), node).into()
            })
    }

    pub fn types_of(&self, types: &[TypeNode]) -> CompileTypes {
        types
            .iter()
            .map(|x| {
                self.var_holder
                    .convert_type(x, self.builtins(), &self.warnings)
            })
            .collect()
    }

    pub fn lambda_name(&self) -> String {
        format!("lambda${}", self.global_info.get_anonymous())
    }

    pub fn generator_name(&self) -> String {
        format!("generator${}", self.global_info.get_anonymous())
    }

    pub fn add_switch_table(&mut self, table: SwitchTable) -> u16 {
        self.global_info.add_table(table)
    }

    pub fn access_handler(&self) -> &AccessHandler {
        self.var_holder.access_handler()
    }

    pub fn access_handler_mut(&mut self) -> &mut AccessHandler {
        self.var_holder.access_handler_mut()
    }

    pub fn access_level(&self, obj: &TypeObject) -> AccessLevel {
        self.var_holder.access_handler().access_level(obj)
    }

    pub fn user_access_level(&self, obj: &UserType) -> AccessLevel {
        self.var_holder.access_handler().user_access_level(obj)
    }

    pub fn add_feature(&mut self, feature: String) {
        match self.features.entry(feature) {
            Entry::Occupied(mut e) => *e.get_mut() += 1,
            Entry::Vacant(v) => {
                v.insert(1);
            }
        }
    }

    pub fn add_features(&mut self, features: impl IntoIterator<Item = String>) {
        features.into_iter().for_each(|x| self.add_feature(x))
    }

    pub fn remove_feature(&mut self, feature: &str) {
        *self.features.get_mut(feature).unwrap() -= 1;
    }

    pub fn remove_features<'b>(&mut self, features: impl IntoIterator<Item = &'b str>) {
        features.into_iter().for_each(|x| self.remove_feature(x))
    }

    pub fn add_predeclared_types(
        &mut self,
        types: HashMap<String, (TypeObject, LineInfo)>,
    ) -> CompileResult<()> {
        self.var_holder.add_predeclared_types(types)
    }

    pub fn set_builtin(
        &mut self,
        name: String,
        index: Option<usize>,
        is_hidden: bool,
        object: LangInstance,
    ) {
        self.builtins
            .as_mut()
            .right()
            .expect("set_builtin assumes there are settable builtins")
            .set_builtin(name, index, is_hidden, LangObject::Instance(object));
    }
}
