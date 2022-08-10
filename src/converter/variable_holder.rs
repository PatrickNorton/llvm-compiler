use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::zip;
use std::mem::replace;

use either::Either;
use itertools::Itertools;

use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::type_node::TypeNode;
use crate::util::int_allocator::IntAllocator;
use crate::util::levenshtein;

use super::access_handler::AccessHandler;
use super::builtins::{BuiltinRef, Builtins, ParsedBuiltins};
use super::constant::{LangConstant, ModuleConstant};
use super::error::CompilerException;
use super::global_info::GlobalCompilerInfo;
use super::import_handler::{ImportHandler, ImportInfo};
use super::lang_obj::LangObject;
use super::mutable::MutableType;
use super::type_obj::{ModuleType, TypeObject};
use super::warning::{self, WarningHolder, WarningType};
use super::{CompileResult, CompileTypes};

#[derive(Debug)]
pub struct VariableHolder {
    access_handler: AccessHandler,

    variables: Vec<HashMap<String, VariableInfo>>,
    type_map: HashMap<String, TypeObject>,
    local_types: Vec<LocalTypeFrame>,
    var_numbers: IntAllocator,
    max_var_size: u16,
}

#[derive(Debug)]
pub struct VariableInfo {
    type_val: TypeObject,
    is_const: bool,
    is_static: bool,
    lang_const: Option<LangConstant>,
    location: u16,
    declaration_info: LineInfo,
}

#[derive(Debug)]
struct LocalTypeFrame {
    parent_type: Option<TypeObject>,
    children: HashMap<String, TypeObject>,
}

impl VariableHolder {
    pub fn new() -> Self {
        Self {
            access_handler: AccessHandler::new(),
            variables: vec![HashMap::new()],
            type_map: HashMap::new(),
            local_types: Vec::new(),
            var_numbers: IntAllocator::new(),
            max_var_size: 0,
        }
    }

    pub fn access_handler(&self) -> &AccessHandler {
        &self.access_handler
    }

    pub fn access_handler_mut(&mut self) -> &mut AccessHandler {
        &mut self.access_handler
    }

    pub fn var_info(&self, name: &str) -> Option<&VariableInfo> {
        self.variables.iter().rev().find_map(|x| x.get(name))
    }

    pub fn replace_var_info_const(&mut self, name: &str, constant: LangConstant) {
        self.variables
            .iter_mut()
            .rev()
            .find_map(|x| x.get_mut(name))
            .unwrap()
            .lang_const = Some(constant)
    }

    pub fn defined_names(&self) -> impl Iterator<Item = &str> {
        self.variables
            .iter()
            .rev()
            .flat_map(|x| x.keys().map(|x| x.as_ref()))
    }

    pub fn add_globals(
        &mut self,
        globals: &HashMap<String, TypeObject>,
        constants: &HashMap<String, LangConstant>,
    ) {
        let var_frame = &mut self.variables[0];
        for (name, ty) in globals {
            if !var_frame.contains_key(name) {
                if let Option::Some(constant) = constants.get(name) {
                    var_frame.insert(
                        name.clone(),
                        VariableInfo::new(
                            ty.clone(),
                            true,
                            false,
                            Some(constant.clone()),
                            u16::MAX,
                            LineInfo::empty(),
                        ),
                    );
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn add_locals(
        &mut self,
        import_handler: &ImportHandler,
        warnings: &WarningHolder,
        global_info: &GlobalCompilerInfo,
    ) -> CompileResult<()> {
        for (path, info) in import_handler.import_infos() {
            let var_map = &mut self.variables[0];
            if info.get_names().is_empty() {
                let handler = global_info.export_info(path);
                let mut export_map = HashMap::with_capacity(handler.get_exports().len());
                let mut export_indices = HashMap::with_capacity(handler.get_exports().len());
                for (name, ty) in handler.get_exports() {
                    let constant =
                        import_handler.imported_constant(info, path, name, global_info)?;
                    export_map.insert(name.clone(), ty.clone().unwrap());
                    export_indices.insert(name.clone(), constant.unwrap());
                }
                let mod_name = info.get_module_name();
                let mod_constant = ModuleConstant::new(mod_name.to_string(), export_indices);
                let mod_type = ModuleType::new(mod_name.to_string(), export_map);
                var_map.insert(
                    mod_name.to_string(),
                    VariableInfo::new(
                        mod_type.into(),
                        true,
                        false,
                        Some(mod_constant.into()),
                        u16::MAX,
                        info.line_info().clone(),
                    ),
                );
            } else if let Option::Some(as_names) = info.get_as_names() {
                for (name, as_name) in zip(info.get_names(), as_names) {
                    if !var_map.contains_key(as_name) {
                        let ty = import_handler.imported_type(info, path, name, global_info)?;
                        let const_index =
                            import_handler.imported_constant(info, path, name, global_info)?;
                        var_map.insert(
                            as_name.clone(),
                            Self::get_variable_info(
                                info,
                                ty,
                                const_index,
                                warnings,
                                &mut self.var_numbers,
                            )?,
                        );
                    }
                }
            } else if info.get_names()[0] != "*" {
                for name in info.get_names() {
                    let ty = import_handler.imported_type(info, path, name, global_info)?;
                    let const_index =
                        import_handler.imported_constant(info, path, name, global_info)?;
                    var_map.insert(
                        name.clone(),
                        Self::get_variable_info(
                            info,
                            ty,
                            const_index,
                            warnings,
                            &mut self.var_numbers,
                        )?,
                    );
                }
            } else {
                let handler = global_info.export_info(path);
                for (name, ty) in handler.get_exports() {
                    let const_index =
                        import_handler.imported_constant(info, path, name, global_info)?;
                    var_map.insert(
                        name.clone(),
                        Self::get_variable_info(
                            info,
                            ty.clone().unwrap(),
                            const_index,
                            warnings,
                            &mut self.var_numbers,
                        )?,
                    );
                }
            }
        }
        Ok(())
    }

    fn get_variable_info(
        info: &ImportInfo,
        ty: TypeObject,
        constant: Option<LangConstant>,
        warnings: &WarningHolder,
        var_numbers: &mut IntAllocator,
    ) -> CompileResult<VariableInfo> {
        if constant.is_some() {
            Ok(VariableInfo::new(
                ty,
                true,
                false,
                constant,
                u16::MAX,
                info.line_info().clone(),
            ))
        } else {
            warning::warn_if(
                "Import is not a compile-time constant, may fail at runtime",
                WarningType::NoType,
                warnings,
                info,
            )?;
            Ok(VariableInfo::new(
                ty,
                true,
                false,
                None,
                var_numbers.next().try_into().unwrap(),
                info.line_info().clone(),
            ))
        }
    }

    pub fn add_stack_frame(&mut self) {
        self.variables.push(HashMap::new())
    }

    pub fn remove_stack_frame(&mut self) {
        for frame in self.variables.pop().unwrap().values() {
            if !frame.has_const_value() && !frame.is_static() {
                self.var_numbers
                    .remove(frame.get_location().unwrap().into());
            }
        }
    }

    pub fn add_variable(
        &mut self,
        name: String,
        var_type: TypeObject,
        is_const: bool,
        info: LineInfo,
    ) -> u16 {
        let var_info = VariableInfo::new(
            var_type,
            is_const,
            false,
            None,
            self.var_numbers
                .next()
                .try_into()
                .expect("Too many variables"),
            info,
        );
        self.add_var_info(name, var_info)
    }

    pub fn add_constant_variable(
        &mut self,
        name: String,
        var_type: TypeObject,
        constant: LangConstant,
        info: LineInfo,
    ) -> u16 {
        let var_info = VariableInfo::new(var_type, true, false, Some(constant), u16::MAX, info);
        self.add_var_info(name, var_info)
    }

    pub fn add_var_info(&mut self, name: String, info: VariableInfo) -> u16 {
        let last = self.variables.last_mut().unwrap();
        let variable = last.len().try_into().unwrap();
        last.insert(name, info);
        variable
    }

    pub fn var_def_in_current_frame(&self, name: &str) -> Option<&VariableInfo> {
        self.variables.last().unwrap().get(name)
    }

    pub fn remove_variable(&mut self, name: &str) {
        for frame in self.variables.iter_mut().rev() {
            if let Option::Some(value) = frame.remove(name) {
                if let Option::Some(loc) = value.get_location() {
                    self.var_numbers.remove(loc as usize);
                }
                return;
            }
        }
        panic!("Variable {} removed but undefined", name)
    }

    pub fn add_local_types(
        &mut self,
        parent: Option<TypeObject>,
        values: HashMap<String, TypeObject>,
    ) {
        self.local_types.push(LocalTypeFrame {
            parent_type: parent,
            children: values,
        })
    }

    pub fn remove_local_types(&mut self) {
        self.local_types.pop().unwrap();
    }

    pub fn reset_max(&mut self) -> u16 {
        replace(
            &mut self.max_var_size,
            self.var_numbers.max().unwrap_or(0).try_into().unwrap(),
        )
    }

    pub fn add_type(&mut self, ty: TypeObject) {
        self.type_map.insert(ty.name().to_string(), ty);
    }

    pub fn has_type(&self, type_name: &str) -> bool {
        self.type_map.contains_key(type_name)
    }

    pub fn get_type_obj(&self, type_name: &str) -> Option<&TypeObject> {
        self.type_map.get(type_name)
    }

    pub fn add_predeclared_types(
        &mut self,
        types: HashMap<String, (TypeObject, LineInfo)>,
    ) -> CompileResult<()> {
        self.type_map.reserve(types.len());
        for (name, (obj, line_info)) in types {
            match self.type_map.entry(name) {
                Entry::Occupied(e) => {
                    return Err(CompilerException::double_def(
                        e.key(),
                        line_info,
                        LineInfo::empty(),
                    )
                    .into())
                }
                Entry::Vacant(e) => {
                    e.insert(obj);
                }
            }
        }
        Ok(())
    }

    pub fn local_parent(&self, ty: &TypeObject) -> Option<&TypeObject> {
        let base_name = ty.base_name();
        self.local_types
            .iter()
            .rev()
            .find(|frame| frame.children.contains_key(&*base_name))
            .and_then(|x| x.parent_type.as_ref())
    }

    pub fn class_of(
        &self,
        builtins: Either<&Builtins, &ParsedBuiltins>,
        name: &str,
    ) -> Option<TypeObject> {
        self.type_map.get(name).cloned().or_else(|| {
            let builtin_map = match builtins {
                Either::Left(b) => b.builtin_map(),
                Either::Right(b) => b.builtin_map(),
            };
            match builtin_map.get(name)? {
                LangObject::Type(ty) => Some(ty.clone()),
                _ => None,
            }
        })
    }

    // TODO? Support builtins-free type conversion
    pub fn convert_type(
        &self,
        node: &TypeNode,
        builtins: BuiltinRef<'_>,
        warnings: &WarningHolder,
    ) -> CompileResult<TypeObject> {
        // FIXME: This string-matching is ugly and I don't like it
        match &*node.str_names() {
            "null" => {
                assert!(node.get_subtypes().is_empty());
                if node.is_optional() {
                    warning::warn_if(
                        "Type null? is equivalent to null",
                        WarningType::TrivialValue,
                        warnings,
                        node,
                    )?;
                }
                Ok(builtins.null_type().clone())
            }
            "cls" => {
                if let Option::Some(cls) = self.access_handler.get_cls() {
                    Ok(wrap(cls, node))
                } else {
                    Err(
                        CompilerException::of("Type 'cls' is not defined in this scope", node)
                            .into(),
                    )
                }
            }
            "super" => {
                if let Option::Some(sup) = self.access_handler.get_super() {
                    Ok(wrap(sup, node))
                } else {
                    Err(
                        CompilerException::of("Type 'super' is not defined in this scope", node)
                            .into(),
                    )
                }
            }
            "" => Ok(TypeObject::list_of(self.types_of(
                warnings,
                builtins,
                node.get_subtypes(),
            )?)),
            _ => self.convert_type_inner(node, builtins, warnings),
        }
    }

    fn convert_type_inner(
        &self,
        node: &TypeNode,
        builtins: BuiltinRef<'_>,
        warnings: &WarningHolder,
    ) -> CompileResult<TypeObject> {
        let type_name = node.str_names();
        if let Option::Some(value) = self.type_map.get(&node.str_names()) {
            self.generify(node, value, builtins, warnings)
        } else {
            for local_type in &self.local_types {
                if let Option::Some(child) = local_type.children.get(&type_name) {
                    return Ok(wrap(child, node));
                }
            }
            // FIXME: This is hideous
            let builtin = match builtins {
                BuiltinRef::Standard(s) => s.get_name(&type_name),
                BuiltinRef::Parsed(p) => {
                    return p
                        .get_name(&type_name)
                        .ok_or_else(|| self.unknown_type_err(&type_name, node, builtins).into())
                        .and_then(|x| self.generify(node, x, builtins, warnings))
                }
            }
            .ok_or_else(|| self.unknown_type_err(&type_name, node, builtins))?;
            match builtin {
                LangObject::Constant(_) => unreachable!(),
                LangObject::Type(type_obj) => self.generify(node, type_obj, builtins, warnings),
                LangObject::Instance(_) => {
                    Err(self.unknown_type_err(&type_name, node, builtins).into())
                }
            }
        }
    }

    fn unknown_type_err(
        &self,
        name: &str,
        node: &TypeNode,
        builtins: BuiltinRef<'_>,
    ) -> CompilerException {
        let names = self.type_map.keys().map(|x| &**x).chain(self.local_types());
        let builtin_iter = builtins.builtin_names();
        let names = names.chain(builtin_iter);
        if let Option::Some(suggestion) = levenshtein::closest_name(name, names) {
            CompilerException::of(
                format!("Unknown type '{}'. Did you mean '{}'?", name, suggestion),
                node,
            )
        } else {
            CompilerException::of(format!("Unknown type '{}'", name), node)
        }
    }

    fn generify(
        &self,
        node: &TypeNode,
        value: &TypeObject,
        builtins: BuiltinRef<'_>,
        warnings: &WarningHolder,
    ) -> CompileResult<TypeObject> {
        if node.get_subtypes().is_empty() {
            Ok(wrap(value, node))
        } else {
            Ok(wrap(
                &value.generify(
                    node,
                    self.types_of(warnings, builtins, node.get_subtypes())?,
                )?,
                node,
            ))
        }
    }

    fn types_of(
        &self,
        warnings: &WarningHolder,
        builtins: BuiltinRef<'_>,
        subtypes: &[TypeNode],
    ) -> CompileTypes {
        subtypes
            .iter()
            .map(|x| self.convert_type(x, builtins, warnings))
            .collect()
    }

    fn local_types(&self) -> impl Iterator<Item = &str> {
        self.local_types
            .iter()
            .rev()
            .flat_map(|x| x.children.keys().map(|x| &**x))
    }
}

impl VariableInfo {
    pub fn new(
        type_val: TypeObject,
        is_const: bool,
        is_static: bool,
        lang_const: Option<LangConstant>,
        location: u16,
        declaration_info: LineInfo,
    ) -> Self {
        Self {
            type_val,
            is_const,
            is_static,
            lang_const,
            location,
            declaration_info,
        }
    }

    pub fn get_type(&self) -> &TypeObject {
        &self.type_val
    }

    pub fn get_location(&self) -> Option<u16> {
        (!self.is_static).then(|| self.location)
    }

    pub fn get_static_location(&self) -> Option<u16> {
        self.is_static.then(|| self.location)
    }

    pub fn has_const_value(&self) -> bool {
        self.lang_const.is_some()
    }

    pub fn const_value(&self) -> Option<&LangConstant> {
        self.lang_const.as_ref()
    }

    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn declaration_info(&self) -> &LineInfo {
        &self.declaration_info
    }
}

fn wrap(obj: &TypeObject, node: &TypeNode) -> TypeObject {
    let mut_node = MutableType::from_nullable(node.get_mutability());
    if !mut_node.is_const_type() {
        if node.is_optional() {
            TypeObject::optional(obj.make_mut())
        } else {
            obj.make_mut()
        }
    } else if node.is_optional() {
        TypeObject::optional(obj.make_const())
    } else {
        obj.make_const()
    }
}

impl Default for VariableHolder {
    fn default() -> Self {
        Self::new()
    }
}
