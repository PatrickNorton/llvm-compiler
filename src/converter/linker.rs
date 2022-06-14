use std::collections::HashMap;
use std::iter::zip;

use crate::parser::base::IndependentNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::definition::{BaseClassRef, DefinitionRef};
use crate::parser::descriptor::DescriptorNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::parse::TopNode;

use super::annotation::{self, should_compile, BuiltinInfo};
use super::compiler_info::CompilerInfo;
use super::constant::{ClassConstant, FunctionConstant, LangConstant};
use super::convertible::BaseConvertible;
use super::default_holder::DefaultHolder;
use super::error::{CompilerException, CompilerTodoError};
use super::lang_obj::LangInstance;
use super::test_converter::TestConverter;
use super::type_loader::TypeLoader;
use super::type_obj::{TypeObject, TypeTypeObject, UserType};
use super::CompileResult;

#[derive(Debug)]
pub struct Linker {
    pub exports: HashMap<String, (String, LineInfo)>,
    pub globals: HashMap<String, TypeObject>,
    pub constants: HashMap<String, LangConstant>,
}

pub fn link<'a>(
    info: &mut CompilerInfo,
    node: &'a TopNode,
    defaults: &mut DefaultHolder<'a>,
) -> CompileResult<Linker> {
    let mut linker = Linker::new();
    // info.load_dependents(node)?;
    // let imported_types = info.import_handler_mut().imported_types()?;
    // info.add_predeclared_types(imported_types)?;
    // Needed for variable-declaration constant-folding
    info.add_stack_frame();
    // Filters out auto interfaces, which are registered earlier
    for stmt in node {
        // FIXME: Check for top-level
        if let Result::Ok(def) = DefinitionRef::try_from(stmt) {
            if !is_auto_interface(def) {
                let name = def.str_name();
                let is_builtin =
                    annotation::is_builtin(def, info.permissions(), def.get_annotations())?;
                let has_builtin = is_builtin.is_some();
                if let Option::Some(type_val) =
                    linker.link_definition(info, def, is_builtin, defaults)?
                {
                    if info.import_handler().get_exports().contains_key(&*name) {
                        info.import_handler_mut()
                            .set_export_type(&name, type_val.clone());
                    }
                    if !has_builtin {
                        linker.globals.insert(name.to_string(), type_val);
                    }
                }
            }
        } else if let IndependentNode::Typedef(td_node) = stmt {
            let ty = info.convert_type(td_node.get_type())?;
            let constant =
                TypeLoader::type_constant(td_node.line_info(), &ty, info)?.ok_or_else(|| {
                    CompilerException::of("Cannot get constant for local types", td_node)
                })?;
            linker
                .constants
                .insert(td_node.get_name().str_name().to_string(), constant);
        } else if let IndependentNode::DeclaredAssign(decl) = stmt {
            linker.link_declaration(info, decl)?;
        }
    }
    info.remove_stack_frame();
    info.add_globals(&linker.globals, &linker.constants);
    Ok(linker)
}

impl Linker {
    fn new() -> Self {
        Self {
            exports: HashMap::new(),
            globals: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    fn link_definition<'a>(
        &mut self,
        info: &mut CompilerInfo,
        stmt: DefinitionRef<'a>,
        is_builtin: Option<BuiltinInfo>,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<Option<TypeObject>> {
        if !should_compile(stmt, info, stmt.get_annotations())? {
            return Ok(None);
        }
        match stmt {
            DefinitionRef::Function(fn_node) => {
                if let Option::Some((ty, index)) =
                    fn_node
                        .base_converter()
                        .parse_header(info, is_builtin.is_some(), defaults)?
                {
                    if let Option::Some(builtin) = is_builtin {
                        let object = LangInstance::new(ty.clone());
                        info.set_builtin(builtin.name, builtin.index, builtin.hidden, object);
                    } else {
                        let fn_name = fn_node.get_name().get_name();
                        let constant = FunctionConstant::new(fn_name.into(), index);
                        self.constants.insert(fn_name.to_string(), constant.into());
                    }
                    Ok(Some(ty))
                } else {
                    Ok(None)
                }
            }
            DefinitionRef::Property(prop) => info.convert_type(prop.get_type().as_type()).map(Some),
            DefinitionRef::Context(_) => {
                Err(CompilerTodoError::of("Context definitions not supported yet", stmt).into())
            }
            DefinitionRef::Operator(_) => {
                Err(CompilerException::of("Operator must be defined in a class", stmt).into())
            }
            DefinitionRef::Method(_) => {
                Err(CompilerException::of("Method must be defined in a class", stmt).into())
            }
            DefinitionRef::BaseClass(BaseClassRef::Class(cls_node)) => {
                let predeclared_type = info
                    .class_of(cls_node.str_name())
                    .unwrap()
                    .try_into()
                    .unwrap();
                let index = cls_node.base_converter().complete_type(
                    info,
                    &predeclared_type,
                    is_builtin.is_none(),
                    defaults,
                )?;
                self.add_constant(
                    cls_node.str_name().to_string(),
                    index,
                    predeclared_type.clone().into(),
                    &is_builtin,
                );
                Ok(Some(TypeTypeObject::new(predeclared_type.into()).into()))
            }
            DefinitionRef::BaseClass(BaseClassRef::Enum(enum_node)) => {
                let predeclared_type = info
                    .class_of(enum_node.get_name().str_name())
                    .unwrap()
                    .try_into()
                    .unwrap();
                let index = enum_node.base_converter().complete_type(
                    info,
                    &predeclared_type,
                    is_builtin.is_none(),
                    defaults,
                )?;
                self.add_constant(
                    enum_node.get_name().str_name().to_string(),
                    index,
                    predeclared_type.clone().into(),
                    &is_builtin,
                );
                Ok(Some(TypeTypeObject::new(predeclared_type.into()).into()))
            }
            DefinitionRef::BaseClass(BaseClassRef::Interface(interface_node)) => {
                let str_name = interface_node.get_name().str_name();
                let predeclared_type = info.class_of(str_name).unwrap().try_into().unwrap();
                if is_builtin.is_none() {
                    let index = interface_node.base_converter().complete_type(
                        info,
                        &predeclared_type,
                        defaults,
                    )?;
                    let constant =
                        ClassConstant::new(str_name, index, predeclared_type.clone().into());
                    self.constants.insert(str_name.to_string(), constant.into());
                } else {
                    interface_node.base_converter().complete_without_reserving(
                        info,
                        &predeclared_type,
                        defaults,
                    )?
                }
                Ok(Some(TypeTypeObject::new(predeclared_type.into()).into()))
            }
            DefinitionRef::BaseClass(BaseClassRef::Union(union_node)) => {
                let str_name = union_node.get_name().str_name();
                let predeclared_type = info.class_of(str_name).unwrap().try_into().unwrap();
                let index = union_node.base_converter().complete_type(
                    info,
                    &predeclared_type,
                    is_builtin.is_none(),
                    defaults,
                )?;
                self.add_constant(
                    str_name.to_string(),
                    index,
                    predeclared_type.clone().into(),
                    &is_builtin,
                );
                Ok(Some(TypeTypeObject::new(predeclared_type.into()).into()))
            }
        }
    }

    fn add_constant(
        &mut self,
        str_name: String,
        index: u16,
        predeclared_type: UserType,
        is_builtin: &Option<BuiltinInfo>,
    ) {
        if is_builtin.is_none() {
            let constant = ClassConstant::new(&str_name, index, predeclared_type);
            self.constants.insert(str_name, constant.into());
        }
    }

    fn link_declaration(
        &mut self,
        info: &mut CompilerInfo,
        decl: &DeclaredAssignmentNode,
    ) -> CompileResult<()> {
        if decl.get_values().len() == 1 && decl.get_types().len() > 1 {
            return Err(CompilerTodoError::of(
                "Multiple returns in top-level declared assignment",
                decl,
            )
            .into());
        } else if decl.get_values().len() != decl.get_types().len() {
            return Err(CompilerException::of(
                "Number of variables does not match number of values",
                decl,
            )
            .into());
        }
        for (dotted_var, (vararg, value)) in zip(decl.get_types(), decl.get_values().pairs()) {
            if !dotted_var.get_type().is_decided() {
                return Err(CompilerException::of(
                    "'var' is illegal in top-level declaration",
                    decl,
                )
                .into());
            } else if !vararg.is_empty() {
                return Err(CompilerTodoError::of(
                    "Varargs in top-level declared assignment",
                    decl,
                )
                .into());
            }
            let name = dotted_var.get_name();
            let ty = info.convert_type(dotted_var.get_type().as_type())?;
            self.globals.insert(name.to_string(), ty.clone());
            if let Option::Some(constant) = TestConverter::constant_return(value, info, 1)? {
                self.constants.insert(name.to_string(), constant.clone());
                info.add_constant_variable(
                    name.to_string(),
                    ty,
                    constant,
                    decl.line_info().clone(),
                );
            } else {
                info.add_variable(name.to_string(), ty, true, decl.line_info().clone());
            }
        }
        Ok(())
    }
}

fn is_auto_interface(def: DefinitionRef<'_>) -> bool {
    match def {
        DefinitionRef::BaseClass(BaseClassRef::Interface(i)) => {
            i.get_descriptors().contains(&DescriptorNode::Auto)
        }
        _ => false,
    }
}

impl Default for Linker {
    fn default() -> Self {
        Self::new()
    }
}
