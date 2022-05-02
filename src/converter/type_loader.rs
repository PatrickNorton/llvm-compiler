use derive_new::new;

use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::type_node::TypeNode;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{ClassConstant, OptionTypeConstant};
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::{CompilerException, CompilerTodoError};
use super::type_obj::{OptionTypeObject, TemplateParam, TypeObject, UserType};
use super::{CompileBytes, CompileConstant, CompileTypes};

#[derive(Debug, new)]
pub struct TypeConverter<'a> {
    node: &'a TypeNode,
    ret_count: u16,
}

#[derive(Debug, new)]
pub struct TypeLoader {
    line_info: LineInfo,
    value: TypeObject,
}

impl<'a> ConverterTest for TypeConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        TypeLoader::new(self.node.line_info().clone(), info.convert_type(self.node)?)
            .return_type(info)
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        TypeLoader::new(self.node.line_info().clone(), info.convert_type(self.node)?)
            .constant_return(info)
    }
}

impl<'a> ConverterBase for TypeConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        assert_eq!(self.ret_count, 1);
        TypeLoader::new(self.node.line_info().clone(), info.convert_type(self.node)?).convert(info)
    }
}

impl ConverterTest for TypeLoader {
    fn return_type(&mut self, _info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![self.value.get_type()])
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        Self::type_constant(&self.line_info, &self.value, info)
    }
}

impl ConverterBase for TypeLoader {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if let Option::Some(constant) = self.constant_return(info)? {
            Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())))
        } else {
            let value = <&TemplateParam>::try_from(&self.value).unwrap();
            TypeLoader::new(self.line_info.clone(), value.get_bound().clone()).convert(info)
            // Commented out until better type inference can come around
            //
            // let parent = info.local_parent(value).map_err(|| {
            //     CompilerException::of(format!("Unknown type {}", value.name()), line_info)
            // })?;
            // let mut bytes = TypeLoader::new(self.line_info.clone(), parent).convert(info)?;
            // bytes.add(Bytecode::LoadDot(value.base_name().into()));
            // Ok(bytes)
        }
    }
}

impl TypeLoader {
    pub fn type_constant(
        line_info: impl Lined,
        value: &TypeObject,
        info: &mut CompilerInfo,
    ) -> CompileConstant {
        if let TypeObject::Option(opt) = value {
            return Self::option_constant(line_info, opt, info);
        }
        let name = value.base_name();
        if name.is_empty() {
            return Err(CompilerTodoError::of(
                "Error in literal conversion: Lists of non-nameable types not complete yet",
                line_info,
            )
            .into());
        }
        let builtins = info.builtins();
        if name == "null" {
            Ok(Some(builtins.null_type_constant().clone()))
        } else if builtins.has_type(&name) {
            builtins
                .constant_of(&name)
                .map(|x| x.into_owned())
                .ok_or_else(|| {
                    CompilerException::of(format!("Type {} not found", name), line_info).into()
                })
                .map(Some)
        } else if let Result::Ok(user) = UserType::try_from(value.clone()) {
            let index = info.class_index(&user);
            let constant = ClassConstant::new(&name, index, user);
            Ok(Some(constant.into()))
        } else if info.var_holder().local_parent(value).is_some() {
            Ok(None)
        } else {
            Err(CompilerException::of(format!("Type '{}' not found", name), line_info).into())
        }
    }

    fn option_constant(
        line_info: impl Lined,
        value: &OptionTypeObject,
        info: &mut CompilerInfo,
    ) -> CompileConstant {
        let interior_type = value.get_option_val();
        let type_const = Self::type_constant(line_info, interior_type, info)?;
        if let Option::Some(type_const) = type_const {
            Ok(Some(
                OptionTypeConstant::new(type_const, interior_type.clone()).into(),
            ))
        } else {
            Ok(None)
        }
    }
}

test_convertible!(TypeNode, TypeConverter);
