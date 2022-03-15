use derive_new::new;

use crate::parser::literal::DictLiteralNode;
use crate::parser::test_node::TestNode;
use crate::util::first;

use super::builtins::OBJECT;
use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible_expected, ConverterBase, ConverterTest};
use super::error::CompilerException;
use super::test_converter::TestConverter;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct DictLiteralConverter<'a> {
    node: &'a DictLiteralNode,
    ret_count: u16,
    expected: Option<Vec<TypeObject>>,
}

impl<'a> ConverterTest for DictLiteralConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        if self.node.get_values().is_empty() {
            match &self.expected {
                Option::None => Err(CompilerException::of(
                    "Cannot deduce type of dict literal",
                    self.node,
                )
                .into()),
                Option::Some(expected) => {
                    let generics = expected[0].get_generics();
                    Ok(vec![info
                        .builtins()
                        .dict_type()
                        .generify(self.node, generics.to_vec())?
                        .make_mut()])
                }
            }
        } else {
            let key_type = return_types(info, self.node.get_values().iter().map(|x| &x.0))?;
            let val_type = return_types(info, self.node.get_values().iter().map(|x| &x.1))?;
            Ok(vec![info
                .builtins()
                .dict_type()
                .generify(self.node, vec![key_type, val_type])?
                .make_mut()])
        }
    }
}

impl<'a> ConverterBase for DictLiteralConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut bytes = BytecodeList::new();
        if self.ret_count == 0 {
            warning::warn(
                "Unnecessary dict creation",
                WarningType::Unused,
                info,
                self.node,
            )?;
            for (key, val) in self.node.get_values() {
                bytes.extend(TestConverter::bytes(key, info, 0)?);
                bytes.extend(TestConverter::bytes(val, info, 0)?);
            }
        } else {
            if self.ret_count != 1 {
                return Err(CompilerException::of(
                    format!("Dict literal only returns 1 value, not {}", self.ret_count),
                    self.node,
                )
                .into());
            }
            let key_type = return_types(info, self.node.get_values().iter().map(|x| &x.0))?;
            let val_type = return_types(info, self.node.get_values().iter().map(|x| &x.1))?;
            for (key, val) in self.node.get_values() {
                bytes.extend(TestConverter::bytes_maybe_option(
                    key,
                    info,
                    1,
                    key_type.clone(),
                )?);
                bytes.extend(TestConverter::bytes_maybe_option(
                    val,
                    info,
                    1,
                    val_type.clone(),
                )?);
            }
            bytes.add(Bytecode::DictCreate((self.node.len() as u16).into()))
        }
        Ok(bytes)
    }
}

fn return_types<'a>(
    info: &mut CompilerInfo,
    values: impl IntoIterator<Item = &'a TestNode>,
) -> CompileResult<TypeObject> {
    let result = values
        .into_iter()
        .map(|x| TestConverter::return_type(info, x, 1).map(first))
        .collect::<CompileResult<Vec<_>>>()?;
    Ok(if result.is_empty() {
        OBJECT.into()
    } else {
        TypeObject::union_of(info, result)
    })
}

test_convertible_expected!(DictLiteralNode, DictLiteralConverter);
