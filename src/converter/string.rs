use std::collections::HashSet;

use derive_new::new;

use crate::parser::string::StringNode;
use crate::parser::string_like::StringPrefix;
use crate::util::string_escape::escaped;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{BytesConstant, CharConstant, LangConstant};
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::{CompilerException, CompilerTodoError};
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct StringConverter<'a> {
    node: &'a StringNode,
    ret_count: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum StringType {
    Str,
    Bytes,
    Char,
    Byte,
}

impl<'a> ConverterTest for StringConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![
            match StringType::from_prefixes(self.node.get_prefixes()) {
                StringType::Str => info.builtins().str_type().clone(),
                StringType::Bytes => info.builtins().bytes_type().clone(),
                StringType::Char => info.builtins().char_type().clone(),
                StringType::Byte => info.builtins().int_type().clone(),
            },
        ])
    }

    fn constant_return(&mut self, _info: &mut CompilerInfo) -> CompileConstant {
        self.constant().map(Some)
    }
}

impl<'a> ConverterBase for StringConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        assert!(!self.node.get_prefixes().contains(&StringPrefix::Formatted));
        if self.ret_count == 0 {
            warning::warn(
                "String-like literal unused",
                WarningType::Unused,
                info,
                self.node,
            )?;
            return Ok(BytecodeList::new());
        }
        if self.node.get_prefixes().contains(&StringPrefix::Regex) {
            return Err(CompilerTodoError::of("Regex strings not yet supported", self.node).into());
        }
        Ok(BytecodeList::of(Bytecode::LoadConst(
            self.constant()?.into(),
        )))
    }
}

impl<'a> StringConverter<'a> {
    fn constant(&self) -> CompileResult<LangConstant> {
        match StringType::from_prefixes(self.node.get_prefixes()) {
            StringType::Str => Ok(self.node.get_contents().into()),
            StringType::Bytes => {
                let contents = self.node.get_contents().as_bytes().to_vec();
                Ok(BytesConstant::new(contents).into())
            }
            StringType::Char => {
                self.check_len("Char")?;
                Ok(CharConstant::new(self.node.get_contents().chars().next().unwrap()).into())
            }
            StringType::Byte => {
                self.check_len("Byte")?;
                let cp = self.node.get_contents().chars().next().unwrap();
                if (cp as u32) < 0x80 {
                    Ok((cp as u32).into())
                } else {
                    Err(CompilerException::of(
                        format!(
                            "Byte literals only support ASCII values, not '{}' (value {:#x})",
                            escaped(cp),
                            cp as u32
                        ),
                        self.node,
                    )
                    .into())
                }
            }
        }
    }

    fn check_len(&self, lit_type: &str) -> CompileResult<()> {
        if self.node.get_contents().chars().count() != 1 {
            Err(CompilerException::of(
                format!("{lit_type} literals must have a length of 1"),
                self.node,
            )
            .into())
        } else {
            Ok(())
        }
    }
}

impl StringType {
    fn from_prefixes(prefixes: &HashSet<StringPrefix>) -> StringType {
        if prefixes.contains(&StringPrefix::Bytes) {
            StringType::Bytes
        } else if prefixes.contains(&StringPrefix::Char) {
            StringType::Char
        } else if prefixes.contains(&StringPrefix::Byte) {
            StringType::Byte
        } else {
            StringType::Str
        }
    }
}

test_convertible!(StringNode, StringConverter);
