use std::iter::zip;

use derive_new::new;

use crate::parser::ternary::TernaryNode;
use crate::util::first;

use super::bytecode::{Bytecode, Label};
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileTypes};

#[derive(Debug, new)]
pub struct TernaryConverter<'a> {
    node: &'a TernaryNode,
    ret_count: u16,
}

impl<'a> ConverterTest for TernaryConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        let if_true = TestConverter::return_type(info, self.node.get_if_true(), self.ret_count)?;
        let if_false = TestConverter::return_type(info, self.node.get_if_false(), self.ret_count)?;
        assert!(
            (self.ret_count as usize) <= if_true.len()
                && (self.ret_count as usize) <= if_false.len()
        );
        Ok(zip(if_true, if_false)
            .map(|(x, y)| TypeObject::union(info, [x, y]))
            .collect())
    }
}

impl<'a> ConverterBase for TernaryConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let mut cond_converter = self.node.get_statement().test_converter(1);
        if let Option::Some(bool_val) = cond_converter
            .constant_return(info)?
            .and_then(|x| x.bool_value())
        {
            return self.convert_opt(info, bool_val);
        }
        let mut bytes = cond_converter.convert(info)?;
        let jump1 = Label::new();
        bytes.add(Bytecode::JumpFalse(jump1.clone().into()));
        let mut if_true_converter = self.node.get_if_true().test_converter(self.ret_count);
        bytes.extend(if_true_converter.convert(info)?);
        let ret_type = first(self.return_type(info)?);
        if self.ret_count == 1
            && OptionTypeObject::needs_make_option(
                &ret_type,
                &if_true_converter.return_type(info)?[0],
            )
        {
            bytes.add(Bytecode::MakeOption());
        }
        let jump2 = Label::new();
        bytes.add(Bytecode::Jump(jump2.clone().into()));
        bytes.add_label(jump1);
        let mut if_false_converter = self.node.get_if_false().test_converter(self.ret_count);
        bytes.extend(if_false_converter.convert(info)?);
        if self.ret_count == 1
            && OptionTypeObject::needs_make_option(
                &ret_type,
                &if_false_converter.return_type(info)?[0],
            )
        {
            bytes.add(Bytecode::MakeOption());
        }
        bytes.add_label(jump2);
        Ok(bytes)
    }
}

impl<'a> TernaryConverter<'a> {
    fn convert_opt(&self, info: &mut CompilerInfo, bool_val: bool) -> CompileBytes {
        warning::warn(
            format!("Condition of ternary always evaluates to {}", bool_val),
            WarningType::Unreachable,
            info,
            self.node.get_statement(),
        )?;
        let (eval, not_eval) = if bool_val {
            (self.node.get_if_true(), self.node.get_if_false())
        } else {
            (self.node.get_if_false(), self.node.get_if_true())
        };
        TestConverter::bytes(not_eval, info, self.ret_count)?; // Check for errors, but don't add to bytes
        TestConverter::bytes(eval, info, self.ret_count)
    }
}

test_convertible!(TernaryNode, TernaryConverter);
