use derive_new::new;
use num::ToPrimitive;

use crate::parser::formatted_string::{FormatInfo, FormatSign, FormatType, FormattedStringNode};
use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::util::decimal::DecimalRef;
use crate::util::first;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{FormatConstant, LangConstant};
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::{CompilerException, CompilerTodoError};
use super::test_converter::TestConverter;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct FormattedStringConverter<'a> {
    node: &'a FormattedStringNode,
    ret_count: u16,
}

impl<'a> ConverterTest for FormattedStringConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        Ok(vec![info.builtins().str_type().clone()])
    }
}

impl<'a> ConverterBase for FormattedStringConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            warning::warn(
                "Unused formatted string",
                WarningType::Unused,
                info,
                self.node,
            )?;
        } else if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!(
                    "F-string literal returns only one argument, not {}",
                    self.ret_count
                ),
                self.node,
            )
            .into());
        }
        let mut bytes = BytecodeList::new();
        let strings = self.node.get_strings();
        let formats = self.node.get_formats();
        let tests = self.node.get_tests();
        assert!(
            strings.len() == tests.len() || strings.len() == tests.len() + 1,
            "Unbalanced strings and tests ({} vs {})",
            strings.len(),
            tests.len()
        );
        if tests.is_empty() {
            warning::warn(
                "F-string with no formatted arguments",
                WarningType::TrivialValue,
                info,
                self.node,
            )?;
        }
        for (i, string) in strings.iter().enumerate() {
            if let Option::Some(test) = tests.get(i) {
                let format = &formats[i];
                let str_value = self.arg_constant(info, test, format)?;
                // This would be improved with feature(if_let_chains), (#53667)
                if let (true, Option::Some(str_value)) = (!format.is_empty(), str_value) {
                    let string = strings[i].clone() + &str_value;
                    bytes.add(Bytecode::LoadConst(string.into()));
                    if i != 0 {
                        bytes.add(Bytecode::Plus());
                    }
                } else {
                    bytes.add(Bytecode::LoadConst((&**string).into()));
                    if i != 0 {
                        bytes.add(Bytecode::Plus());
                    }
                    self.convert_argument(info, test, &mut bytes, format)?;
                    bytes.add(Bytecode::Plus());
                }
            } else {
                bytes.add(Bytecode::LoadConst((&**string).into()));
                if i != 0 {
                    bytes.add(Bytecode::Plus());
                }
            }
        }
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }
}

impl<'a> FormattedStringConverter<'a> {
    fn arg_constant(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        format: &FormatInfo,
    ) -> CompileResult<Option<String>> {
        if !format.only_type() {
            return Ok(None);
        }
        if !format.is_empty() {
            let mut converter = arg.test_converter(1);
            match format.fmt_type {
                FormatType::Str | FormatType::None => converter
                    .constant_return(info)
                    .map(|x| x.and_then(|y| y.str_value())),
                FormatType::Repr => converter
                    .constant_return(info)
                    .map(|x| x.and_then(|y| y.repr_value())),
                FormatType::Number | FormatType::Decimal => self.decimal_constant(info, converter),
                FormatType::LowerHex => self.hex_constant(info, converter),
                FormatType::Octal => self.octal_constant(info, converter),
                FormatType::Binary => self.binary_constant(info, converter),
                FormatType::Character => self.char_constant(info, converter),
                FormatType::UpperHex => self.upper_hex_constant(info, converter),
                FormatType::Scientific => self.exp_constant(info, converter),
                FormatType::UpperSci => self.upper_exp_constant(info, converter),
                FormatType::Fixed => self.fixed_constant(info, converter),
                FormatType::UpperFixed => self.upper_fixed_constant(info, converter),
                FormatType::General => self.general_float_constant(info, converter),
                FormatType::UpperGeneral => self.upper_general_constant(info, converter),
                FormatType::Percentage => self.percent_constant(info, converter),
            }
        } else {
            TestConverter::constant_return(arg, info, 1).map(|x| x.and_then(|y| y.str_value()))
        }
    }

    fn decimal_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match &value {
                LangConstant::Bigint(_) | LangConstant::Int(_) => Ok(value.str_value()),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn hex_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Int(val) => Ok(Some(format!("{:x}", val.get_value()))),
                LangConstant::Bigint(val) => Ok(Some(format!("{:x}", val.get_value()))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn octal_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Int(val) => Ok(Some(format!("{:o}", val.get_value()))),
                LangConstant::Bigint(val) => Ok(Some(format!("{:o}", val.get_value()))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn binary_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Int(val) => Ok(Some(format!("{:b}", val.get_value()))),
                LangConstant::Bigint(val) => Ok(Some(format!("{:b}", val.get_value()))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn char_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Char(c) => Ok(Some(c.get_value().to_string())),
                LangConstant::Int(i) => Ok(int_to_char(&i.get_value())),
                LangConstant::Bigint(b) => Ok(int_to_char(b.get_value())),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn upper_hex_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Int(val) => Ok(Some(format!("{:X}", val.get_value()))),
                LangConstant::Bigint(val) => Ok(Some(format!("{:X}", val.get_value()))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn exp_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Decimal(d) => {
                    Ok(Some(self.exp_constant_inner(d.get_value().as_ref())))
                }
                LangConstant::Bigint(b) => Ok(Some(
                    self.exp_constant_inner(DecimalRef::from_int(b.get_value())),
                )),
                LangConstant::Int(i) => Ok(Some(
                    self.exp_constant_inner(DecimalRef::from_int(&i.get_value().into())),
                )),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn exp_constant_inner(&self, dec_val: DecimalRef) -> String {
        let shift = dec_val.precision() - dec_val.scale() as usize - 1;
        let result = dec_val.move_point_left(shift);
        format!("{:.6}e{:+03}", result, shift)
    }

    fn upper_exp_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Decimal(d) => Ok(Some(
                    self.exp_constant_inner(d.get_value().as_ref())
                        .to_uppercase(),
                )),
                LangConstant::Bigint(b) => Ok(Some(
                    self.exp_constant_inner(DecimalRef::from_int(b.get_value()))
                        .to_uppercase(),
                )),
                LangConstant::Int(i) => Ok(Some(
                    self.exp_constant_inner(DecimalRef::from_int(&i.get_value().into()))
                        .to_uppercase(),
                )),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn fixed_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Decimal(d) => Ok(Some(format!("{:.6}", d.get_value()))),
                LangConstant::Bigint(b) => Ok(Some(format!("{}.000000", b.get_value()))),
                LangConstant::Int(i) => Ok(Some(format!("{}.000000", i.get_value()))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn upper_fixed_constant(
        &self,
        info: &mut CompilerInfo,
        converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        self.fixed_constant(info, converter)
            .map(|x| x.map(|y| y.to_ascii_uppercase()))
    }

    fn general_float_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Decimal(d) => Ok(Some(format!("{}", d.get_value()))),
                LangConstant::Bigint(b) => {
                    Ok(Some(format!("{}", DecimalRef::from_int(b.get_value()))))
                }
                LangConstant::Int(i) => Ok(Some(format!("{}", i.get_value() as f64))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn upper_general_constant(
        &self,
        info: &mut CompilerInfo,
        converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        self.general_float_constant(info, converter)
            .map(|x| x.map(|y| y.to_ascii_uppercase()))
    }

    fn percent_constant(
        &self,
        info: &mut CompilerInfo,
        mut converter: impl ConverterTest,
    ) -> CompileResult<Option<String>> {
        if let Option::Some(value) = converter.constant_return(info)? {
            match value {
                LangConstant::Decimal(d) => Ok(Some(format!("{:.2}%", d.get_value()))),
                LangConstant::Bigint(b) => Ok(Some(format!("{}00%", b.get_value()))),
                LangConstant::Int(i) => Ok(Some(format!("{}00%", i.get_value()))),
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn convert_argument(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        if !format.is_empty() {
            self.convert_format_args(info, arg, bytes, format)
        } else {
            self.convert_to_str(info, arg, bytes, format)
        }
    }

    fn convert_format_args(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        assert!(!format.is_empty());
        match format.fmt_type {
            FormatType::Str | FormatType::None => self.convert_to_str(info, arg, bytes, format),
            FormatType::Repr => self.convert_to_repr(info, arg, bytes, format),
            FormatType::Decimal => self.convert_to_int(info, arg, bytes, format),
            FormatType::LowerHex => self.convert_to_base(info, arg, 16, bytes, format),
            FormatType::UpperHex => self.convert_to_upper_hex(info, arg, bytes, format),
            FormatType::Octal => self.convert_to_base(info, arg, 8, bytes, format),
            FormatType::Binary => self.convert_to_base(info, arg, 2, bytes, format),
            FormatType::Character => self.convert_to_char(info, arg, bytes, format),
            FormatType::Number
            | FormatType::Scientific
            | FormatType::UpperSci
            | FormatType::Fixed
            | FormatType::UpperFixed
            | FormatType::General
            | FormatType::UpperGeneral
            | FormatType::Percentage => self.convert_decimal(info, arg, bytes, format),
        }
    }

    fn convert_to_str(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        let mut converter = arg.test_converter(1);
        let ret_type = &converter.return_type(info)?[0];
        let is_not_str = !info.builtins().str_type().is_superclass(ret_type);
        if format.only_type() {
            bytes.extend(converter.convert(info)?);
            if is_not_str {
                bytes.add(Bytecode::CallOp(OpSpTypeNode::Str.into(), 0.into()));
            }
        } else {
            let fmt_args = FormatConstant::new(format.clone());
            self.check_str_format(format)?;
            bytes.add(Bytecode::LoadConst(
                info.builtins().format_const().clone().into(),
            ));
            bytes.extend(converter.convert(info)?);
            if is_not_str {
                bytes.add(Bytecode::CallOp(OpSpTypeNode::Str.into(), 0.into()));
            }
            bytes.add(Bytecode::LoadConst(fmt_args.into()));
            bytes.add(Bytecode::CallTos(1.into()))
        }
        Ok(())
    }

    fn convert_to_repr(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        let mut converter = arg.test_converter(1);
        if format.only_type() {
            bytes.extend(converter.convert(info)?);
            bytes.add(Bytecode::CallOp(OpSpTypeNode::Repr.into(), 0.into()));
        } else {
            let fmt_args = FormatConstant::new(format.clone());
            self.check_str_format(format)?;
            bytes.add(Bytecode::LoadConst(
                info.builtins().format_const().clone().into(),
            ));
            bytes.extend(converter.convert(info)?);
            bytes.add(Bytecode::CallOp(OpSpTypeNode::Repr.into(), 0.into()));
            bytes.add(Bytecode::LoadConst(fmt_args.into()));
            bytes.add(Bytecode::CallTos(1.into()))
        }
        Ok(())
    }

    fn check_str_format(&self, format: &FormatInfo) -> CompileResult<()> {
        if format.sign != FormatSign::None {
            Err(CompilerException::of(
                "Sign specifier is invalid in non-numeric format specifiers",
                format,
            )
            .into())
        } else if format.precision != 0 {
            Err(CompilerException::of(
                "Precision is not allowed in non-numeric format specifiers",
                format,
            )
            .into())
        } else {
            Ok(())
        }
    }

    fn convert_to_int(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        if format.only_type() {
            let mut converter = arg.test_converter(1);
            let ret_type = first(converter.return_type(info)?);
            bytes.extend(converter.convert(info)?);
            make_int(info, &ret_type, arg, bytes)?;
            bytes.add(Bytecode::CallOp(OpSpTypeNode::Str.into(), 0.into()));
            Ok(())
        } else {
            self.convert_fmt_int(info, arg, bytes, format)
        }
    }

    fn convert_to_base(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        base: u32,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        if base == 10 {
            self.convert_to_int(info, arg, bytes, format)
        } else if format.only_type() {
            let mut converter = arg.test_converter(1);
            let ret_type = first(converter.return_type(info)?);
            bytes.extend(converter.convert(info)?);
            make_int(info, &ret_type, arg, bytes)?;
            bytes.add(Bytecode::LoadConst(base.into()));
            bytes.add(Bytecode::CallMethod("strBase".into(), 1.into()));
            Ok(())
        } else {
            self.convert_fmt_int(info, arg, bytes, format)
        }
    }

    fn convert_to_upper_hex(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        assert_eq!(format.fmt_type, FormatType::UpperHex);
        self.convert_fmt_int(info, arg, bytes, format)
    }

    fn convert_to_char(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        if !format.only_type() {
            return Err(CompilerTodoError::of("Non-simple char conversion", arg).into());
        }
        let mut converter = arg.test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        let builtins = info.builtins();
        if builtins.char_type().is_superclass(&ret_type) {
            bytes.extend(converter.convert(info)?);
            bytes.add(Bytecode::CallOp(OpSpTypeNode::Str.into(), 0.into()));
            Ok(())
        } else if builtins.int_type().is_superclass(&ret_type) {
            bytes.add(Bytecode::LoadConst(builtins.char_constant().clone().into()));
            bytes.extend(converter.convert(info)?);
            bytes.add(Bytecode::CallTos(1.into()));
            Ok(())
        } else {
            Err(CompilerException::of(
                format!(
                    "'c' format argument expects either an int or a char, not '{}'",
                    ret_type.name()
                ),
                arg,
            )
            .into())
        }
    }

    fn convert_decimal(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        let ret_type = convert_fmt_load(info, arg, bytes)?;
        let builtins = info.builtins();
        if !builtins.int_type().is_superclass(&ret_type)
            && !builtins.dec_type().is_superclass(&ret_type)
        {
            Err(CompilerException::of(
                format!(
                    "Decimal format specifiers require either \
                     an integer or decimal argument, not '{}'",
                    ret_type.name()
                ),
                format,
            )
            .into())
        } else {
            bytes.add(Bytecode::LoadConst(format.clone().into()));
            bytes.add(Bytecode::CallTos(2.into()));
            Ok(())
        }
    }

    fn convert_fmt_int(
        &self,
        info: &mut CompilerInfo,
        arg: &TestNode,
        bytes: &mut BytecodeList,
        format: &FormatInfo,
    ) -> CompileResult<()> {
        if format.precision != 0 {
            return Err(CompilerException::of(
                "Precision is not allowed in integer format specifier",
                format,
            )
            .into());
        }
        let ret_type = convert_fmt_load(info, arg, bytes)?;
        make_int(info, &ret_type, arg, bytes)?;
        bytes.add(Bytecode::LoadConst(format.clone().into()));
        bytes.add(Bytecode::CallTos(2.into()));
        Ok(())
    }
}

fn make_int(
    info: &mut CompilerInfo,
    ret_type: &TypeObject,
    arg: impl Lined,
    bytes: &mut BytecodeList,
) -> CompileResult<()> {
    if info.builtins().int_type().is_superclass(ret_type) {
        ret_type.try_operator_info(arg, OpSpTypeNode::Int, info)?;
        bytes.add(Bytecode::CallOp(OpSpTypeNode::Int.into(), 0.into()));
    }
    Ok(())
}

fn convert_fmt_load(
    info: &mut CompilerInfo,
    arg: &TestNode,
    bytes: &mut BytecodeList,
) -> CompileResult<TypeObject> {
    let mut converter = arg.test_converter(1);
    let ret_type = first(converter.return_type(info)?);
    let format = info.builtins().format_const().clone();
    bytes.add(Bytecode::LoadConst(format.into()));
    bytes.extend(converter.convert(info)?);
    Ok(ret_type)
}

fn int_to_char(x: &impl ToPrimitive) -> Option<String> {
    x.to_u32()
        .and_then(|x| char::try_from(x).ok())
        .map(|x| x.to_string())
}

test_convertible!(FormattedStringNode, FormattedStringConverter);
