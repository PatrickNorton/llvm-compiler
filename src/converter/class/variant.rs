use derive_new::new;

use crate::converter::bytecode::Bytecode;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::{
    test_convertible, ConverterBase, ConverterTest, TestConvertible,
};
use crate::converter::error::CompilerException;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_obj::TypeObject;
use crate::converter::warning::WarningType;
use crate::converter::{warning, CompileBytes, CompileTypes};
use crate::parser::variant::VariantCreationNode;
use crate::util::first;

#[derive(Debug, new)]
pub struct VariantConverter<'a> {
    node: &'a VariantCreationNode,
    ret_count: u16,
}

impl<'a> ConverterTest for VariantConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        let mut union_converter = self.node.get_union().test_converter(1);
        let first = first(union_converter.return_type(info)?);
        let ret_type = &first.get_generics()[0];
        Ok(vec![ret_type.make_mut()])
    }
}

impl<'a> ConverterBase for VariantConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count == 0 {
            warning::warn(
                "Unused variant access",
                WarningType::Unused,
                info,
                self.node,
            )?;
        } else if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!(
                    "Union variant can only return one value, not {}",
                    self.ret_count
                ),
                self.node,
            )
            .into());
        }
        let mut union_converter = self.node.get_union().test_converter(1);
        let ret_type = first(union_converter.return_type(info)?);
        assert!(matches!(ret_type, TypeObject::Type(_)));
        let mut bytes = union_converter.convert(info)?;
        bytes.extend(TestConverter::bytes(self.node.get_value(), info, 1)?);
        bytes.add(Bytecode::MakeVariant(self.node.get_variant_no().into()));
        if self.ret_count == 0 {
            bytes.add(Bytecode::PopTop());
        }
        Ok(bytes)
    }
}

test_convertible!(VariantCreationNode, VariantConverter);
