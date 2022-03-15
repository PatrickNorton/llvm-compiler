use crate::parser::test_node::TestNode;

use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::diverge::DivergingInfo;
use super::test_converter::TestConverter;
use super::type_obj::TypeObject;
use super::{CompileBytes, CompileConstant, CompileResult, CompileTypes};

// NOTE: I don't think these need to take &mut self on all of these methods

pub trait ConverterBase {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes;

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.convert(info).map(|x| (x, DivergingInfo::new()))
    }
}

pub trait ConverterTest: ConverterBase {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes;

    #[allow(unused_variables)] // I don't think this should be a lint here, but whatever...
    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        Ok(None)
    }
}

pub trait BaseConvertible<'a> {
    type Converter: ConverterBase + 'a;

    fn base_converter(self) -> Self::Converter;
}

pub trait TestConvertible<'a> {
    type Converter: ConverterTest + 'a;

    fn test_converter(self, ret_count: u16) -> Self::Converter;

    #[allow(unused_variables)]
    fn test_conv_expected(self, ret_count: u16, expected: Vec<TypeObject>) -> Self::Converter
    where
        Self: Sized,
    {
        self.test_converter(ret_count)
    }
}

impl<'a> BaseConvertible<'a> for &'a TestNode {
    type Converter = TestConverter<'a>;

    fn base_converter(self) -> Self::Converter {
        self.test_converter(0)
    }
}

macro_rules! base_convertible {
    ($node:ty, $($conv:ident)::+) => {
        impl<'a> $crate::converter::convertible::BaseConvertible<'a> for &'a $node {
            type Converter = $($conv)::+<'a>;

            fn base_converter(self) -> Self::Converter {
                $($conv)::+::new(self)
            }
        }
    };
}

pub(super) use base_convertible;

macro_rules! test_convertible {
    ($node:ty, $($conv:ident)::+) => {
        impl<'a> $crate::converter::convertible::BaseConvertible<'a> for &'a $node {
            type Converter = $($conv)::+<'a>;

            fn base_converter(self) -> Self::Converter {
                $($conv)::+::new(self, 0)
            }
        }

        impl<'a> $crate::converter::convertible::TestConvertible<'a> for &'a $node {
            type Converter = $($conv)::+<'a>;

            fn test_converter(self, ret_count: u16) -> Self::Converter {
                $($conv)::+::new(self, ret_count)
            }
        }
    }
}

macro_rules! test_convertible_expected {
    ($node:ty, $($conv:ident)::+) => {
        impl<'a> $crate::converter::convertible::BaseConvertible<'a> for &'a $node {
            type Converter = $($conv)::+<'a>;

            fn base_converter(self) -> Self::Converter {
                $($conv)::+::new(self, 0, None)
            }
        }

        impl<'a> $crate::converter::convertible::TestConvertible<'a> for &'a $node {
            type Converter = $($conv)::+<'a>;

            fn test_converter(self, ret_count: u16) -> Self::Converter {
                $($conv)::+::new(self, ret_count, None)
            }

            fn test_conv_expected(self, ret_count: u16, expected: Vec<TypeObject>) -> Self::Converter
            where
                Self: Sized,
            {
                $($conv)::+::new(self, ret_count, Some(expected))
            }
        }
    };
}

pub(super) use {test_convertible, test_convertible_expected};
