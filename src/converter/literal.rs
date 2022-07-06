use std::collections::HashSet;

use derive_new::new;

use crate::parser::line_info::Lined;
use crate::parser::literal::LiteralNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::util::first;

use super::base_converter::BaseConverter;
use super::builtins::OBJECT;
use super::bytecode::{ArgcBytecode, Bytecode};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{LangConstant, TupleConstant};
use super::convertible::{
    test_convertible_expected, ConverterBase, ConverterTest, TestConvertible,
};
use super::error::{CompilerException, CompilerInternalError, CompilerTodoError};
use super::test_converter::TestConverter;
use super::type_loader::TypeLoader;
use super::type_obj::{OptionTypeObject, TypeObject};
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileConstant, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct LiteralConverter<'a> {
    node: &'a LiteralNode,
    ret_count: u16,
    expected: Option<&'a [TypeObject]>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum LiteralType {
    List,
    Set,
    Tuple,
}

impl<'a> ConverterTest for LiteralConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        let literal_type = self.literal_type()?;
        let literal_cls = literal_type.get_type(info).make_mut();
        if literal_type == LiteralType::Tuple {
            Ok(vec![
                literal_cls.generify(self.node, self.tuple_return_types(info)?)?
            ])
        } else if self.node.get_builders().is_empty() {
            if let Option::Some(expected) = &self.expected {
                let generics = expected[0].get_generics();
                Ok(vec![literal_cls.generify(self.node, generics.to_vec())?])
            } else {
                Err(CompilerException::of(
                    format!("Cannot deduce type of {} literal", literal_type.name()),
                    self.node,
                )
                .into())
            }
        } else {
            let generics = self.return_types(info)?;
            Ok(vec![literal_cls
                .generify(self.node, vec![generics])?
                .make_mut()])
        }
    }

    fn constant_return(&mut self, info: &mut CompilerInfo) -> CompileConstant {
        if self.literal_type()? != LiteralType::Tuple {
            return Ok(None);
        }
        let mut values = Vec::with_capacity(self.node.get_builders().len());
        for (splat, builder) in self.node.get_builders() {
            let constant = TestConverter::constant_return(builder, info, 1)?;
            if let Option::Some(value) = constant {
                match splat.as_str() {
                    "" => values.push(value),
                    "*" => {
                        if let LangConstant::Tuple(tup) = value {
                            values.extend(tup.get_values().iter().cloned())
                        } else {
                            return Ok(None);
                        }
                    }
                    _ => return Ok(None),
                }
            } else {
                return Ok(None);
            }
        }
        Ok(Some(TupleConstant::new(values).into()))
    }
}

impl<'a> ConverterBase for LiteralConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let literal_type = self.literal_type()?;
        if self.ret_count == 0 {
            warning::warn(
                format!("Unnecessary {} creation", literal_type.name()),
                WarningType::Unused,
                info,
                self.node,
            )?;
            let mut bytes = BytecodeList::new();
            for (_, value) in self.node.get_builders() {
                bytes.extend(BaseConverter::bytes(value, info)?);
            }
            Ok(bytes)
        } else if self.node.get_builders().is_empty() {
            self.convert_empty(info)
        } else {
            self.convert_single(info)
        }
    }
}

impl<'a> LiteralConverter<'a> {
    fn convert_single(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        let literal_type = self.literal_type()?;
        if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!("Literal returns 1 value, expected {}", self.ret_count),
                self.node,
            )
            .into());
        }
        let mut bytes = BytecodeList::new();
        if let Option::Some(constant) = self.constant_return(info)? {
            return Ok(BytecodeList::of(Bytecode::LoadConst(constant.into())));
        }
        let mut unknowns = HashSet::new();
        let mut additional = 0;
        let builders = self.node.get_builders();
        if literal_type == LiteralType::Tuple {
            let ret_types = self.tuple_return_types(info)?;
            for (i, (splat, builder)) in builders.iter().enumerate() {
                additional += self.convert_inner(
                    info,
                    &mut bytes,
                    builder,
                    splat,
                    &ret_types[i + additional as usize],
                    &mut unknowns,
                    i,
                )?;
            }
            self.complete_literal(
                info,
                &mut bytes,
                literal_type,
                &mut unknowns,
                additional,
                None,
            )?;
        } else {
            let ret_type = self.return_types(info)?;
            for (i, (splat, builder)) in builders.iter().enumerate() {
                additional += self.convert_inner(
                    info,
                    &mut bytes,
                    builder,
                    splat,
                    &ret_type,
                    &mut unknowns,
                    i,
                )?;
            }
            self.complete_literal(
                info,
                &mut bytes,
                literal_type,
                &mut unknowns,
                additional,
                Some(ret_type),
            )?;
        }
        Ok(bytes)
    }

    fn convert_empty(&self, info: &mut CompilerInfo) -> CompileBytes {
        let literal_type = self.literal_type()?;
        if literal_type == LiteralType::Tuple {
            Ok(BytecodeList::of(Bytecode::PackTuple(0.into())))
        } else if let Option::Some(expected) = &self.expected {
            let mut bytes = BytecodeList::new();
            let generics = expected[0].get_generics();
            literal_type
                .get_type(info)
                .generify(self.node, generics.to_vec())?;
            bytes.extend(
                TypeLoader::new(self.node.line_info().clone(), self.return_types(info)?)
                    .convert(info)?,
            );
            bytes.add(literal_type.bytecode(0.into()));
            Ok(bytes)
        } else {
            Err(CompilerException::of(
                format!("Cannot deduce type of {} literal", literal_type.name()),
                self.node,
            )
            .into())
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn convert_inner(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        value: &TestNode,
        splat: &str,
        ret_type: &TypeObject,
        unknowns: &mut HashSet<u16>,
        i: usize,
    ) -> CompileResult<u16> {
        match splat {
            "" => {
                bytes.extend(TestConverter::bytes_maybe_option(value, info, 1, ret_type)?);
                if !unknowns.is_empty() {
                    bytes.add(Bytecode::Swap2()); // Keep unknown length on top
                }
                Ok(0)
            }
            "*" => {
                if let Result::Ok(lit) = <&LiteralNode>::try_from(value) {
                    self.convert_star_literal(info, bytes, lit, ret_type, unknowns, i)
                } else {
                    self.convert_star(info, bytes, value, unknowns, i)
                }
            }
            "**" => Err(dict_splat_exception(value).into()),
            _ => Err(unknown_splat_error(value, splat).into()),
        }
    }

    fn convert_star_literal(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        value: &LiteralNode,
        ret_type: &TypeObject,
        unknowns: &mut HashSet<u16>,
        i: usize,
    ) -> CompileResult<u16> {
        let self_type = self.literal_type()?;
        let literal_type = LiteralType::from_brace(value.get_brace_type())
            .ok_or_else(|| literal_err(value, value.get_brace_type()))?;
        if literal_type == LiteralType::Set && self_type != LiteralType::Set {
            return self.convert_star(info, bytes, value, unknowns, i);
        }
        let mut values = HashSet::new();
        let mut additional = value.get_builders().len() - 1;
        for (j, (_, builder)) in value.get_builders().iter().enumerate() {
            let splat = &*value.get_builders()[i].0;
            additional +=
                self.convert_inner(info, bytes, builder, splat, ret_type, &mut values, j)? as usize;
        }
        if !values.is_empty() {
            unknowns.insert(i.try_into().unwrap());
        }
        Ok(additional.try_into().unwrap())
    }

    fn convert_star<T>(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        value: &T,
        unknowns: &mut HashSet<u16>,
        i: usize,
    ) -> CompileResult<u16>
    where
        T: Lined,
        for<'b> &'b T: TestConvertible<'b>,
    {
        let mut converter = value.test_converter(1);
        let conv_ret = first(converter.return_type(info)?);
        let constant = converter.constant_return(info)?;
        if let Option::Some(x) = constant.and_then(|x| TupleConstant::try_from(x).ok()) {
            return self.convert_tuple_literal(bytes, &x);
        }
        bytes.extend(converter.convert(info)?);
        if let TypeObject::Tuple(_) = &conv_ret {
            bytes.add(Bytecode::UnpackTuple());
            if !unknowns.is_empty() {
                Err(CompilerTodoError::of("Tuple packing after dynamic unpack", value).into())
            } else {
                Ok((conv_ret.get_generics().len() - 1).try_into().unwrap())
            }
        } else if conv_ret.operator_info(OpSpTypeNode::Iter, info).is_some() {
            bytes.add(Bytecode::UnpackIterable());
            if !unknowns.is_empty() {
                bytes.add(Bytecode::DupTop());
                bytes.add(Bytecode::SwapDyn());
                bytes.add(Bytecode::Plus());
            }
            unknowns.insert(i.try_into().unwrap());
            Ok(0)
        } else {
            Err(splat_exception(value, &conv_ret).into())
        }
    }

    fn convert_tuple_literal(
        &self,
        bytes: &mut BytecodeList,
        x: &TupleConstant,
    ) -> CompileResult<u16> {
        for val in x.get_values() {
            bytes.add(Bytecode::LoadConst(val.clone().into()))
        }
        Ok((x.get_values().len() - 1).try_into().unwrap())
    }

    fn complete_literal(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        literal_type: LiteralType,
        unknowns: &mut HashSet<u16>,
        additional: u16,
        ret_type: Option<TypeObject>,
    ) -> CompileResult<()> {
        let builder_len = ((self.node.get_builders().len() as u16) + additional)
            .checked_sub(unknowns.len() as u16)
            .expect("Should not have a negative number of builders");
        if unknowns.is_empty() {
            if let Option::Some(ret_type) = ret_type {
                bytes
                    .extend(TypeLoader::new(self.node.line_info().clone(), ret_type).convert(info)?)
            }
            bytes.add(literal_type.bytecode(builder_len.into()))
        } else {
            if literal_type == LiteralType::Tuple {
                let index = *unknowns.iter().next().unwrap();
                return Err(CompilerException::of(
                    "Cannot unpack iterables in tuple literal",
                    &self.node.get_builders()[index as usize].1,
                )
                .into());
            }
            if builder_len != 0 {
                bytes.add(Bytecode::LoadConst(builder_len.into()));
                bytes.add(Bytecode::Plus());
            }
            bytes.extend(
                TypeLoader::new(self.node.line_info().clone(), ret_type.unwrap()).convert(info)?,
            );
            bytes.add(literal_type.dyn_code().unwrap());
        }
        Ok(())
    }

    fn literal_type(&self) -> CompileResult<LiteralType> {
        LiteralType::from_brace(self.node.get_brace_type())
            .ok_or_else(|| literal_err(self.node, self.node.get_brace_type()).into())
    }

    fn return_types(&self, info: &mut CompilerInfo) -> CompileResult<TypeObject> {
        let args = self.node.get_builders();
        let expected_val = self
            .expected
            .filter(|x| self.expected_type_works(info, x))
            .map(|x| x[0].clone());
        let mut result = Vec::with_capacity(args.len());
        for (vararg, arg) in args {
            match &**vararg {
                "" => result.push(first(TestConverter::return_type(info, arg, 1)?)),
                "*" => {
                    let ret_type = first(TestConverter::return_type(info, arg, 1)?);
                    if let TypeObject::Tuple(_) = &ret_type {
                        result.extend(ret_type.get_generics().to_vec());
                    } else if info.builtins().iterable().is_superclass(&ret_type) {
                        result.push(first(info.builtins().de_iterable(&ret_type)?));
                    } else {
                        return Err(splat_exception(arg, &ret_type).into());
                    }
                }
                "**" => return Err(dict_splat_exception(arg).into()),
                _ => return Err(unknown_splat_error(arg, vararg).into()),
            }
        }
        if let Option::Some(expected) = expected_val {
            if args.is_empty() {
                Ok(expected)
            } else {
                Ok(convert_expected(
                    TypeObject::union_of(info, result),
                    expected,
                ))
            }
        } else if args.is_empty() {
            Ok(OBJECT.into())
        } else {
            Ok(TypeObject::union_of(info, result))
        }
    }

    fn tuple_return_types(&self, info: &mut CompilerInfo) -> CompileTypes {
        let mut originals = self.original_return_types(info)?;
        if let Option::Some(expected) = self.expected.filter(|x| self.expected_type_works(info, x))
        {
            let generics = expected[0].get_generics();
            if generics.len() == originals.len() {
                for i in 0..originals.len() {
                    let given = originals[i].clone();
                    let expected = generics[i].clone();
                    originals[i] = convert_expected(given, expected);
                }
            }
        }
        Ok(originals)
    }

    fn original_return_types(&self, info: &mut CompilerInfo) -> CompileTypes {
        let mut result = Vec::with_capacity(self.node.get_builders().len());
        for (splat, arg) in self.node.get_builders() {
            let value = first(TestConverter::return_type(info, arg, 1)?);
            match &**splat {
                "" => result.push(value),
                "*" => {
                    if let TypeObject::Tuple(_) = &value {
                        result.extend(value.get_generics().to_vec());
                    } else if value.operator_info(OpSpTypeNode::Iter, info).is_some() {
                        return Err(CompilerException::of(
                            "Cannot unpack iterable in tuple literal",
                            arg,
                        )
                        .into());
                    } else {
                        return Err(splat_exception(arg, &value).into());
                    }
                }
                "**" => return Err(dict_splat_exception(arg).into()),
                _ => return Err(unknown_splat_error(arg, splat).into()),
            }
        }
        Ok(result)
    }

    fn expected_type_works(&self, info: &CompilerInfo, expected: &[TypeObject]) -> bool {
        self.literal_type()
            .unwrap()
            .get_type(info)
            .same_base_type(&expected[0])
    }
}

impl LiteralType {
    fn from_brace(brace: char) -> Option<LiteralType> {
        match brace {
            '[' => Some(LiteralType::List),
            '{' => Some(LiteralType::Set),
            '(' => Some(LiteralType::Tuple),
            _ => None,
        }
    }

    fn get_type<'a>(&self, info: &'a CompilerInfo) -> &'a TypeObject {
        let builtins = info.builtins();
        match self {
            LiteralType::List => builtins.list_type(),
            LiteralType::Set => builtins.set_type(),
            LiteralType::Tuple => builtins.tuple_type(),
        }
    }

    fn bytecode(&self, argc: ArgcBytecode) -> Bytecode {
        match self {
            LiteralType::List => Bytecode::ListCreate(argc),
            LiteralType::Set => Bytecode::SetCreate(argc),
            LiteralType::Tuple => Bytecode::PackTuple(argc),
        }
    }

    fn dyn_code(&self) -> Option<Bytecode> {
        match self {
            LiteralType::List => Some(Bytecode::ListDyn()),
            LiteralType::Set => Some(Bytecode::SetDyn()),
            LiteralType::Tuple => None,
        }
    }

    const fn name(&self) -> &'static str {
        match self {
            LiteralType::List => "list",
            LiteralType::Set => "set",
            LiteralType::Tuple => "tuple",
        }
    }
}

fn literal_err(info: impl Lined, brace: char) -> CompilerInternalError {
    CompilerInternalError::of(format!("Unknown brace type {}", brace), info)
}

fn convert_expected(given: TypeObject, expected: TypeObject) -> TypeObject {
    if expected.is_superclass(&given) || OptionTypeObject::needs_and_super(&expected, &given) {
        expected
    } else {
        given
    }
}

fn splat_exception(info: impl Lined, ty: &TypeObject) -> CompilerException {
    CompilerException::of(
        format!(
            "Cannot unpack type '{}': Unpacking is only valid on tuples or iterables",
            ty.name()
        ),
        info,
    )
}

fn dict_splat_exception(info: impl Lined) -> CompilerException {
    CompilerException::of(
        "Dictionary unpacking with '**' is only allowed in dict literals",
        info,
    )
}

fn unknown_splat_error(info: impl Lined, splat: &str) -> CompilerInternalError {
    CompilerInternalError::of(format!("Unknown splat type '{}'", splat), info)
}

test_convertible_expected!(LiteralNode, LiteralConverter);
