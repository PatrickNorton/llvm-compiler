use derive_new::new;

use crate::parser::comprehension::ComprehensionNode;
use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::typed_var::TypedVariableNode;
use crate::parser::variable::VarLikeNode;
use crate::util::first;

use super::argument::ArgumentInfo;
use super::builtins::FORBIDDEN_NAMES;
use super::bytecode::Label;
use super::bytecode::{ArgcBytecode, Bytecode};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{test_convertible, ConverterBase, ConverterTest};
use super::error::CompilerTodoError;
use super::error::{CompilerException, CompilerInternalError};
use super::fn_info::FunctionInfo;
use super::function::Function;
use super::generic::GenericInfo;
use super::test_converter::TestConverter;
use super::type_loader::TypeLoader;
use super::type_obj::TypeObject;
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct ComprehensionConverter<'a> {
    node: &'a ComprehensionNode,
    ret_count: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum BraceType {
    List,
    Set,
    Generator,
}

impl<'a> ConverterTest for ComprehensionConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        let generic = self.generic_type(info)?;
        let ty = self.get_brace()?.get_type(info);
        ty.generify(self.node, vec![generic]).map(|x| vec![x])
    }
}

impl<'a> ConverterBase for ComprehensionConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.ret_count > 1 {
            return Err(CompilerException::of(
                format!(
                    "Comprehension only returns 1 value, expected {}",
                    self.ret_count
                ),
                self.node,
            )
            .into());
        }
        let brace_type = self.get_brace()?;
        if brace_type == BraceType::Generator {
            if self.ret_count == 0 {
                warning::warn(
                    "Comprehension with no returns serves no purpose",
                    WarningType::Unused,
                    info,
                    self.node,
                )?;
                return Ok(BytecodeList::new());
            }
            let mut bytes = self.inner_convert(info, brace_type)?;
            bytes.add(Bytecode::Return(0.into()));
            let fn_info = FunctionInfo::new(
                self.node.line_info().clone(),
                info.generator_name(),
                true,
                GenericInfo::empty(),
                ArgumentInfo::empty(),
                self.return_type(info)?,
            );
            let fn_no =
                info.add_function(Function::new(self.node.line_info().clone(), fn_info, bytes));
            let mut true_bytes = BytecodeList::new();
            true_bytes.add(Bytecode::MakeFunction(fn_no.into()));
            true_bytes.add(Bytecode::CallTos(0.into()));
            Ok(true_bytes)
        } else {
            self.inner_convert(info, brace_type)
        }
    }
}

impl<'a> ComprehensionConverter<'a> {
    fn inner_convert(&self, info: &mut CompilerInfo, brace_type: BraceType) -> CompileBytes {
        debug_assert!(self.ret_count == 1 || self.ret_count == 0);
        let mut bytes = BytecodeList::new();
        if let Option::Some(create_code) = brace_type.create_code() {
            bytes.extend(
                TypeLoader::new(self.node.line_info().clone(), self.generic_type(info)?)
                    .convert(info)?,
            );
            bytes.add(create_code(0.into()));
        }
        bytes.add(Bytecode::LoadConst(
            info.builtins().iter_constant().clone().into(),
        ));
        bytes.extend(TestConverter::bytes(
            &self.node.get_looped()[0],
            info,
            self.ret_count,
        )?);
        bytes.add(Bytecode::CallTos(1.into()));
        let top_jump = Label::new();
        bytes.add_label(top_jump.clone());
        let for_jump = Label::new();
        bytes.add(Bytecode::ForIter(for_jump.clone().into(), 1.into()));
        if self.node.get_variables().len() > 1 {
            return Err(CompilerTodoError::of(
                "Cannot convert comprehension with more than one variable yet",
                self.node,
            )
            .into());
        }
        // Add the variable for the loop
        let variable = &self.node.get_variables()[0];
        info.add_stack_frame();
        if let VarLikeNode::Typed(typed_var) = variable {
            info.check_definition(typed_var.get_name(), typed_var)?;
            let true_type = self.var_type(info, typed_var)?;
            info.add_variable(
                typed_var.get_name().to_string(),
                true_type,
                false,
                typed_var.line_info().clone(),
            );
        }
        bytes.add(Bytecode::Store(
            info.var_index(variable.get_variable())?.into(),
        ));
        if !self.node.get_condition().is_empty() {
            bytes.extend(TestConverter::bytes(self.node.get_condition(), info, 1)?);
            bytes.add(Bytecode::JumpFalse(top_jump.clone().into()));
        }
        let while_jump = self.add_while_cond(info, &mut bytes)?;
        if brace_type.add_swap() {
            bytes.add(Bytecode::Swap2()); // The iterator object will be atop the list, swap it and back again
        }
        bytes.extend(TestConverter::bytes(
            self.node.get_builder()[0].get_argument(),
            info,
            1,
        )?);
        bytes.add(brace_type.add_code());
        if brace_type.add_swap() {
            bytes.add(Bytecode::Swap2());
        }
        bytes.add(Bytecode::Jump(top_jump.into()));
        bytes.add_label(for_jump);
        if let Option::Some(while_jump) = while_jump {
            bytes.add_label(while_jump);
        }
        if self.ret_count == 0 {
            debug_assert_ne!(brace_type, BraceType::Generator);
            bytes.add(Bytecode::PopTop());
        }
        info.remove_stack_frame();
        Ok(bytes)
    }

    fn get_brace(&self) -> CompileResult<BraceType> {
        BraceType::from_brace(self.node.get_brace()).ok_or_else(|| {
            CompilerInternalError::of(
                format!("Unexpected brace type {:?}", self.node.get_brace()),
                self.node,
            )
            .into()
        })
    }

    fn generic_type(&self, info: &mut CompilerInfo) -> CompileResult<TypeObject> {
        match &self.node.get_variables()[0] {
            VarLikeNode::Typed(typed_var) => {
                info.add_stack_frame();
                let name = typed_var.get_variable().get_name();
                if FORBIDDEN_NAMES.contains(&name) {
                    return Err(CompilerException::of(
                        format!("Illegal name for variable '{}'", name),
                        typed_var.get_variable(),
                    )
                    .into());
                }
                info.check_definition(name, typed_var)?;
                let true_type = self.var_type(info, typed_var)?;
                info.add_variable(
                    name.to_string(),
                    true_type,
                    false,
                    typed_var.line_info().clone(),
                );
                let result =
                    TestConverter::return_type(info, self.node.get_builder()[0].get_argument(), 1)?;
                info.remove_stack_frame();
                Ok(first(result))
            }
            VarLikeNode::Variable(_) => {
                TestConverter::return_type(info, self.node.get_builder()[0].get_argument(), 1)
                    .map(first)
            }
        }
    }

    fn var_type(
        &self,
        info: &mut CompilerInfo,
        typed_var: &TypedVariableNode,
    ) -> CompileResult<TypeObject> {
        let tv_type = typed_var.get_type();
        match tv_type {
            TypeLikeNode::Type(ty) => info.convert_type(ty),
            TypeLikeNode::Var(_) => {
                let ret_type = first(TestConverter::return_type(
                    info,
                    &self.node.get_looped()[0],
                    1,
                )?);
                let op_ret = first(ret_type.try_operator_return_type(
                    &self.node.get_looped()[0],
                    OpSpTypeNode::Iter,
                    info,
                )?);
                Ok(first(info.builtins().de_iterable(&op_ret)?))
            }
        }
    }

    fn add_while_cond(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
    ) -> CompileResult<Option<Label>> {
        if !self.node.get_while_cond().is_empty() {
            bytes.extend(TestConverter::bytes(self.node.get_while_cond(), info, 1)?);
            let inner_while_jump = Label::new();
            bytes.add(Bytecode::JumpTrue(inner_while_jump.clone().into()));
            bytes.add(Bytecode::PopTop());
            let while_jump = Label::new();
            bytes.add(Bytecode::Jump(while_jump.clone().into()));
            bytes.add_label(inner_while_jump);
            Ok(Some(while_jump))
        } else {
            Ok(None)
        }
    }
}

impl BraceType {
    fn from_brace(brace: char) -> Option<Self> {
        match brace {
            '[' => Some(BraceType::List),
            '{' => Some(BraceType::Set),
            '(' => Some(BraceType::Generator),
            _ => None,
        }
    }

    fn get_type<'a>(&self, info: &'a CompilerInfo) -> &'a TypeObject {
        let builtins = info.builtins();
        match self {
            BraceType::List => builtins.list_type(),
            BraceType::Set => builtins.set_type(),
            BraceType::Generator => builtins.iterable(),
        }
    }

    fn create_code(&self) -> Option<fn(ArgcBytecode) -> Bytecode> {
        match self {
            BraceType::List => Some(Bytecode::ListCreate),
            BraceType::Set => Some(Bytecode::SetCreate),
            BraceType::Generator => None,
        }
    }

    fn add_code(&self) -> Bytecode {
        match self {
            BraceType::List => Bytecode::ListAdd(),
            BraceType::Set => Bytecode::SetAdd(),
            BraceType::Generator => Bytecode::Yield(1.into()),
        }
    }

    fn add_swap(&self) -> bool {
        !matches!(self, &BraceType::Generator)
    }
}

test_convertible!(ComprehensionNode, ComprehensionConverter);
