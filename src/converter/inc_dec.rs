use derive_new::new;

use crate::parser::dotted::DottedVariableNode;
use crate::parser::inc_dec::{IncDecNode, IncDecType};
use crate::parser::index::IndexNode;
use crate::parser::name::NameNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::first;

use super::bytecode::Bytecode;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::dotted_var::DotConverter;
use super::error::{CompilerException, CompilerInternalError};
use super::index::IndexConverter;
use super::type_obj::TypeObject;
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct IncDecConverter<'a> {
    node: &'a IncDecNode,
}

impl<'a> ConverterBase for IncDecConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        match self.node.get_variable() {
            NameNode::Dotted(d) => self.convert_dot(d, info),
            NameNode::Index(i) => self.convert_index(i, info),
            NameNode::Variable(v) => self.convert_variable(v, info),
            _ => Err(CompilerInternalError::of(
                format!("Cannot {} this node", self.inc_name()),
                self.node,
            )
            .into()),
        }
    }
}

impl<'a> IncDecConverter<'a> {
    fn convert_variable(&self, variable: &VariableNode, info: &mut CompilerInfo) -> CompileBytes {
        let mut converter = variable.test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        if !info.builtins().int_type().is_superclass(&ret_type) {
            return Err(self.type_error(&ret_type).into());
        }
        let mut bytes = converter.convert(info)?;
        bytes.add(Bytecode::LoadConst(1.into()));
        bytes.add(self.inc_bytecode());
        if info.variable_is_immutable(variable.get_name()) {
            return Err(CompilerException::of(
                format!("Cannot {} non-mut variable", self.inc_name()),
                self.node,
            )
            .into());
        }
        bytes.add(Bytecode::Store(info.var_index(variable)?.into()));
        Ok(bytes)
    }

    fn convert_dot(&self, dot: &DottedVariableNode, info: &mut CompilerInfo) -> CompileBytes {
        match dot.get_last().get_post_dot() {
            NameNode::Variable(_) => self.convert_dot_var(dot, info),
            NameNode::Index(_) => self.convert_dot_index(dot, info),
            NameNode::Function(_) => Err(CompilerException::of(
                format!("Cannot {} result of function call", self.inc_name()),
                self.node,
            )
            .into()),
            NameNode::SpecialOp(_) => Err(CompilerException::of(
                format!("Cannot {} raw operator", self.inc_name()),
                self.node,
            )
            .into()),
            _ => Err(CompilerInternalError::of(
                format!("Unknown type for dotted {}", self.inc_name()),
                self.node,
            )
            .into()),
        }
    }

    fn convert_dot_var(&self, dot: &DottedVariableNode, info: &mut CompilerInfo) -> CompileBytes {
        let (mut converter, name) = DotConverter::except_last(dot, 1)?;
        let ret_type = first(converter.return_type(info)?);
        let attr_info = ret_type.try_attr_type(self.node, name, info)?;
        if info.builtins().int_type().is_superclass(&attr_info) {
            return Err(self.type_error(&attr_info).into());
        }
        if !ret_type.can_set_attr(name, info) {
            return Err(CompilerException::of(
                format!("Cannot {} non-mut attribute", self.inc_name()),
                self.node,
            )
            .into());
        }
        let mut bytes = converter.convert(info)?;
        bytes.add(Bytecode::DupTop());
        bytes.add(Bytecode::LoadDot(name.into()));
        bytes.add(Bytecode::LoadConst(1.into()));
        bytes.add(self.inc_bytecode());
        bytes.add(Bytecode::StoreAttr(name.into()));
        Ok(bytes)
    }

    fn convert_dot_index(&self, dot: &DottedVariableNode, info: &mut CompilerInfo) -> CompileBytes {
        let (converter, indices) = DotConverter::except_last_index(dot, 1)?;
        self.finish_indices(converter, indices, info)
    }

    fn convert_index(&self, var: &IndexNode, info: &mut CompilerInfo) -> CompileBytes {
        let converter = var.get_var().test_converter(1);
        self.finish_indices(converter, var.get_indices(), info)
    }

    fn finish_indices(
        &self,
        mut converter: impl ConverterTest,
        indices: &[TestNode],
        info: &mut CompilerInfo,
    ) -> CompileBytes {
        self.check_index(&converter.return_type(info)?[0], info)?;
        let mut bytes = IndexConverter::convert_duplicate(
            converter,
            indices,
            info,
            indices.len().try_into().unwrap(),
        )?;
        bytes.add(Bytecode::LoadConst(1.into()));
        bytes.add(self.inc_bytecode());
        bytes.add(Bytecode::StoreSubscript(
            ((indices.len() + 1) as u16).into(),
        ));
        Ok(bytes)
    }

    fn type_error(&self, ty: &TypeObject) -> CompilerException {
        CompilerException::of(
            format!(
                "Object of type {} cannot be {}ed",
                ty.name(),
                self.inc_name()
            ),
            self.node,
        )
    }

    fn check_index(&self, index_type: &TypeObject, info: &mut CompilerInfo) -> CompileResult<()> {
        if index_type
            .operator_info(OpSpTypeNode::GetAttr, info)?
            .is_none()
            || index_type
                .operator_info(OpSpTypeNode::SetAttr, info)?
                .is_none()
        {
            Err(self.type_error(index_type).into())
        } else {
            Ok(())
        }
    }

    fn inc_name(&self) -> &'static str {
        inc_name(self.node.get_sign())
    }

    fn inc_bytecode(&self) -> Bytecode {
        match self.node.get_sign() {
            IncDecType::Plus => Bytecode::Plus(),
            IncDecType::Minus => Bytecode::Minus(),
        }
    }
}

const fn inc_name(ty: IncDecType) -> &'static str {
    match ty {
        IncDecType::Plus => "increment",
        IncDecType::Minus => "decrement",
    }
}

base_convertible!(IncDecNode, IncDecConverter);
