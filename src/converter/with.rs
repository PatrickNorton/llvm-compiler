use derive_new::new;

use crate::parser::line_info::Lined;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::with_stmt::WithStatementNode;
use crate::util::first;

use super::base_converter::BaseConverter;
use super::bytecode::{Bytecode, Label};
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::error::{CompilerException, CompilerTodoError};
use super::CompileBytes;

#[derive(Debug, new)]
pub struct WithConverter<'a> {
    node: &'a WithStatementNode,
}

impl<'a> ConverterBase for WithConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        if self.node.get_managed().len() != 1 || self.node.get_vars().len() != 1 {
            return Err(CompilerTodoError::of(
                format!(
                    "With statements with more than one managed value (got {})",
                    self.node.get_managed().len()
                ),
                self.node,
            )
            .into());
        }
        info.add_stack_frame();
        let mut context_converter = self.node.get_managed()[0].test_converter(1);
        let variable = &self.node.get_vars()[0];
        let value_type = variable.get_type();
        let return_type = first(
            first(context_converter.return_type(info)?).try_operator_return_type(
                self.node,
                OpSpTypeNode::Enter,
                info,
            )?,
        );
        let true_type = match value_type {
            TypeLikeNode::Type(ty) => info.convert_type(ty)?,
            TypeLikeNode::Var(_) => return_type.clone(),
        };
        if !true_type.is_superclass(&return_type) {
            return Err(CompilerException::of(
                format!(
                    "Object in 'with' statement returns '{}' from operator enter(), \
                     attempted to assign it to variable of incompatible type '{}'",
                    return_type.name(),
                    true_type.name()
                ),
                self.node,
            )
            .into());
        }
        let mut bytes = context_converter.convert(info)?;
        info.check_definition(variable.get_variable().get_name(), variable)?;
        let var_index = info.add_variable(
            variable.get_variable().get_name().to_string(),
            true_type,
            false,
            variable.line_info().clone(),
        );
        bytes.add(Bytecode::DupTop());
        bytes.add(Bytecode::CallOp(OpSpTypeNode::Enter.into(), 0.into()));
        bytes.add(Bytecode::Store(var_index.into()));
        let try_jump = Label::new();
        bytes.add(Bytecode::EnterTry(try_jump.clone().into()));
        bytes.extend(BaseConverter::bytes(self.node.get_body(), info)?);
        bytes.add_label(try_jump);
        bytes.add(Bytecode::Finally());
        bytes.add(Bytecode::CallOp(OpSpTypeNode::Exit.into(), 0.into()));
        bytes.add(Bytecode::EndTry(0.into()));
        Ok(bytes)
    }
}

base_convertible!(WithStatementNode, WithConverter);
