use std::borrow::Cow;

use derive_new::new;

use crate::parser::derived_op::DerivedOperatorNode;
use crate::parser::line_info::LineInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::access_handler::AccessLevel;
use super::builtins::{FALSE, TRUE};
use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::{base_convertible, ConverterBase};
use super::diverge::DivergingInfo;
use super::error::{CompilerException, CompilerInternalError};
use super::type_obj::UserType;
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct DerivedOperatorConverter<'a> {
    node: &'a DerivedOperatorNode,
}

impl<'a> ConverterBase for DerivedOperatorConverter<'a> {
    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        match self.node.get_operator() {
            OpSpTypeNode::Equals => self.convert_equals(info),
            OpSpTypeNode::Compare => self.convert_compare(info),
            OpSpTypeNode::Hash => self.convert_hash(info),
            OpSpTypeNode::Repr => self.convert_repr(info),
            _ => Err(
                CompilerInternalError::of("Attempted to convert invalid operator", self.node)
                    .into(),
            ),
        }
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut diverging_info = DivergingInfo::new();
        diverging_info.known_return();
        self.convert(info).map(|x| (x, diverging_info))
    }
}

impl<'a> DerivedOperatorConverter<'a> {
    /// Adds code for derived equality tests.
    ///
    /// # Approximate generated code
    /// ```text
    /// public operator == (Type other) {
    ///     # Repeat the following for each field...
    ///     if self.field != other.field {
    ///         return false
    ///     }
    ///     # End repeat
    ///     return true
    /// }
    /// ```
    fn convert_equals(&self, info: &mut CompilerInfo) -> CompileBytes {
        let ty = get_self_type(info)?;
        let mut bytes = BytecodeList::new();
        bytes.add(Bytecode::LoadValue(0.into())); // self
        bytes.add(Bytecode::GetType());
        bytes.add(Bytecode::LoadValue(2.into())); // other
        bytes.add(Bytecode::GetType());
        bytes.add(Bytecode::Equal());
        for field in ty.get_fields() {
            load_fields(&mut bytes, field);
            bytes.add(Bytecode::Equal());
            bytes.extend(post_jump_bytes());
        }
        bytes.add(Bytecode::LoadConst(TRUE.into()));
        bytes.add(Bytecode::Return(1.into()));
        Ok(bytes)
    }

    /// Adds code for derived comparison tests.
    ///
    /// # Approximate generated code
    /// ```text
    /// public operator <=> (Type other) {
    ///     # Repeat the following for each field...
    ///     var cmp = self.field <=> other.field
    ///     if cmp != 0 {
    ///         return cmp
    ///     }
    ///     # End repeat
    ///     return 0
    /// }
    /// ```
    fn convert_compare(&self, info: &mut CompilerInfo) -> CompileBytes {
        // FIXME: Get consistent return type for operator <=> (this uses int)
        let ty = get_self_type(info)?;
        let mut bytes = BytecodeList::new();
        for field in ty.get_fields() {
            let field_type = ty.attr_ty_access(field, AccessLevel::Private).unwrap();
            if field_type
                .op_info_access(OpSpTypeNode::Compare, AccessLevel::Private, info.builtins())
                .is_none()
            {
                return Err(CompilerException::of(
                    format!(
                        "Cannot derive <=> for type '{}': \
                         Field {} has type '{}', which does not implement a <=> operator",
                        ty.name(),
                        field,
                        field_type.name()
                    ),
                    self.node,
                )
                .into());
            }
            load_fields(&mut bytes, field);
            bytes.add(Bytecode::Compare());
            bytes.add(Bytecode::DupTop());
            bytes.add(Bytecode::LoadConst(0u32.into()));
            bytes.add(Bytecode::Equal());
            let jump_label = Label::new();
            bytes.add(Bytecode::JumpTrue(jump_label.clone().into()));
            bytes.add(Bytecode::Return(1.into()));
            bytes.add_label(jump_label);
        }
        bytes.add(Bytecode::LoadConst(0u32.into()));
        bytes.add(Bytecode::Return(1.into()));
        Ok(bytes)
    }

    /// Adds code for derived hash values.
    ///
    /// This code generates a tuple with each of the fields as a value and then
    /// calls `operator hash` on it.
    fn convert_hash(&self, info: &mut CompilerInfo) -> CompileBytes {
        let ty = get_self_type(info)?;
        let mut bytes = BytecodeList::new();
        let mut field_count = 0;
        for field in ty.get_fields() {
            let field_type = ty.attr_ty_access(field, AccessLevel::Private).unwrap();
            if field_type
                .op_info_access(OpSpTypeNode::Hash, AccessLevel::Private, info.builtins())
                .is_none()
            {
                return Err(CompilerException::of(
                    format!(
                        "Cannot derive hash for type '{}': \
                         Field {} has type '{}', which does not implement a hash operator",
                        ty.name(),
                        field,
                        field_type.name()
                    ),
                    self.node,
                )
                .into());
            }
            bytes.add(Bytecode::LoadValue(0.into()));
            bytes.add(Bytecode::LoadDot(field.into()));
            field_count += 1;
        }
        bytes.add(Bytecode::PackTuple((field_count as u16).into()));
        bytes.add(Bytecode::CallOp(OpSpTypeNode::Hash.into(), 0.into()));
        bytes.add(Bytecode::Return(1.into()));
        Ok(bytes)
    }

    /// Adds code for derived repr values.
    ///
    /// # Generated representation
    /// ```text
    /// [Type name]{[field] = [repr(self.field)], ...}
    /// ```
    fn convert_repr(&self, info: &mut CompilerInfo) -> CompileBytes {
        let ty = get_self_type(info)?;
        let mut bytes = BytecodeList::new();
        if ty.is_final() && ty.get_generic_info().is_empty() {
            bytes.add(Bytecode::LoadConst(format!("{}{{", ty.base_name()).into()));
        } else {
            bytes.add(Bytecode::LoadValue(0.into()));
            bytes.add(Bytecode::GetType());
            bytes.add(Bytecode::CallOp(OpSpTypeNode::Str.into(), 0.into()));
        }
        let mut first = true;
        for field in ty.get_fields() {
            let field_name = if first {
                format!("{} = ", field)
            } else {
                format!(", {} = ", field)
            };
            first = false;
            bytes.add(Bytecode::LoadConst(field_name.into()));
            bytes.add(Bytecode::Plus());
            bytes.add(Bytecode::LoadValue(0.into()));
            bytes.add(Bytecode::LoadDot(field.into()));
            bytes.add(Bytecode::CallOp(OpSpTypeNode::Repr.into(), 0.into()));
            bytes.add(Bytecode::Plus());
        }
        bytes.add(Bytecode::LoadConst("}".into()));
        bytes.add(Bytecode::Plus());
        bytes.add(Bytecode::Return(1.into()));
        Ok(bytes)
    }
}

fn get_self_type(info: &mut CompilerInfo) -> CompileResult<UserType> {
    let ty = info.get_type("self").ok_or_else(|| {
        CompilerInternalError::of(
            "Operator derivation should always take place in a method \
             (type of 'self' should always be defined)",
            LineInfo::empty(),
        )
    })?;
    UserType::try_from(Cow::into_owned(ty)).map_err(|t| {
        CompilerInternalError::of(
            format!(
                "Derivation should always happen on a user-defined type (type of 'self' was {})",
                t.name()
            ),
            LineInfo::empty(),
        )
        .into()
    })
}

fn load_fields(bytes: &mut BytecodeList, field: &str) {
    bytes.add(Bytecode::LoadValue(0.into())); // self
    bytes.add(Bytecode::LoadDot(field.into()));
    bytes.add(Bytecode::LoadValue(2.into())); // other
    bytes.add(Bytecode::LoadDot(field.into()));
}

fn post_jump_bytes() -> BytecodeList {
    let mut bytes = BytecodeList::with_capacity(4);
    let label = Label::new();
    bytes.add(Bytecode::JumpTrue(label.clone().into()));
    bytes.add(Bytecode::LoadConst(FALSE.into()));
    bytes.add(Bytecode::Return(1.into()));
    bytes.add_label(label);
    bytes
}

base_convertible!(DerivedOperatorNode, DerivedOperatorConverter);
