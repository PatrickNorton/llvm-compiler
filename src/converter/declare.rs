use derive_new::new;

use crate::parser::annotation::AnnotatableRef;
use crate::parser::declaration::DeclarationNode;
use crate::parser::line_info::Lined;
use crate::parser::type_node::TypeLikeNode;

use super::annotation::{impl_annotatable, AnnotatableConverter};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::base_convertible;
use super::diverge::DivergingInfo;
use super::error::CompilerException;
use super::mutable::MutableType;
use super::{CompileBytes, CompileResult};

#[derive(Debug, new)]
pub struct DeclarationConverter<'a> {
    node: &'a DeclarationNode,
}

impl<'a> AnnotatableConverter<'a> for DeclarationConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::Declaration(self.node)
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let node_type = match self.node.get_type() {
            TypeLikeNode::Type(ty) => ty,
            TypeLikeNode::Var(_) => {
                return Err(
                    CompilerException::of("'var' not allowed in declarations", self.node).into(),
                )
            }
        };
        let name = self.node.get_name().get_name();
        info.check_definition(name, self.node)?;
        let ty = info.convert_type(node_type)?;
        let mutability = MutableType::from_descriptors(self.node.get_descriptors());
        let true_type = if mutability.is_const_type() {
            ty.make_const()
        } else {
            ty.make_mut()
        };
        let is_const = mutability.is_const_ref();
        info.add_variable(
            name.to_string(),
            true_type,
            is_const,
            self.node.line_info().clone(),
        );
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

impl_annotatable!(DeclarationConverter<'a>);

base_convertible!(DeclarationNode, DeclarationConverter);
