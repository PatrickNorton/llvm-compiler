use std::collections::HashSet;

use crate::parser::base::IndependentNode;
use crate::parser::class_def::ClassDefinitionNode;
use crate::parser::context::ContextDefinitionNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::enum_def::EnumDefinitionNode;
use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::interface::InterfaceDefinitionNode;
use crate::parser::line_info::Lined;
use crate::parser::method::MethodDefinitionNode;
use crate::parser::name::NameNode;
use crate::parser::operator_def::OperatorDefinitionNode;
use crate::parser::property::PropertyDefinitionNode;
use crate::parser::union_def::UnionDefinitionNode;

#[derive(Debug)]
pub enum DefinitionNode {
    BaseClass(BaseClassNode),
    Context(ContextDefinitionNode),
    Function(FunctionDefinitionNode),
    Method(MethodDefinitionNode),
    Operator(OperatorDefinitionNode),
    Property(PropertyDefinitionNode),
}

#[derive(Debug)]
pub enum BaseClassNode {
    Class(ClassDefinitionNode),
    Enum(EnumDefinitionNode),
    Interface(InterfaceDefinitionNode),
    Union(UnionDefinitionNode),
}

impl DefinitionNode {
    pub fn get_annotations(&self) -> &Vec<NameNode> {
        match self {
            DefinitionNode::BaseClass(b) => b.get_annotations(),
            DefinitionNode::Context(c) => c.get_annotations(),
            DefinitionNode::Function(f) => f.get_annotations(),
            DefinitionNode::Method(m) => m.get_annotations(),
            DefinitionNode::Operator(o) => o.get_annotations(),
            DefinitionNode::Property(p) => p.get_annotations(),
        }
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        match self {
            DefinitionNode::BaseClass(b) => b.add_annotations(annotations),
            DefinitionNode::Context(c) => c.add_annotations(annotations),
            DefinitionNode::Function(f) => f.add_annotations(annotations),
            DefinitionNode::Method(m) => m.add_annotations(annotations),
            DefinitionNode::Operator(o) => o.add_annotations(annotations),
            DefinitionNode::Property(p) => p.add_annotations(annotations),
        }
    }
}

impl BaseClassNode {
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        match self {
            BaseClassNode::Class(c) => c.valid_descriptors(),
            BaseClassNode::Enum(e) => e.valid_descriptors(),
            BaseClassNode::Interface(i) => i.valid_descriptors(),
            BaseClassNode::Union(u) => u.valid_descriptors(),
        }
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        match self {
            BaseClassNode::Class(c) => c.add_descriptors(descriptors),
            BaseClassNode::Enum(e) => e.add_descriptors(descriptors),
            BaseClassNode::Interface(i) => i.add_descriptors(descriptors),
            BaseClassNode::Union(u) => u.add_descriptors(descriptors),
        }
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        match self {
            BaseClassNode::Class(c) => c.get_annotations(),
            BaseClassNode::Enum(e) => e.get_annotations(),
            BaseClassNode::Interface(i) => i.get_annotations(),
            BaseClassNode::Union(u) => u.get_annotations(),
        }
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        match self {
            BaseClassNode::Class(c) => c.add_annotations(annotations),
            BaseClassNode::Enum(e) => e.add_annotations(annotations),
            BaseClassNode::Interface(i) => i.add_annotations(annotations),
            BaseClassNode::Union(u) => u.add_annotations(annotations),
        }
    }
}

impl Lined for BaseClassNode {
    fn line_info(&self) -> &super::line_info::LineInfo {
        match self {
            BaseClassNode::Class(c) => c.line_info(),
            BaseClassNode::Enum(e) => e.line_info(),
            BaseClassNode::Interface(i) => i.line_info(),
            BaseClassNode::Union(u) => u.line_info(),
        }
    }
}

impl Lined for DefinitionNode {
    fn line_info(&self) -> &super::line_info::LineInfo {
        match self {
            DefinitionNode::BaseClass(b) => b.line_info(),
            DefinitionNode::Context(c) => c.line_info(),
            DefinitionNode::Function(f) => f.line_info(),
            DefinitionNode::Method(m) => m.line_info(),
            DefinitionNode::Operator(o) => o.line_info(),
            DefinitionNode::Property(p) => p.line_info(),
        }
    }
}

impl From<BaseClassNode> for IndependentNode {
    fn from(value: BaseClassNode) -> Self {
        match value {
            BaseClassNode::Class(c) => IndependentNode::ClassDef(c),
            BaseClassNode::Enum(e) => IndependentNode::Enum(e),
            BaseClassNode::Interface(i) => IndependentNode::Interface(i),
            BaseClassNode::Union(u) => IndependentNode::Union(u),
        }
    }
}

impl TryFrom<IndependentNode> for BaseClassNode {
    type Error = IndependentNode;

    fn try_from(value: IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::ClassDef(c) => Ok(BaseClassNode::Class(c)),
            IndependentNode::Enum(e) => Ok(BaseClassNode::Enum(e)),
            IndependentNode::Interface(i) => Ok(BaseClassNode::Interface(i)),
            IndependentNode::Union(u) => Ok(BaseClassNode::Union(u)),
            value => Err(value),
        }
    }
}

impl From<DefinitionNode> for IndependentNode {
    fn from(value: DefinitionNode) -> Self {
        match value {
            DefinitionNode::BaseClass(b) => b.into(),
            DefinitionNode::Context(c) => IndependentNode::Context(c),
            DefinitionNode::Function(f) => IndependentNode::FunctionDef(f),
            DefinitionNode::Method(m) => IndependentNode::Method(m),
            DefinitionNode::Operator(o) => IndependentNode::OpDef(o),
            DefinitionNode::Property(p) => IndependentNode::Property(p),
        }
    }
}

impl TryFrom<IndependentNode> for DefinitionNode {
    type Error = IndependentNode;

    fn try_from(value: IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::Context(c) => Ok(DefinitionNode::Context(c)),
            IndependentNode::FunctionDef(f) => Ok(DefinitionNode::Function(f)),
            IndependentNode::Method(m) => Ok(DefinitionNode::Method(m)),
            IndependentNode::OpDef(o) => Ok(DefinitionNode::Operator(o)),
            IndependentNode::Property(p) => Ok(DefinitionNode::Property(p)),
            value => value.try_into().map(DefinitionNode::BaseClass),
        }
    }
}
