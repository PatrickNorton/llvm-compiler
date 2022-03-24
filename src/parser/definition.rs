use std::borrow::Cow;
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
use crate::parser::type_node::TypeNode;
use crate::parser::union_def::UnionDefinitionNode;

use super::line_info::LineInfo;

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

#[derive(Debug, Clone, Copy)]
pub enum DefinitionRef<'a> {
    BaseClass(BaseClassRef<'a>),
    Context(&'a ContextDefinitionNode),
    Function(&'a FunctionDefinitionNode),
    Method(&'a MethodDefinitionNode),
    Operator(&'a OperatorDefinitionNode),
    Property(&'a PropertyDefinitionNode),
}

#[derive(Debug, Clone, Copy)]
pub enum BaseClassRef<'a> {
    Class(&'a ClassDefinitionNode),
    Enum(&'a EnumDefinitionNode),
    Interface(&'a InterfaceDefinitionNode),
    Union(&'a UnionDefinitionNode),
}

macro_rules! definition_each {
    ($self:ident: $name:ident => $value:expr) => {
        match $self {
            Self::BaseClass($name) => $value,
            Self::Context($name) => $value,
            Self::Function($name) => $value,
            Self::Method($name) => $value,
            Self::Operator($name) => $value,
            Self::Property($name) => $value,
        }
    };
}

macro_rules! base_class_each {
    ($self:ident: $name:ident => $value:expr) => {
        match $self {
            Self::Class($name) => $value,
            Self::Enum($name) => $value,
            Self::Interface($name) => $value,
            Self::Union($name) => $value,
        }
    };
}

impl DefinitionNode {
    pub fn get_annotations(&self) -> &Vec<NameNode> {
        definition_each!(self: x => x.get_annotations())
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        definition_each!(self: x => x.add_annotations(annotations))
    }

    pub fn get_decorators(&self) -> &Vec<NameNode> {
        definition_each!(self: x => x.get_decorators())
    }

    pub fn add_decorators(&mut self, decorators: Vec<NameNode>) {
        definition_each!(self: x => x.add_decorators(decorators))
    }
}

impl BaseClassNode {
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        base_class_each!(self: x => x.valid_descriptors())
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        base_class_each!(self: x => x.add_descriptors(descriptors))
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        base_class_each!(self: x => x.get_annotations())
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        base_class_each!(self: x => x.add_annotations(annotations))
    }

    pub fn get_decorators(&self) -> &Vec<NameNode> {
        base_class_each!(self: x => x.get_decorators())
    }

    pub fn add_decorators(&mut self, decorators: Vec<NameNode>) {
        base_class_each!(self: x => x.add_decorators(decorators))
    }
}

impl<'a> DefinitionRef<'a> {
    pub fn str_name(self) -> Cow<'a, str> {
        match self {
            DefinitionRef::BaseClass(b) => b.get_name().str_name().into(),
            DefinitionRef::Context(c) => c.get_name().get_name().into(),
            DefinitionRef::Function(f) => f.get_name().get_name().into(),
            DefinitionRef::Method(m) => m.get_name().get_name().into(),
            DefinitionRef::Operator(o) => o.get_op_code().get_operator().to_string().into(),
            DefinitionRef::Property(p) => p.get_name().get_name().into(),
        }
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        definition_each!(self: x => x.get_annotations())
    }
}

impl<'a> BaseClassRef<'a> {
    pub fn get_name(self) -> &'a TypeNode {
        base_class_each!(self: x => x.get_name())
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        base_class_each!(self: x => x.get_annotations())
    }
}

impl Lined for BaseClassNode {
    fn line_info(&self) -> &LineInfo {
        base_class_each!(self: x => x.line_info())
    }
}

impl Lined for DefinitionNode {
    fn line_info(&self) -> &LineInfo {
        definition_each!(self: x => x.line_info())
    }
}

impl Lined for DefinitionRef<'_> {
    fn line_info(&self) -> &LineInfo {
        definition_each!(self: x => x.line_info())
    }
}

impl Lined for BaseClassRef<'_> {
    fn line_info(&self) -> &LineInfo {
        base_class_each!(self: x => x.line_info())
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

impl<'a> TryFrom<&'a IndependentNode> for DefinitionRef<'a> {
    type Error = ();

    fn try_from(value: &'a IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::Context(c) => Ok(DefinitionRef::Context(c)),
            IndependentNode::FunctionDef(f) => Ok(DefinitionRef::Function(f)),
            IndependentNode::Method(m) => Ok(DefinitionRef::Method(m)),
            IndependentNode::OpDef(o) => Ok(DefinitionRef::Operator(o)),
            IndependentNode::Property(p) => Ok(DefinitionRef::Property(p)),
            value => value.try_into().map(DefinitionRef::BaseClass),
        }
    }
}

impl<'a> TryFrom<&'a IndependentNode> for BaseClassRef<'a> {
    type Error = ();

    fn try_from(value: &'a IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::ClassDef(c) => Ok(BaseClassRef::Class(c)),
            IndependentNode::Enum(e) => Ok(BaseClassRef::Enum(e)),
            IndependentNode::Interface(i) => Ok(BaseClassRef::Interface(i)),
            IndependentNode::Union(u) => Ok(BaseClassRef::Union(u)),
            _ => Err(()),
        }
    }
}
