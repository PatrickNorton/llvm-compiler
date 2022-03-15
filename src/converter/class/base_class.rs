use crate::parser::class_def::{ClassBodyNode, ClassDefinitionNode};
use crate::parser::enum_def::EnumDefinitionNode;
use crate::parser::interface::InterfaceDefinitionNode;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::type_node::TypeNode;
use crate::parser::union_def::UnionDefinitionNode;

pub(super) trait BaseClass: Lined {
    fn get_name(&self) -> &TypeNode;

    fn get_annotations(&self) -> &[NameNode];

    fn get_superclasses(&self) -> &[TypeNode];
}

pub(super) trait BodiedClass: BaseClass {
    fn get_body(&self) -> &ClassBodyNode;
}

impl BaseClass for ClassDefinitionNode {
    fn get_name(&self) -> &TypeNode {
        self.get_name()
    }

    fn get_annotations(&self) -> &[NameNode] {
        self.get_annotations()
    }

    fn get_superclasses(&self) -> &[TypeNode] {
        self.get_superclasses()
    }
}

impl BodiedClass for ClassDefinitionNode {
    fn get_body(&self) -> &ClassBodyNode {
        self.get_body()
    }
}

impl BaseClass for EnumDefinitionNode {
    fn get_name(&self) -> &TypeNode {
        self.get_name()
    }

    fn get_annotations(&self) -> &[NameNode] {
        self.get_annotations()
    }

    fn get_superclasses(&self) -> &[TypeNode] {
        self.get_superclasses()
    }
}

impl BodiedClass for EnumDefinitionNode {
    fn get_body(&self) -> &ClassBodyNode {
        self.get_body()
    }
}

impl BaseClass for InterfaceDefinitionNode {
    fn get_name(&self) -> &TypeNode {
        self.get_name()
    }

    fn get_annotations(&self) -> &[NameNode] {
        self.get_annotations()
    }

    fn get_superclasses(&self) -> &[TypeNode] {
        self.get_superclasses()
    }
}

impl BaseClass for UnionDefinitionNode {
    fn get_name(&self) -> &TypeNode {
        self.get_name()
    }

    fn get_annotations(&self) -> &[NameNode] {
        self.get_annotations()
    }

    fn get_superclasses(&self) -> &[TypeNode] {
        self.get_superclasses()
    }
}

impl BodiedClass for UnionDefinitionNode {
    fn get_body(&self) -> &ClassBodyNode {
        self.get_body()
    }
}
