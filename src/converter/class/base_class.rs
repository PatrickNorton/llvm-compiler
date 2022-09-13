use crate::parser::class_def::{ClassBodyNode, ClassDefinitionNode};
use crate::parser::definition::BaseClassRef;
use crate::parser::enum_def::EnumDefinitionNode;
use crate::parser::interface::InterfaceDefinitionNode;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::type_node::TypeNode;
use crate::parser::union_def::UnionDefinitionNode;

pub trait BaseClass: Lined {
    fn get_name(&self) -> &TypeNode;

    fn get_annotations(&self) -> &[NameNode];

    fn get_superclasses(&self) -> &[TypeNode];

    fn str_name(&self) -> &str {
        self.get_name().str_name()
    }
}

pub trait BodiedClass: BaseClass {
    fn get_body(&self) -> &ClassBodyNode;
}

macro_rules! base_class_each {
    ($self:ident: $x:ident => $val:expr) => {
        match $self {
            BaseClassRef::Class($x) => $val,
            BaseClassRef::Enum($x) => $val,
            BaseClassRef::Interface($x) => $val,
            BaseClassRef::Union($x) => $val,
        }
    };
}

impl<'a> BaseClass for BaseClassRef<'a> {
    fn get_name(&self) -> &TypeNode {
        base_class_each!(self: x => x.get_name())
    }

    fn get_annotations(&self) -> &[NameNode] {
        base_class_each!(self: x => x.get_annotations())
    }

    fn get_superclasses(&self) -> &[TypeNode] {
        base_class_each!(self: x => x.get_superclasses())
    }
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
