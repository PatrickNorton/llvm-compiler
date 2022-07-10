use std::collections::HashSet;

use crate::parser::descriptor::DescriptorNode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MutableType {
    Standard,
    Mut,
    Final,
    Mref,
    MutMethod,
}

impl MutableType {
    pub fn is_const_type(self) -> bool {
        matches!(self, Self::Mref | Self::Standard | Self::MutMethod)
    }

    pub fn is_const_ref(self) -> bool {
        matches!(self, Self::Standard | Self::Final | Self::MutMethod)
    }

    pub fn from_descriptor(descriptor: DescriptorNode) -> Option<Self> {
        match descriptor {
            DescriptorNode::Mut => Some(Self::Mut),
            DescriptorNode::Final => Some(Self::Final),
            DescriptorNode::Mref => Some(Self::Mref),
            _ => None,
        }
    }

    pub fn from_descriptors(descriptors: &HashSet<DescriptorNode>) -> Self {
        descriptors
            .iter()
            .find_map(|&x| Self::from_descriptor(x))
            .unwrap_or(Self::Standard)
    }

    pub fn from_nullable(descriptor: Option<DescriptorNode>) -> Self {
        descriptor.map_or_else(
            || MutableType::Standard,
            |x| {
                Self::from_descriptor(x).unwrap_or_else(|| {
                    panic!(
                        "Descriptor does not correspond to a mutability: {}",
                        x.name()
                    )
                })
            },
        )
    }
}
