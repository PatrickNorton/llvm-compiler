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

const DESCRIPTOR_PAIRS: &[(DescriptorNode, MutableType)] = &[
    (DescriptorNode::Mut, MutableType::Mut),
    (DescriptorNode::Final, MutableType::Final),
    (DescriptorNode::Mref, MutableType::Mref),
];

impl MutableType {
    pub fn is_const_type(self) -> bool {
        matches!(self, Self::Mref | Self::Standard)
    }

    pub fn is_const_ref(self) -> bool {
        matches!(self, Self::Standard | Self::Final)
    }

    pub fn from_descriptor(descriptor: DescriptorNode) -> Self {
        match descriptor {
            DescriptorNode::Mut => Self::Mut,
            DescriptorNode::Final => Self::Final,
            DescriptorNode::Mref => Self::Mref,
            _ => panic!(
                "Descriptor does not correspond to a mutability: {}",
                descriptor.name()
            ),
        }
    }

    pub fn from_descriptors(descriptors: &HashSet<DescriptorNode>) -> Self {
        DESCRIPTOR_PAIRS
            .iter()
            .find(|x| descriptors.contains(&x.0))
            .map(|x| x.1)
            .unwrap_or(MutableType::Standard)
    }

    pub fn from_nullable(descriptor: Option<DescriptorNode>) -> Self {
        descriptor.map_or_else(|| Self::Standard, Self::from_descriptor)
    }
}
