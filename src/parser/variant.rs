use derive_new::new;

use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;

#[derive(Debug, new)]
pub struct VariantCreationNode {
    line_info: LineInfo,
    union: TestNode,
    variant_no: u16,
    value: TestNode,
}

impl VariantCreationNode {
    pub fn get_union(&self) -> &TestNode {
        &self.union
    }

    pub fn get_variant_no(&self) -> u16 {
        self.variant_no
    }

    pub fn get_value(&self) -> &TestNode {
        &self.value
    }
}

impl Lined for VariantCreationNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
