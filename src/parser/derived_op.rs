use derive_new::new;

use super::line_info::{LineInfo, Lined};
use super::operator_sp::OpSpTypeNode;

#[derive(Debug, new)]
pub struct DerivedOperatorNode {
    line_info: LineInfo,
    operator: OpSpTypeNode,
}

impl DerivedOperatorNode {
    pub fn get_operator(&self) -> OpSpTypeNode {
        self.operator
    }
}

impl Lined for DerivedOperatorNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
