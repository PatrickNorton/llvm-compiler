use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;

#[derive(Debug)]
pub struct TernaryNode {
    line_info: LineInfo,
    if_true: TestNode,
    statement: TestNode,
    if_false: TestNode,
}

impl TernaryNode {
    pub fn new(if_true: TestNode, statement: TestNode, if_false: TestNode) -> Self {
        TernaryNode {
            line_info: if_true.line_info().clone(),
            if_true,
            statement,
            if_false,
        }
    }

    pub fn get_if_true(&self) -> &TestNode {
        &self.if_true
    }

    pub fn get_statement(&self) -> &TestNode {
        &self.statement
    }

    pub fn get_if_false(&self) -> &TestNode {
        &self.if_false
    }
}

impl Lined for TernaryNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
