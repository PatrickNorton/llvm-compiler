use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;

#[derive(Debug)]
pub struct IndexNode {
    line_info: LineInfo,
    var: Box<TestNode>,
    indices: Vec<TestNode>,
}

impl IndexNode {
    pub fn new(var: TestNode, indices: Vec<TestNode>) -> IndexNode {
        IndexNode::new_lined(var.line_info().clone(), Box::new(var), indices)
    }

    pub fn new_lined(line_info: LineInfo, var: Box<TestNode>, indices: Vec<TestNode>) -> IndexNode {
        IndexNode {
            line_info,
            var,
            indices,
        }
    }
}

impl Lined for IndexNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
