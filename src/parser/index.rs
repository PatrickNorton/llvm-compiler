use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;

use super::name::NameNode;

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

    pub fn get_var(&self) -> &TestNode {
        &self.var
    }

    pub fn get_indices(&self) -> &[TestNode] {
        &self.indices
    }
}

impl Lined for IndexNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> TryFrom<&'a NameNode> for &'a IndexNode {
    type Error = ();

    fn try_from(value: &'a NameNode) -> Result<Self, Self::Error> {
        match value {
            NameNode::Index(x) => Ok(x),
            _ => Err(()),
        }
    }
}
