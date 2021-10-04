use crate::parser::line_info::LineInfo;
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
            line_info: LineInfo::empty(), // FIXME
            if_true,
            statement,
            if_false,
        }
    }
}
