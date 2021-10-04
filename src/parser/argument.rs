use crate::parser::line_info::LineInfo;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct ArgumentNode {
    line_info: LineInfo,
    variable: VariableNode,
    vararg: String,
    argument: TestNode,
}

impl ArgumentNode {
    pub fn from_test_node(node: TestNode) -> ArgumentNode {
        todo!()
    }

    pub fn from_test_nodes(nodes: Vec<TestNode>) -> Vec<ArgumentNode> {
        nodes
            .into_iter()
            .map(ArgumentNode::from_test_node)
            .collect()
    }
}
