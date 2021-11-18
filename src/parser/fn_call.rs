use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;

#[derive(Debug)]
pub struct FunctionCallNode {
    line_info: LineInfo,
    caller: Box<TestNode>,
    parameters: Vec<ArgumentNode>,
}

impl FunctionCallNode {
    pub fn new(caller: TestNode, parameters: Vec<ArgumentNode>) -> Self {
        Self::new_lined(caller.line_info().clone(), Box::new(caller), parameters)
    }

    pub fn new_lined(
        line_info: LineInfo,
        caller: Box<TestNode>,
        parameters: Vec<ArgumentNode>,
    ) -> Self {
        Self {
            line_info,
            caller,
            parameters,
        }
    }
}

impl Lined for FunctionCallNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
