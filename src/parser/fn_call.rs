use crate::parser::argument::ArgumentNode;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;

use super::name::NameNode;
use super::variable::VariableNode;

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

    pub fn get_caller(&self) -> &TestNode {
        &self.caller
    }

    pub fn get_parameters(&self) -> &[ArgumentNode] {
        &self.parameters
    }

    pub fn get_variable(&self) -> Option<&VariableNode> {
        (&*self.caller).try_into().ok()
    }
}

impl Lined for FunctionCallNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> TryFrom<&'a TestNode> for &'a FunctionCallNode {
    type Error = ();

    fn try_from(value: &'a TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Name(NameNode::Function(f)) => Ok(f),
            _ => Err(()),
        }
    }
}
