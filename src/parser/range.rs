use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::slice::SliceNode;
use crate::parser::test_node::TestNode;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct RangeLiteralNode {
    line_info: LineInfo,
    start: Box<TestNode>,
    stop: Box<TestNode>,
    step: Box<TestNode>,
}

impl RangeLiteralNode {
    pub fn new(line_info: LineInfo, start: TestNode, stop: TestNode, step: TestNode) -> Self {
        Self {
            line_info,
            start: Box::new(start),
            stop: Box::new(stop),
            step: Box::new(step),
        }
    }

    pub fn new_boxed(
        line_info: LineInfo,
        start: Box<TestNode>,
        stop: Box<TestNode>,
        step: Box<TestNode>,
    ) -> Self {
        Self {
            line_info,
            start,
            stop,
            step,
        }
    }

    pub fn get_start(&self) -> &TestNode {
        &self.start
    }

    pub fn get_stop(&self) -> &TestNode {
        &self.start
    }

    pub fn get_step(&self) -> &TestNode {
        &self.start
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<RangeLiteralNode> {
        SliceNode::parse(tokens).map(Into::into)
    }
}

impl Lined for RangeLiteralNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
