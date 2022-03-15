use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::range::RangeLiteralNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct SliceNode {
    line_info: LineInfo,
    start: Box<TestNode>,
    stop: Box<TestNode>,
    step: Box<TestNode>,
}

impl SliceNode {
    pub fn new(line_info: LineInfo, start: TestNode, stop: TestNode, step: TestNode) -> Self {
        Self {
            line_info,
            start: Box::new(start),
            stop: Box::new(stop),
            step: Box::new(step),
        }
    }

    pub fn get_start(&self) -> &TestNode {
        &self.start
    }

    pub fn get_stop(&self) -> &TestNode {
        &self.stop
    }

    pub fn get_step(&self) -> &TestNode {
        &self.step
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<SliceNode> {
        assert!(tokens.token_equals("[")?);
        let line_info = tokens.next_tok(true)?.deconstruct().0;
        let start = if let TokenType::Colon = tokens.token_type()? {
            TestNode::empty()
        } else {
            TestNode::parse_newline(tokens, true)?
        };
        if tokens.next_if(|t| t.equals("]"))?.is_some() {
            return Ok(SliceNode::new(
                line_info,
                start,
                TestNode::empty(),
                TestNode::empty(),
            ));
        }
        let end = Self::slice_test(tokens)?;
        if tokens.next_if(|t| t.equals("]"))?.is_some() {
            return Ok(SliceNode::new(line_info, start, end, TestNode::empty()));
        }
        let step = Self::slice_test(tokens)?;
        tokens.expect("]", false)?;
        Ok(SliceNode::new(line_info, start, end, step))
    }

    fn slice_test(tokens: &mut TokenList) -> ParseResult<TestNode> {
        if tokens
            .next_if(|t| matches!(t.token_type(), TokenType::Colon))?
            .is_some()
        {
            if tokens.token_equals(":")? || tokens.token_equals("]")? {
                Ok(TestNode::empty())
            } else {
                TestNode::parse_newline(tokens, true)
            }
        } else {
            Err(tokens.error_expected(":"))
        }
    }
}

impl From<SliceNode> for RangeLiteralNode {
    fn from(x: SliceNode) -> Self {
        Self::new_boxed(x.line_info, x.start, x.stop, x.step)
    }
}

impl Lined for SliceNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
