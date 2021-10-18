use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct TestListNode {
    line_info: LineInfo,
    values: Vec<(String, TestNode)>,
}

impl TestListNode {
    pub fn empty() -> TestListNode {
        Self::new(LineInfo::empty(), vec![])
    }

    pub fn new(line_info: LineInfo, values: Vec<(String, TestNode)>) -> TestListNode {
        Self { line_info, values }
    }

    pub fn parse_post_if(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<(TestListNode, Option<TestNode>)> {
        if !ignore_newlines && matches!(tokens.token_type()?, TokenType::Newline) {
            return Ok((TestListNode::empty(), Option::None));
        }
        let mut values = Vec::new();
        let mut post_if = Option::None;
        while TestNode::next_is_test(tokens)?
            || matches!(tokens.token_type()?, TokenType::Keyword(Keyword::If))
        {
            if parse_if_matches!(tokens, ignore_newlines, TokenType::Keyword(Keyword::If))?
                .is_some()
            {
                post_if = Option::Some(TestNode::parse_newline(tokens, ignore_newlines)?);
                break;
            }
            let vararg = if tokens.token_eq_either("*", "**")? {
                tokens.next_tok(ignore_newlines)?.into_sequence()
            } else {
                String::new()
            };
            let (next, cond) = TestNode::parse_maybe_post_if(tokens, ignore_newlines)?;
            values.push((vararg, next));
            if let Option::Some(cond) = cond {
                post_if = Option::Some(cond);
                break;
            }
            if let TokenType::Comma = tokens.token_type()? {
                tokens.next_tok(ignore_newlines)?;
            } else {
                break;
            }
        }
        let node = TestListNode::new(LineInfo::empty(), values);
        Ok((node, post_if))
    }
}

impl Lined for TestListNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
