use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct YieldStatementNode {
    line_info: LineInfo,
    is_from: bool,
    returned: TestListNode,
    cond: TestNode,
}

impl YieldStatementNode {
    pub fn new(line_info: LineInfo, is_from: bool, returned: TestListNode, cond: TestNode) -> Self {
        Self {
            line_info,
            is_from,
            returned,
            cond,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<YieldStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Yield)));
        let is_from = parse_if_matches!(tokens, TokenType::Keyword(Keyword::From))?.is_some();
        let (returned, cond) = TestListNode::parse_post_if(tokens, false)?;
        Ok(YieldStatementNode::new(
            info,
            is_from,
            returned,
            cond.unwrap_or_default(),
        ))
    }
}

impl Lined for YieldStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
