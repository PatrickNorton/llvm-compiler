use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct ReturnStatementNode {
    line_info: LineInfo,
    returned: TestListNode,
    cond: TestNode,
}

impl ReturnStatementNode {
    pub fn new(line_info: LineInfo, returned: TestListNode, cond: TestNode) -> Self {
        Self {
            line_info,
            returned,
            cond,
        }
    }

    pub fn get_returned(&self) -> &TestListNode {
        &self.returned
    }

    pub fn get_cond(&self) -> &TestNode {
        &self.cond
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ReturnStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Return)));
        let (returned, cond) = TestListNode::parse_post_if(tokens, false)?;
        Ok(ReturnStatementNode::new(
            info,
            returned,
            cond.unwrap_or_default(),
        ))
    }
}

impl Lined for ReturnStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
