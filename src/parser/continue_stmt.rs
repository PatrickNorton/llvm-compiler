use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct ContinueStatementNode {
    line_info: LineInfo,
    cond: TestNode,
}

impl ContinueStatementNode {
    pub fn new(line_info: LineInfo, cond: TestNode) -> Self {
        Self { line_info, cond }
    }

    pub fn get_cond(&self) -> &TestNode {
        &self.cond
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ContinueStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Continue)));
        let cond = TestNode::parse_on_keyword(tokens, Keyword::If, false)?;
        Ok(ContinueStatementNode::new(info, cond))
    }
}

impl Lined for ContinueStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
