use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct SynchronizedStatementNode {
    line_info: LineInfo,
    value: TestNode,
    body: StatementBodyNode,
}

impl SynchronizedStatementNode {
    pub fn new(line_info: LineInfo, value: TestNode, body: StatementBodyNode) -> Self {
        Self {
            line_info,
            value,
            body,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<SynchronizedStatementNode> {
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Sync)));
        let value = TestNode::parse(tokens)?;
        let body = StatementBodyNode::parse(tokens)?;
        Ok(SynchronizedStatementNode::new(line_info, value, body))
    }
}

impl Lined for SynchronizedStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
