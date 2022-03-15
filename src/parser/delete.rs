use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct DeleteStatementNode {
    line_info: LineInfo,
    deletion: TestNode,
}

impl DeleteStatementNode {
    pub fn new(line_info: LineInfo, deletion: TestNode) -> Self {
        Self {
            line_info,
            deletion,
        }
    }

    pub fn get_deleted(&self) -> &TestNode {
        &self.deletion
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DeleteStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Del)));
        let assertion = TestNode::parse(tokens)?;
        Ok(DeleteStatementNode::new(info, assertion))
    }
}

impl Lined for DeleteStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
