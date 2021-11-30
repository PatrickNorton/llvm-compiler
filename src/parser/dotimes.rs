use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct DotimesStatementNode {
    line_info: LineInfo,
    iterations: TestNode,
    body: StatementBodyNode,
    nobreak: StatementBodyNode,
}

impl DotimesStatementNode {
    pub fn new(
        line_info: LineInfo,
        iterations: TestNode,
        body: StatementBodyNode,
        nobreak: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            iterations,
            body,
            nobreak,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DotimesStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Do)));
        let iterations = TestNode::parse(tokens)?;
        let body = StatementBodyNode::parse(tokens)?;
        let nobreak = StatementBodyNode::parse_on_keyword(tokens, Keyword::Nobreak)?;
        Ok(DotimesStatementNode::new(info, iterations, body, nobreak))
    }
}

impl Lined for DotimesStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
