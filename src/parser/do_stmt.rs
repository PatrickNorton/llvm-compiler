use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct DoStatementNode {
    line_info: LineInfo,
    body: StatementBodyNode,
    conditional: TestNode,
}

impl DoStatementNode {
    pub fn new(line_info: LineInfo, body: StatementBodyNode, conditional: TestNode) -> Self {
        Self {
            line_info,
            body,
            conditional,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DoStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Do)));
        let body = StatementBodyNode::parse(tokens)?;
        if parse_if_matches!(tokens, TokenType::Keyword(Keyword::While))?.is_none() {
            return Err(tokens.error("Do statements must have a corresponding while"));
        }
        let conditional = TestNode::parse(tokens)?;
        Ok(DoStatementNode::new(info, body, conditional))
    }
}

impl Lined for DoStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
