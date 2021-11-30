use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct WhileStatementNode {
    line_info: LineInfo,
    cond: TestNode,
    as_stmt: VariableNode,
    body: StatementBodyNode,
    nobreak: StatementBodyNode,
}

impl WhileStatementNode {
    pub fn new(
        line_info: LineInfo,
        cond: TestNode,
        as_stmt: VariableNode,
        body: StatementBodyNode,
        nobreak: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            cond,
            as_stmt,
            body,
            nobreak,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<WhileStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::While)));
        let cond = TestNode::parse(tokens)?;
        let as_stmt = VariableNode::parse_on_keyword(tokens, Keyword::As)?;
        let body = StatementBodyNode::parse(tokens)?;
        let nobreak = StatementBodyNode::parse_on_keyword(tokens, Keyword::Nobreak)?;
        Ok(WhileStatementNode::new(info, cond, as_stmt, body, nobreak))
    }
}

impl Lined for WhileStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
