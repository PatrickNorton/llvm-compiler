use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::return_stmt::ReturnStatementNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct DeferStatementNode {
    line_info: LineInfo,
    body: StatementBodyNode,
}

impl DeferStatementNode {
    pub fn new(line_info: LineInfo, body: StatementBodyNode) -> Self {
        Self { line_info, body }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DeferStatementNode> {
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Defer)));
        let body = if let TokenType::Keyword(Keyword::Return) = tokens.token_type()? {
            let statement = ReturnStatementNode::parse(tokens)?;
            StatementBodyNode::new(
                statement.line_info().clone(),
                vec![IndependentNode::Return(statement)],
            )
        } else {
            StatementBodyNode::parse(tokens)?
        };
        Ok(DeferStatementNode::new(line_info, body))
    }
}

impl Lined for DeferStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
