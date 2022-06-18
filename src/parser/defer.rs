use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::return_stmt::ReturnStatementNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

/// The node representing a `defer` statement.
///
/// # Syntax
/// ```text
/// "defer" ([ReturnStatementNode] | [StatementBodyNode])
/// ```
#[derive(Debug)]
pub struct DeferStatementNode {
    line_info: LineInfo,
    body: StatementBodyNode,
}

impl DeferStatementNode {
    /// Creates a new  [`DeferStatementNode`].
    ///
    /// A `defer return` statement is represented by a `StatementBodyNode` with
    /// only the `return` statement in it.
    pub fn new(line_info: LineInfo, body: StatementBodyNode) -> Self {
        Self { line_info, body }
    }

    /// Parses a [`DeferStatementNode`] from the given list of tokens.
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
