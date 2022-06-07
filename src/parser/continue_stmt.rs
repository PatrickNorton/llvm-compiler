use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

/// The node representing a `continue` statement.
///
/// # Syntax
/// ```
/// "continue" ["if" TestNode]
/// ```
#[derive(Debug)]
pub struct ContinueStatementNode {
    line_info: LineInfo,
    cond: TestNode,
}

impl ContinueStatementNode {
    /// Creates a new [`ContinueStatementNode`].
    pub fn new(line_info: LineInfo, cond: TestNode) -> Self {
        Self { line_info, cond }
    }

    /// The conditional associated with the node, or [`TestNode::empty`] if no
    /// conditional is attached.
    pub fn get_cond(&self) -> &TestNode {
        &self.cond
    }

    /// Parses a [`ContinueStatementNode`] from the given list of tokens.
    ///
    /// If the first token in the list is not the `continue` keyword, this
    /// method will panic.
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
