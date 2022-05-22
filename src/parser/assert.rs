use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

/// The node representing an assertion.
///
/// # Grammar
/// ```text
/// assert [TestNode] (as [TestNode])?
/// ```
#[derive(Debug)]
pub struct AssertStatementNode {
    line_info: LineInfo,
    assertion: TestNode,
    as_statement: TestNode,
}

impl AssertStatementNode {
    /// Creates a new [`AssertStatementNode`].
    pub fn new(line_info: LineInfo, assertion: TestNode, as_statement: TestNode) -> Self {
        Self {
            line_info,
            assertion,
            as_statement,
        }
    }

    /// The expression that is being asserted.
    pub fn get_assertion(&self) -> &TestNode {
        &self.assertion
    }

    /// The `as` clause associated with the assertion.
    ///
    /// If no `as` clause was given, returns [`TestNode::empty`].
    pub fn get_as(&self) -> &TestNode {
        &self.as_statement
    }

    /// Parses an assertion from the given [`TokenList`].
    ///
    /// This method assumes the first token in the list is `assert`.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssertStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Assert)));
        let assertion = TestNode::parse(tokens)?;
        let as_stmt = TestNode::parse_on_keyword(tokens, Keyword::As, false)?;
        Ok(AssertStatementNode::new(info, assertion, as_stmt))
    }
}

impl Lined for AssertStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
