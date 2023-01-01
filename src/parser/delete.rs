use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

/// A node representing a `delete` statement.
///
/// # Syntax
/// ```text
/// "delete" [TestNode]
/// ```
#[derive(Debug)]
pub struct DeleteStatementNode {
    line_info: LineInfo,
    deletion: TestNode,
}

impl DeleteStatementNode {
    /// Create a new [`DeleteStatementNode`].
    pub fn new(line_info: LineInfo, deletion: TestNode) -> Self {
        Self {
            line_info,
            deletion,
        }
    }

    /// The node that is to be deleted.
    ///
    /// # Caveats
    ///
    /// Note that this does not necessarily represent a valid deletable node;
    /// this could be any [`TestNode`] (even beyond the fact that it might not
    /// refer to an assigned name). As an example, `delete true` could make it
    /// to this stage, even though that statement is invalid. More importantly,
    /// `delete foo()` or `delete match { ... }` would still be parsed here,
    /// even though they are totally nonsensical statements. When used, care
    /// should be taken to make sure that this returns a validly deletable
    /// statement and not just anything.
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
