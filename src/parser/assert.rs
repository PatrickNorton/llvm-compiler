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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parser::name::NameNode;
    use crate::parser::test_node::TestNode;
    use crate::parser::tokenizer::Tokenizer;

    use super::AssertStatementNode;

    fn variable_name(node: &TestNode) -> &str {
        match node {
            TestNode::Name(NameNode::Variable(v)) => v.get_name(),
            x => panic!("Expected variable, got {:?}", x),
        }
    }

    #[test]
    fn simple_assert() {
        let mut token_list = Tokenizer::parse_str("assert foo", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(variable_name(assertion.get_assertion()), "foo");
        assert!(assertion.get_as().is_empty());
    }

    #[test]
    fn assert_with_as() {
        let mut token_list =
            Tokenizer::parse_str("assert foo as bar", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(variable_name(assertion.get_assertion()), "foo");
        assert_eq!(variable_name(assertion.get_as()), "bar");
    }

    #[test]
    fn assert_eol() {
        let mut token_list = Tokenizer::parse_str("assert\n", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list);
        assert!(assertion.is_err(), "{:?}", assertion);
    }

    #[test]
    fn assert_eof() {
        let mut token_list = Tokenizer::parse_str("assert", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list);
        assert!(assertion.is_err(), "{:?}", assertion);
    }

    #[test]
    fn assert_as_eol() {
        let mut token_list =
            Tokenizer::parse_str("assert foo as\n", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list);
        assert!(assertion.is_err(), "{:?}", assertion);
    }

    #[test]
    fn assert_as_eof() {
        let mut token_list = Tokenizer::parse_str("assert foo as", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list);
        assert!(assertion.is_err(), "{:?}", assertion);
    }

    #[test]
    fn assert_empty_with_as() {
        let mut token_list = Tokenizer::parse_str("assert as foo", PathBuf::from("/"), 0).unwrap();
        let assertion = AssertStatementNode::parse(&mut token_list);
        assert!(assertion.is_err(), "{:?}", assertion);
    }
}
