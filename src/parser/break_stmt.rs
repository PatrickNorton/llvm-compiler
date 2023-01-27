use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

/// A node representing a `break` statement.
///
/// # Syntax
/// ```text
/// "break" [number] ["as" TestNode] ["if" TestNode]
/// ```
#[derive(Debug)]
pub struct BreakStatementNode {
    line_info: LineInfo,
    loops: usize,
    cond: TestNode,
    as_stmt: TestNode,
}

impl BreakStatementNode {
    /// Creates a new [`BreakStatementNode`].
    pub fn new(line_info: LineInfo, loops: usize, cond: TestNode, as_stmt: TestNode) -> Self {
        Self {
            line_info,
            loops,
            cond,
            as_stmt,
        }
    }

    /// The depth of loops that are to be broken out from.
    pub fn get_loops(&self) -> usize {
        self.loops
    }

    /// The condition associated with the `break`.
    ///
    /// If the method returns [`TestNode::empty`], then there is no condition.
    pub fn get_cond(&self) -> &TestNode {
        &self.cond
    }

    /// The `as` clause associated with the `break`.
    ///
    /// If the method returns [`TestNode::empty`], then there is no associated
    /// `as` clause.
    pub fn get_as(&self) -> &TestNode {
        &self.as_stmt
    }

    /// Parses a [`BreakStatementNode`] from the given list of tokens.
    ///
    /// This method assumes the first token in the list is the `break` keyword.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<BreakStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Break)));
        let loops = if let TokenType::Number(_) = tokens.token_type()? {
            tokens.next_token()?.into_sequence().parse().unwrap()
        } else {
            1
        };
        let (as_stmt, cond) = if let TokenType::Keyword(Keyword::As) = tokens.token_type()? {
            tokens.next_token()?;
            TestNode::parse_maybe_post_if(tokens, false)?
        } else if let TokenType::Keyword(Keyword::If) = tokens.token_type()? {
            tokens.next_token()?;
            (TestNode::empty(), Some(TestNode::parse(tokens)?))
        } else {
            (TestNode::empty(), None)
        };
        Ok(BreakStatementNode::new(
            info,
            loops,
            cond.unwrap_or_else(TestNode::empty),
            as_stmt,
        ))
    }
}

impl Lined for BreakStatementNode {
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

    use super::BreakStatementNode;

    fn variable_name(node: &TestNode) -> &str {
        match node {
            TestNode::Name(NameNode::Variable(v)) => v.get_name(),
            x => panic!("Expected variable, got {x:?}"),
        }
    }

    #[test]
    fn simple_break() {
        let mut token_list = Tokenizer::parse_str("break", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 1);
        assert!(node.get_cond().is_empty());
        assert!(node.get_as().is_empty());
    }

    #[test]
    fn break_as() {
        let mut token_list = Tokenizer::parse_str("break as foo", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 1);
        assert!(node.get_cond().is_empty());
        assert_eq!(variable_name(node.get_as()), "foo");
    }

    #[test]
    fn break_cond() {
        let mut token_list = Tokenizer::parse_str("break if foo", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 1);
        assert_eq!(variable_name(node.get_cond()), "foo");
        assert!(node.get_as().is_empty());
    }

    #[test]
    fn break_loops() {
        let mut token_list = Tokenizer::parse_str("break 3", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 3);
        assert!(node.get_cond().is_empty());
        assert!(node.get_as().is_empty());
    }

    #[test]
    fn break_as_cond() {
        let mut token_list =
            Tokenizer::parse_str("break as foo if bar", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 1);
        assert_eq!(variable_name(node.get_cond()), "bar");
        assert_eq!(variable_name(node.get_as()), "foo");
    }

    #[test]
    fn break_as_loops() {
        let mut token_list = Tokenizer::parse_str("break 3 as foo", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 3);
        assert!(node.get_cond().is_empty());
        assert_eq!(variable_name(node.get_as()), "foo");
    }

    #[test]
    fn break_loop_cond() {
        let mut token_list = Tokenizer::parse_str("break 3 if foo", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 3);
        assert_eq!(variable_name(node.get_cond()), "foo");
        assert!(node.get_as().is_empty());
    }

    #[test]
    fn break_all() {
        let mut token_list =
            Tokenizer::parse_str("break 3 as foo if bar", PathBuf::from("/"), 0).unwrap();
        let node = BreakStatementNode::parse(&mut token_list).unwrap();
        assert_eq!(node.get_loops(), 3);
        assert_eq!(variable_name(node.get_cond()), "bar");
        assert_eq!(variable_name(node.get_as()), "foo");
    }
}
