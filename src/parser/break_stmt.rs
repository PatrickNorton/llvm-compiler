use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct BreakStatementNode {
    line_info: LineInfo,
    loops: usize,
    cond: TestNode,
    as_stmt: TestNode,
}

impl BreakStatementNode {
    pub fn new(line_info: LineInfo, loops: usize, cond: TestNode, as_stmt: TestNode) -> Self {
        Self {
            line_info,
            loops,
            cond,
            as_stmt,
        }
    }

    pub fn get_loops(&self) -> usize {
        self.loops
    }

    pub fn get_cond(&self) -> &TestNode {
        &self.cond
    }

    pub fn get_as(&self) -> &TestNode {
        &self.as_stmt
    }

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
            (TestNode::parse(tokens)?, None)
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
