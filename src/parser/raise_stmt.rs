use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::LineInfo;
use crate::parser::line_info::Lined;
use crate::parser::macros::parse_if_matches;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct RaiseStatementNode {
    line_info: LineInfo,
    raised: Box<TestNode>,
    condition: Box<TestNode>,
    from: Box<TestNode>,
}

impl RaiseStatementNode {
    pub fn new(
        line_info: LineInfo,
        raised: Box<TestNode>,
        condition: Box<TestNode>,
        from: Box<TestNode>,
    ) -> Self {
        Self {
            line_info,
            raised,
            condition,
            from,
        }
    }

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<RaiseStatementNode> {
        let (line_info, token_type) = tokens.next_tok(ignore_newlines)?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Raise)));
        let (raised, cond) = TestNode::parse_maybe_post_if(tokens, ignore_newlines)?;
        let (condition, from) = if cond.is_none()
            && parse_if_matches!(tokens, TokenType::Keyword(Keyword::From))?.is_some()
        {
            TestNode::parse_maybe_post_if(tokens, ignore_newlines)?
        } else {
            (TestNode::empty(), cond)
        };
        Ok(RaiseStatementNode::new(
            line_info,
            Box::new(raised),
            Box::new(condition),
            Box::new(from.unwrap_or_else(TestNode::empty)),
        ))
    }
}

impl Lined for RaiseStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
