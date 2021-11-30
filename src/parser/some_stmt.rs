use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator::{OperatorNode, OperatorTypeNode};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::util::first_n;

#[derive(Debug)]
pub struct SomeStatementNode {
    line_info: LineInfo,
    contained: Box<TestNode>,
    container: Box<TestNode>,
}

impl SomeStatementNode {
    pub fn new(line_info: LineInfo, contained: Box<TestNode>, container: Box<TestNode>) -> Self {
        Self {
            line_info,
            contained,
            container,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<SomeStatementNode> {
        let (line_info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Some)));
        let contained = TestNode::parse(tokens)?;
        if let Result::Ok(in_stmt) = OperatorNode::try_from(contained) {
            let (operator, operands) = in_stmt.deconstruct();
            if operator != OperatorTypeNode::In {
                Err(tokens.error_expected("in"))
            } else {
                let [a, b] = first_n(operands);
                Ok(SomeStatementNode::new(
                    line_info,
                    Box::new(a.into_argument()),
                    Box::new(b.into_argument()),
                ))
            }
        } else {
            Err(tokens.error_expected("in"))
        }
    }
}

impl Lined for SomeStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
