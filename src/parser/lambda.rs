use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::typed_arg::TypedArgumentListNode;

#[derive(Debug)]
pub struct LambdaNode {
    line_info: LineInfo,
    args: TypedArgumentListNode,
    returns: Vec<TypeNode>,
    is_arrow: bool,
    body: StatementBodyNode,
}

impl LambdaNode {
    pub fn new(
        line_info: LineInfo,
        args: TypedArgumentListNode,
        returns: Vec<TypeNode>,
        is_arrow: bool,
        body: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            args,
            returns,
            is_arrow,
            body,
        }
    }

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<LambdaNode> {
        let (line_info, token_type) = tokens.next_tok(ignore_newlines)?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Lambda)));
        let args = TypedArgumentListNode::parse_optional_parens(tokens)?;
        let returns = TypeNode::parse_ret_val(tokens, ignore_newlines)?;
        let (is_arrow, body) = if let Option::Some(tok) =
            parse_if_matches!(tokens, ignore_newlines, TokenType::DoubleArrow)?
        {
            let (info, _) = tok.deconstruct();
            let body = TestNode::parse_newline(tokens, ignore_newlines)?;
            (
                true,
                StatementBodyNode::new(info, vec![IndependentNode::Test(body)]),
            )
        } else {
            (false, StatementBodyNode::parse(tokens)?)
        };
        Ok(LambdaNode::new(line_info, args, returns, is_arrow, body))
    }
}

impl Lined for LambdaNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
