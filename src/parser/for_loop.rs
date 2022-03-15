use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_list::TestListNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VarLikeNode;

#[derive(Debug)]
pub struct ForStatementNode {
    line_info: LineInfo,
    vars: Vec<VarLikeNode>,
    iterables: TestListNode,
    body: StatementBodyNode,
    nobreak: StatementBodyNode,
}

impl ForStatementNode {
    pub fn new(
        line_info: LineInfo,
        vars: Vec<VarLikeNode>,
        iterables: TestListNode,
        body: StatementBodyNode,
        nobreak: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            vars,
            iterables,
            body,
            nobreak,
        }
    }

    pub fn get_vars(&self) -> &[VarLikeNode] {
        &self.vars
    }

    pub fn get_iterables(&self) -> &TestListNode {
        &self.iterables
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
    }

    pub fn get_nobreak(&self) -> &StatementBodyNode {
        &self.nobreak
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ForStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::For)));
        let ignore_newlines = tokens.token_equals("(")?;
        if ignore_newlines {
            tokens.next_tok(true)?;
        }
        let vars = VarLikeNode::parse_list(tokens, ignore_newlines)?;
        tokens.expect_keyword(Keyword::In, ignore_newlines)?;
        let iterables = TestListNode::parse(tokens, ignore_newlines)?;
        if ignore_newlines {
            tokens.expect(")", false)?;
        }
        let body = StatementBodyNode::parse(tokens)?;
        let nobreak = StatementBodyNode::parse_on_keyword(tokens, Keyword::Nobreak)?;
        Ok(ForStatementNode::new(info, vars, iterables, body, nobreak))
    }
}

impl Lined for ForStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
