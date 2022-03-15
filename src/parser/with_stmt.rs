use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_list::TestListNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::typed_var::TypedVariableNode;

#[derive(Debug)]
pub struct WithStatementNode {
    line_info: LineInfo,
    managed: TestListNode,
    vars: Vec<TypedVariableNode>,
    body: StatementBodyNode,
}

impl WithStatementNode {
    pub fn new(
        line_info: LineInfo,
        managed: TestListNode,
        vars: Vec<TypedVariableNode>,
        body: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            managed,
            vars,
            body,
        }
    }

    pub fn get_managed(&self) -> &TestListNode {
        &self.managed
    }

    pub fn get_vars(&self) -> &[TypedVariableNode] {
        &self.vars
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<WithStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Try)));
        let managed = TestListNode::parse(tokens, false)?;
        let vars = TypedVariableNode::parse_list_on_keyword(tokens, Keyword::As)?;
        let body = StatementBodyNode::parse(tokens)?;
        Ok(WithStatementNode::new(info, managed, vars, body))
    }
}

impl Lined for WithStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
