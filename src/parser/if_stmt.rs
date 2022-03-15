use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct IfStatementNode {
    line_info: LineInfo,
    conditional: Box<TestNode>,
    as_stmt: VariableNode,
    body: StatementBodyNode,
    elifs: Vec<ElifStatementNode>,
    else_stmt: StatementBodyNode,
}

#[derive(Debug)]
pub struct ElifStatementNode {
    line_info: LineInfo,
    test: TestNode,
    as_stmt: VariableNode,
    body: StatementBodyNode,
}

impl IfStatementNode {
    pub fn new(
        line_info: LineInfo,
        conditional: Box<TestNode>,
        as_stmt: VariableNode,
        body: StatementBodyNode,
        elifs: Vec<ElifStatementNode>,
        else_stmt: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            conditional,
            as_stmt,
            body,
            elifs,
            else_stmt,
        }
    }

    pub fn get_conditional(&self) -> &TestNode {
        &self.conditional
    }

    pub fn get_as(&self) -> &VariableNode {
        &self.as_stmt
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
    }

    pub fn get_elifs(&self) -> &[ElifStatementNode] {
        &self.elifs
    }

    pub fn get_else(&self) -> &StatementBodyNode {
        &self.else_stmt
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<IfStatementNode> {
        let (info, token) = tokens.next_token()?.deconstruct();
        assert_eq!(token, TokenType::Keyword(Keyword::If));
        let test = TestNode::parse(tokens)?;
        let as_stmt = VariableNode::parse_on_keyword(tokens, Keyword::As)?;
        let body = StatementBodyNode::parse(tokens)?;
        let mut elifs = Vec::new();
        while let TokenType::Keyword(Keyword::Elif) = tokens.token_type()? {
            tokens.next_token()?;
            let elif_condition = TestNode::parse(tokens)?;
            let elif_as = VariableNode::parse_on_keyword(tokens, Keyword::As)?;
            let elif_body = StatementBodyNode::parse(tokens)?;
            elifs.push(ElifStatementNode::new(
                info.clone(),
                elif_condition,
                elif_as,
                elif_body,
            ));
        }
        if let TokenType::Keyword(Keyword::Else) = tokens.token_type()? {
            if let TokenType::Keyword(Keyword::If) = tokens.token_type_at(1)? {
                return Err(tokens.error("'else if' is not legal, use 'elif' instead"));
            }
        }
        let else_stmt = StatementBodyNode::parse_on_keyword(tokens, Keyword::Else)?;
        Ok(IfStatementNode::new(
            info,
            Box::new(test),
            as_stmt,
            body,
            elifs,
            else_stmt,
        ))
    }
}

impl ElifStatementNode {
    fn new(
        line_info: LineInfo,
        test: TestNode,
        as_stmt: VariableNode,
        body: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            test,
            as_stmt,
            body,
        }
    }

    pub fn get_test(&self) -> &TestNode {
        &self.test
    }

    pub fn get_as(&self) -> &VariableNode {
        &self.as_stmt
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
    }
}

impl Lined for IfStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for ElifStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
