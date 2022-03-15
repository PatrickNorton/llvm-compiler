use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct SwitchStatementNode {
    line_info: LineInfo,
    switched: Box<TestNode>,
    cases: Vec<CaseStatementNode>,
}

#[derive(Debug)]
pub struct CaseStatementNode {
    line_info: LineInfo,
    label: Vec<TestNode>,
    body: StatementBodyNode,
    arrow: bool,
    as_stmt: VariableNode,
    is_default: bool,
}

impl SwitchStatementNode {
    pub fn new(
        line_info: LineInfo,
        switched: Box<TestNode>,
        cases: Vec<CaseStatementNode>,
    ) -> Self {
        Self {
            line_info,
            switched,
            cases,
        }
    }

    pub fn get_switched(&self) -> &TestNode {
        &self.switched
    }

    pub fn get_cases(&self) -> &[CaseStatementNode] {
        &self.cases
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<SwitchStatementNode> {
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Switch)));
        let switched = TestNode::parse(tokens)?;
        tokens.expect("{", true)?;
        let mut cases = Vec::new();
        while let TokenType::Keyword(Keyword::Case | Keyword::Default) = tokens.token_type()? {
            cases.push(CaseStatementNode::parse(tokens)?);
            if !tokens.token_equals("}")? {
                tokens.expect_newline()?;
            } else {
                break;
            }
        }
        tokens.expect("}", false)?;
        Ok(SwitchStatementNode::new(
            line_info,
            Box::new(switched),
            cases,
        ))
    }
}

impl CaseStatementNode {
    pub fn new(
        line_info: LineInfo,
        label: Vec<TestNode>,
        body: StatementBodyNode,
        arrow: bool,
        as_stmt: VariableNode,
    ) -> Self {
        Self {
            line_info,
            label,
            body,
            arrow,
            as_stmt,
            is_default: false,
        }
    }

    pub fn new_default(
        line_info: LineInfo,
        body: StatementBodyNode,
        arrow: bool,
        as_stmt: VariableNode,
    ) -> Self {
        Self {
            line_info,
            label: Vec::new(),
            body,
            arrow,
            as_stmt,
            is_default: true,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<CaseStatementNode> {
        if let TokenType::Keyword(Keyword::Default) = tokens.token_type()? {
            return Self::parse_default(tokens);
        }
        let (line_info, token_type) = tokens.next_token()?.deconstruct();
        assert!(matches!(token_type, TokenType::Keyword(Keyword::Case)));
        let label = TestNode::parse_list(tokens, false)?;
        let (body, arrow, as_stmt) = Self::parse_body(tokens)?;
        Ok(CaseStatementNode::new(
            line_info, label, body, arrow, as_stmt,
        ))
    }

    fn parse_default(tokens: &mut TokenList) -> ParseResult<CaseStatementNode> {
        let (line_info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Default)));
        let (body, arrow, as_stmt) = Self::parse_body(tokens)?;
        Ok(CaseStatementNode::new_default(
            line_info, body, arrow, as_stmt,
        ))
    }

    fn parse_body(tokens: &mut TokenList) -> ParseResult<(StatementBodyNode, bool, VariableNode)> {
        let as_stmt = VariableNode::parse_on_keyword(tokens, Keyword::As)?;
        let arrow = matches!(tokens.token_type()?, TokenType::DoubleArrow);
        let body =
            if let Option::Some(token) = parse_if_matches!(tokens, true, TokenType::DoubleArrow)? {
                let arrow_info = token.deconstruct().0;
                StatementBodyNode::new(
                    arrow_info,
                    vec![IndependentNode::Test(TestNode::parse(tokens)?)],
                )
            } else {
                StatementBodyNode::parse(tokens)?
            };
        Ok((body, arrow, as_stmt))
    }
}

impl CaseStatementNode {
    pub fn get_label(&self) -> &[TestNode] {
        &self.label
    }

    pub fn get_body(&self) -> &StatementBodyNode {
        &self.body
    }

    pub fn is_arrow(&self) -> bool {
        self.arrow
    }

    pub fn get_as(&self) -> &VariableNode {
        &self.as_stmt
    }

    pub fn is_default(&self) -> bool {
        self.is_default
    }
}

impl Lined for SwitchStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for CaseStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
