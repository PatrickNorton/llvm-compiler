use std::ops::Index;

use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct StatementBodyNode {
    line_info: LineInfo,
    statements: Vec<IndependentNode>,
}

impl StatementBodyNode {
    pub const fn empty() -> Self {
        Self::new(LineInfo::empty(), Vec::new())
    }

    pub const fn new(line_info: LineInfo, statements: Vec<IndependentNode>) -> Self {
        Self {
            line_info,
            statements,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<StatementBodyNode> {
        if !tokens.token_equals("{")? {
            return Err(tokens.error("The body of a function must be enclosed in curly brackets"));
        }
        let (line_info, _) = tokens.next_tok(true)?.deconstruct();
        let st = Self::parse_until_token(line_info, tokens, ["}"])?;
        assert!(tokens.token_equals("}")?);
        tokens.next_token()?;
        Ok(st)
    }

    pub fn parse_on_token(tokens: &mut TokenList, keyword: &str) -> ParseResult<StatementBodyNode> {
        if tokens.token_equals(keyword)? {
            tokens.next_token()?;
            Self::parse(tokens)
        } else {
            Ok(Self::empty())
        }
    }

    pub fn parse_on_keyword(
        tokens: &mut TokenList,
        keyword: Keyword,
    ) -> ParseResult<StatementBodyNode> {
        if parse_if_matches!(tokens, &TokenType::Keyword(k) if k == keyword)?.is_some() {
            Self::parse(tokens)
        } else {
            Ok(Self::empty())
        }
    }

    fn parse_until_token<const N: usize>(
        line_info: LineInfo,
        tokens: &mut TokenList,
        values: [&'static str; N],
    ) -> ParseResult<StatementBodyNode> {
        let mut statements = Vec::new();
        while !values.contains(&tokens.first()?.get_sequence()) {
            statements.push(IndependentNode::parse(tokens)?);
            if !values.contains(&tokens.first()?.get_sequence()) {
                tokens.expect_newline()?;
            }
        }
        Ok(StatementBodyNode::new(line_info, statements))
    }
}

impl Default for StatementBodyNode {
    fn default() -> Self {
        Self::empty()
    }
}

impl Lined for StatementBodyNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> IntoIterator for &'a StatementBodyNode {
    type Item = &'a IndependentNode;

    type IntoIter = <&'a [IndependentNode] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.iter()
    }
}

impl Index<usize> for StatementBodyNode {
    type Output = IndependentNode;

    fn index(&self, index: usize) -> &Self::Output {
        &self.statements[index]
    }
}
