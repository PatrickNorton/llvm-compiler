use crate::parser::argument::ArgumentNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VarLikeNode;

#[derive(Debug)]
pub struct ComprehensionNode {
    line_info: LineInfo,
    brace: char,
    variables: Vec<VarLikeNode>,
    builder: Vec<ArgumentNode>,
    looped: TestListNode,
    condition: Box<TestNode>,
    while_cond: Box<TestNode>,
}

#[derive(Debug)]
pub struct DictComprehensionNode {
    line_info: LineInfo,
    brace: char,
    variables: Vec<VarLikeNode>,
    key: Box<TestNode>,
    builder: Box<TestNode>,
    looped: TestListNode,
    condition: Box<TestNode>,
    while_cond: Box<TestNode>,
}

impl ComprehensionNode {
    pub fn new(
        line_info: LineInfo,
        brace: char,
        variables: Vec<VarLikeNode>,
        builder: Vec<ArgumentNode>,
        looped: TestListNode,
        condition: Box<TestNode>,
        while_cond: Box<TestNode>,
    ) -> Self {
        Self {
            line_info,
            brace,
            variables,
            builder,
            looped,
            condition,
            while_cond,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ComprehensionNode> {
        let (line_info, token) = tokens.next_tok(true)?.deconstruct();
        let brace = match token {
            TokenType::OpenBrace(x) => x,
            _ => panic!("Expected an open brace"),
        };
        let close_brace = TokenList::matching_brace(brace);
        let builder = ArgumentNode::parse_brace_free_list(tokens)?;
        if parse_if_matches!(tokens, true, TokenType::Keyword(Keyword::For))?.is_none() {
            return Err(tokens.error("Invalid start to comprehension"));
        }
        let variables = VarLikeNode::parse_list(tokens, true)?;
        if parse_if_matches!(tokens, TokenType::Keyword(Keyword::In))?.is_none() {
            return Err(tokens.error("Comprehension body must have in after variable list"));
        }
        let (looped, condition) = TestListNode::parse_post_if(tokens, true)?;
        let while_cond = TestNode::parse_on_keyword(tokens, Keyword::While, true)?;
        if !tokens.token_equals(close_brace.to_string())? {
            return Err(if let TokenType::CloseBrace(c) = *(tokens.token_type()?) {
                tokens.error(format!(
                    "Unmatched brace: {} does not match {}",
                    close_brace, c
                ))
            } else {
                tokens.error_expected("close brace")
            });
        } else {
            tokens.next_token()?;
            Ok(ComprehensionNode::new(
                line_info,
                brace,
                variables,
                builder,
                looped,
                Box::new(condition.unwrap_or_else(TestNode::empty)),
                Box::new(while_cond),
            ))
        }
    }
}

impl DictComprehensionNode {
    pub fn new(
        line_info: LineInfo,
        variables: Vec<VarLikeNode>,
        key: Box<TestNode>,
        builder: Box<TestNode>,
        looped: TestListNode,
        condition: Box<TestNode>,
        while_cond: Box<TestNode>,
    ) -> Self {
        Self {
            line_info,
            brace: '{',
            variables,
            key,
            builder,
            looped,
            condition,
            while_cond,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DictComprehensionNode> {
        assert!(tokens.token_equals("{")?);
        let line_info = tokens.next_tok(true)?.deconstruct().0;
        let key = TestNode::parse_newline(tokens, true)?;
        tokens.expect(":", true)?;
        let val = TestNode::parse_newline(tokens, true)?;
        tokens.expect_keyword(Keyword::For, true)?;
        let vars = VarLikeNode::parse_list(tokens, true)?;
        tokens.expect_keyword(Keyword::In, true)?;
        let (looped, condition) = TestListNode::parse_post_if(tokens, true)?;
        let while_cond = TestNode::parse_on_keyword(tokens, Keyword::While, true)?;
        tokens.expect("}", false)?;
        Ok(DictComprehensionNode::new(
            line_info,
            vars,
            Box::new(key),
            Box::new(val),
            looped,
            Box::new(condition.unwrap_or_else(TestNode::empty)),
            Box::new(while_cond),
        ))
    }
}

impl Lined for ComprehensionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for DictComprehensionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
