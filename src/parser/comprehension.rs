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

/// A node representing a list comprehension, set comprehension, or generator
/// literal.
///
/// # Syntax
/// ```text
/// OPEN_BRACE TestNode *("," TestNode) [","] "for" VarLikeNode
///     *("," VarLikeNode) [","] "in" TestNode *("," TestNode) [","]
///     ["if" TestNode] ["while" TestNode] CLOSE_BRACE
/// ```
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

/// A node representing a dictionary comprehension.
///
/// # Syntax
/// ```text
/// "{" TestNode ":" TestNode "for" VarLikeNode *("," VarLikeNode) [","] "in"
///     TestNode *("," VarLikeNode) [","] ["if" TestNode] ["while" TestNode] "}"
/// ```
#[derive(Debug)]
pub struct DictComprehensionNode {
    line_info: LineInfo,
    variables: Vec<VarLikeNode>,
    key: Box<TestNode>,
    builder: Box<TestNode>,
    looped: TestListNode,
    condition: Box<TestNode>,
    while_cond: Box<TestNode>,
}

impl ComprehensionNode {
    /// Creates a new [`ComprehensionNode`].
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

    /// The type of brace surrounding the comprehension.
    ///
    /// This can only return 3 chars (and may be replaced with an enum in the
    /// future):
    /// - `'('`: for generator literals
    /// - `'['`: for list comprehensions
    /// - `'{'`: for set comprehensions
    pub fn get_brace(&self) -> char {
        self.brace
    }

    /// The list of statements that are being looped over.
    pub fn get_looped(&self) -> &TestListNode {
        &self.looped
    }

    /// The list of variable names in the loop.
    pub fn get_variables(&self) -> &[VarLikeNode] {
        &self.variables
    }

    /// The conditional statement in the comprehension, or [`TestNode::empty`]
    /// when the node has no conditional.
    pub fn get_condition(&self) -> &TestNode {
        &self.condition
    }

    /// The list of builders of the comprehension.
    pub fn get_builder(&self) -> &[ArgumentNode] {
        &self.builder
    }

    /// The `while` conditional in the comprehension, or [`TestNode::empty`]
    /// when the node has no conditional.
    pub fn get_while_cond(&self) -> &TestNode {
        &self.while_cond
    }

    /// Parses a comprehension from the given list of tokens.
    ///
    /// This assumes that the first token in the list is an open brace, or the
    /// method will panic.
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
            Err(if let TokenType::CloseBrace(c) = *(tokens.token_type()?) {
                tokens.error(format!(
                    "Unmatched brace: {} does not match {}",
                    close_brace, c
                ))
            } else {
                tokens.error_expected("close brace")
            })
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
    /// Creates a new [`DictComprehensionNode`].
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
            variables,
            key,
            builder,
            looped,
            condition,
            while_cond,
        }
    }

    /// The list of variable names in the `for` loop.
    pub fn get_variables(&self) -> &[VarLikeNode] {
        &self.variables
    }

    /// The key expression in the dict comprehension.
    pub fn get_key(&self) -> &TestNode {
        &self.key
    }

    /// The value expression in the dict comprehension.
    pub fn get_builder(&self) -> &TestNode {
        &self.builder
    }

    /// The list of expressions that are looped over in the `for` loop.
    pub fn get_looped(&self) -> &TestListNode {
        &self.looped
    }

    /// The condition in the comprehension.
    pub fn get_condition(&self) -> &TestNode {
        &self.condition
    }

    /// Parses a dict comprehension from the given list of tokens.
    ///
    /// This assumes the first token in the list is `"{"`, otherwise it will
    /// panic.
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
