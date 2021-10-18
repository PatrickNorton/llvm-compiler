use crate::parser::comprehension::{ComprehensionNode, DictComprehensionNode};
use crate::parser::dotted::DottedVariableNode;
use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::formatted_string::FormattedStringNode;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::literal::{DictLiteralNode, LiteralNode};
use crate::parser::name::NameNode;
use crate::parser::number::NumberNode;
use crate::parser::operator::{OperatorNode, OperatorTypeNode};
use crate::parser::operator_fn::EscapedOperatorNode;
use crate::parser::post_dot::PostDottableNode;
use crate::parser::range::RangeLiteralNode;
use crate::parser::string::StringNode;
use crate::parser::string_like::StringLikeNode;
use crate::parser::ternary::TernaryNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use std::collections::VecDeque;
use std::convert::TryFrom;

#[derive(Debug)]
pub enum TestNode {
    Comprehension(ComprehensionNode),
    DictComp(DictComprehensionNode),
    DictLiteral(DictLiteralNode),
    Empty(EmptyTestNode),
    Formatted(FormattedStringNode),
    Literal(LiteralNode),
    Name(NameNode),
    Number(NumberNode),
    Operator(OperatorNode),
    OperatorType(OperatorTypeNode),
    Range(RangeLiteralNode),
    String(StringNode),
    Ternary(Box<TernaryNode>),
}

#[derive(Debug)]
pub struct EmptyTestNode {
    line_info: LineInfo,
}

impl TestNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<TestNode> {
        Self::parse_newline(tokens, false)
    }

    pub fn empty() -> TestNode {
        TestNode::Empty(EmptyTestNode {
            line_info: LineInfo::empty(),
        })
    }

    fn parse_no_ternary(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TestNode> {
        let line_info = tokens.line_info()?.clone();
        let node = Self::parse_expression(tokens, ignore_newlines)?;
        if matches!(
            tokens.token_type()?,
            TokenType::Assign(_) | TokenType::AugAssign(_)
        ) {
            return Err(ParserError::Normal(ParserException::of(
                "Illegal assignment",
                line_info,
            )));
        }
        match PostDottableNode::try_from(node) {
            Result::Ok(x) => Self::parse_post(tokens, x.into(), ignore_newlines),
            Result::Err(node) => Ok(node),
        }
    }

    pub fn parse_newline(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TestNode> {
        let if_true = Self::parse_no_ternary(tokens, ignore_newlines)?;
        if matches!(tokens.token_type()?, TokenType::Keyword(Keyword::If)) {
            tokens.next_tok(ignore_newlines)?;
            let statement = Self::parse_newline(tokens, ignore_newlines)?;
            if !matches!(tokens.token_type()?, TokenType::Keyword(Keyword::Else)) {
                return Err(tokens.error("Ternary must have an else"));
            }
            tokens.next_tok(ignore_newlines)?;
            let if_false = Self::parse_newline(tokens, ignore_newlines)?;
            Ok(TestNode::Ternary(Box::new(TernaryNode::new(
                if_true, statement, if_false,
            ))))
        } else {
            Ok(if_true)
        }
    }

    pub fn parse_on_keyword(
        tokens: &mut TokenList,
        keyword: Keyword,
        ignore_newlines: bool,
    ) -> ParseResult<TestNode> {
        if matches!(tokens.token_type()?, TokenType::Keyword(k) if k == &keyword) {
            Self::parse_newline(tokens, ignore_newlines)
        } else {
            Ok(Self::empty())
        }
    }

    fn parse_expression(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TestNode> {
        let mut queue = VecDeque::<StackValue>::new();
        let mut stack = VecDeque::<DummyOp>::new();
        let mut parse_curly = true;
        loop {
            let line_info = tokens.line_info()?.clone();
            let node = Self::parse_node(tokens, ignore_newlines, parse_curly)?;
            match node {
                Option::None => break,
                Option::Some(TestNode::OperatorType(op)) => {
                    // Convert - to u- where needed
                    let operator = if op == OperatorTypeNode::Subtract && parse_curly {
                        OperatorTypeNode::USubtract
                    } else {
                        op
                    };
                    // Operators in a place where they shouldn't be, e.g. 1 + * 2
                    if parse_curly ^ (operator.is_unary() && !operator.is_postfix()) {
                        return Err(tokens.default_error());
                    }
                    // Push all operators that bind more tightly onto the queue
                    while stack
                        .get(0)
                        .map_or_else(|| false, |x| operator.precedence() >= x.op.precedence())
                    {
                        queue.push_back(StackValue::Op(stack.pop_front().unwrap()));
                    }
                    // Postfix operators don't go on the stack, as they have no
                    // arguments left to be parsed
                    if operator.is_postfix() {
                        queue.push_back(StackValue::Op(DummyOp::new(operator, line_info)))
                    } else {
                        stack.push_front(DummyOp::new(operator, line_info.clone()))
                    }
                }
                Option::Some(node) => {
                    if !parse_curly {
                        return Err(tokens.default_error());
                    }
                    // Non-operators just get pushed onto the stack
                    queue.push_back(StackValue::Node(node));
                    parse_curly = false;
                }
            }
        }
        while let Option::Some(x) = stack.pop_front() {
            queue.push_back(StackValue::Op(x))
        }
        if queue.is_empty() {
            Err(tokens.error("Illegal empty statement"))
        } else {
            convert_queue_to_node(queue)
        }
    }

    fn parse_node(
        tokens: &mut TokenList,
        ignore_newlines: bool,
        parse_curly: bool,
    ) -> ParseResult<Option<TestNode>> {
        let node = Self::parse_internal_node(tokens, ignore_newlines, parse_curly)?;
        Ok(match node {
            Option::Some(node) => match PostDottableNode::try_from(node) {
                Result::Ok(x) => Option::Some(Self::parse_post(tokens, x.into(), ignore_newlines)?),
                Result::Err(node) => Option::Some(node),
            },
            Option::None => Option::None,
        })
    }

    fn parse_internal_node(
        tokens: &mut TokenList,
        ignore_newlines: bool,
        parse_curly: bool,
    ) -> ParseResult<Option<TestNode>> {
        if ignore_newlines {
            tokens.pass_newlines()?;
        }
        match tokens.token_type()? {
            TokenType::OpenBrace(c) => {
                if parse_curly || *c != '{' {
                    Self::parse_open_brace(tokens, ignore_newlines).map(Option::Some)
                } else {
                    ParseResult::Ok(Option::None)
                }
            }
            TokenType::Name(_) => {
                NameNode::parse_newline(tokens, ignore_newlines).map(|x| Some(TestNode::Name(x)))
            }
            TokenType::Number(_) => Ok(Some(TestNode::Number(NumberNode::parse(tokens)?))),
            TokenType::Operator(op) => Ok(Some(TestNode::OperatorType(*op))),
            TokenType::OpFunc(_) => EscapedOperatorNode::parse(tokens, ignore_newlines)
                .map(|x| Some(TestNode::Name(NameNode::EscapedOp(x)))),
            TokenType::Newline => {
                if ignore_newlines {
                    panic!("Illegal place for newline")
                } else {
                    ParseResult::Ok(Option::None)
                }
            }
            TokenType::String(_) => StringLikeNode::parse(tokens).map(|x| Some(x.into())),
            TokenType::Keyword(_) => Self::parse_keyword_node(tokens, ignore_newlines),
            _ => ParseResult::Ok(Option::None),
        }
    }

    fn parse_keyword_node(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<Option<TestNode>> {
        let (token, _) = tokens.next_tok(ignore_newlines)?.deconstruct();
        match token {
            TokenType::Keyword(key) => match key {
                Keyword::Some => todo!("SomeStatementNode::parse(tokens)"),
                Keyword::In => todo!("OperatorTypeNode::parse(tokens)"),
                Keyword::Switch => todo!("SwitchStatementNode::parse(tokens)"),
                Keyword::Lambda => todo!("LambdaNode::parse(tokens)"),
                Keyword::Raise => todo!("RaiseStatementNode::parse(tokens)"),
                _ => Ok(None),
            },
            _ => panic!("Expected a token"),
        }
    }

    fn parse_open_brace(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TestNode> {
        let pre = Self::parse_open_brace_no_dot(tokens)?;
        Self::parse_post(tokens, pre, ignore_newlines)
    }

    fn parse_post(
        tokens: &mut TokenList,
        pre: TestNode,
        ignore_newlines: bool,
    ) -> ParseResult<TestNode> {
        let value = Self::parse_post_braces(tokens, pre, ignore_newlines)?;
        DottedVariableNode::parse_post_dots(tokens, value, ignore_newlines)
    }

    pub(super) fn parse_post_braces(
        tokens: &mut TokenList,
        pre: TestNode,
        ignore_newlines: bool,
    ) -> ParseResult<TestNode> {
        if ignore_newlines {
            tokens.pass_newlines()?;
        }
        while let TokenType::OpenBrace(c) = tokens.token_type()? {
            match c {
                '(' => {
                    todo!("pre = FunctionCallNode::new(pre, ArgumentNode::parse_list(tokens)?)")
                }
                '[' => {
                    if tokens.brace_contains(&TokenType::Colon)? {
                        todo!("pre = IndexNode::new(pre, SliceNode::parse(tokens)?)")
                    } else {
                        todo!(
                            "pre = IndexNode::new(pre, LiteralNode::parse(tokens)?.get_builders()"
                        )
                    }
                }
                '{' => return ParseResult::Ok(pre),
                _ => panic!("Unknown brace type"),
            }
        }
        ParseResult::Ok(pre)
    }

    pub fn parse_maybe_post_if(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<(TestNode, Option<TestNode>)> {
        let pre_if = Self::parse_no_ternary(tokens, ignore_newlines)?;
        if !matches!(tokens.token_type()?, TokenType::Keyword(Keyword::If)) {
            return Ok((pre_if, Option::None));
        }
        tokens.next_tok(ignore_newlines)?;
        let post_if = Self::parse_newline(tokens, ignore_newlines)?;
        if parse_if_matches!(tokens, ignore_newlines, TokenType::Keyword(Keyword::Else))?.is_some()
        {
            let ternary = TernaryNode::new(
                pre_if,
                post_if,
                Self::parse_newline(tokens, ignore_newlines)?,
            );
            Ok((TestNode::Ternary(Box::new(ternary)), Option::None))
        } else {
            Ok((pre_if, Option::Some(post_if)))
        }
    }

    fn parse_open_brace_no_dot(tokens: &mut TokenList) -> ParseResult<TestNode> {
        match tokens.token_type()? {
            TokenType::OpenBrace(c) => match c {
                '(' => {
                    if tokens.brace_contains_kwd(Keyword::For)? {
                        ComprehensionNode::parse(tokens).map(TestNode::Comprehension)
                    } else if tokens.brace_contains_str(",")? || tokens.brace_is_empty()? {
                        LiteralNode::parse(tokens).map(TestNode::Literal)
                    } else {
                        tokens.next_token()?;
                        let contained = Self::parse_newline(tokens, true)?;
                        if !tokens.token_equals(")")? {
                            return Err(
                                tokens.error_with_first("Unmatched brace: ) does not match")
                            );
                        }
                        tokens.next_token()?;
                        Result::Ok(contained)
                    }
                }
                '[' => {
                    if tokens.brace_contains(&TokenType::Colon)? {
                        RangeLiteralNode::parse(tokens).map(TestNode::Range)
                    } else if tokens.brace_contains_kwd(Keyword::For)? {
                        ComprehensionNode::parse(tokens).map(TestNode::Comprehension)
                    } else {
                        LiteralNode::parse(tokens).map(TestNode::Literal)
                    }
                }
                '{' => {
                    if tokens.brace_contains_kwd(Keyword::For)? {
                        if tokens.brace_contains(&TokenType::Colon)? {
                            DictComprehensionNode::parse(tokens).map(TestNode::DictComp)
                        } else {
                            ComprehensionNode::parse(tokens).map(TestNode::Comprehension)
                        }
                    } else if tokens.brace_contains(&TokenType::Colon)? {
                        DictLiteralNode::parse(tokens).map(TestNode::DictLiteral)
                    } else {
                        LiteralNode::parse(tokens).map(TestNode::Literal)
                    }
                }
                x => panic!("Unknown brace found: {}", x),
            },
            _ => panic!(),
        }
    }

    pub fn next_is_test(tokens: &mut TokenList) -> ParseResult<bool> {
        Ok(matches!(
            tokens.token_type()?,
            TokenType::Name(_)
                | TokenType::OpenBrace(_)
                | TokenType::Operator(_)
                | TokenType::Number(_)
                | TokenType::OpFunc(_)
                | TokenType::Ellipsis
                | TokenType::String(_)
                | TokenType::Keyword(Keyword::Lambda | Keyword::Some | Keyword::Switch)
        ))
    }
}

#[derive(Debug)]
enum StackValue {
    Node(TestNode),
    Op(DummyOp),
}

#[derive(Debug)]
struct DummyOp {
    op: OperatorTypeNode,
    line_info: LineInfo,
}

impl DummyOp {
    pub fn new(op: OperatorTypeNode, line_info: LineInfo) -> DummyOp {
        DummyOp { op, line_info }
    }
}

fn convert_queue_to_node(queue: VecDeque<StackValue>) -> ParseResult<TestNode> {
    let mut temp = VecDeque::new();
    for t in queue {
        match t {
            StackValue::Op(t) => {
                let info = t.line_info;
                let op = t.op;
                let nodes;
                if op.is_unary() {
                    nodes = vec![temp.pop_front().unwrap()];
                } else {
                    let mut t = vec![temp.pop_front().unwrap(), temp.pop_front().unwrap()];
                    t.reverse();
                    nodes = t;
                }
                temp.push_front(TestNode::Operator(OperatorNode::from_nodes(
                    info, op, nodes,
                )))
            }
            StackValue::Node(t) => temp.push_front(t),
        }
    }
    let node = temp.pop_front().unwrap();
    if let Option::Some(x) = temp.pop_front() {
        Err(ParserError::Normal(ParserException::of("Invalid node", x)))
    } else {
        Ok(node)
    }
}

impl Lined for TestNode {
    fn line_info(&self) -> &LineInfo {
        todo!()
    }
}
