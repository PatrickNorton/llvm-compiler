use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::LineInfo;
use crate::parser::operator::{OperatorNode, OperatorTypeNode};
use crate::parser::string::StringNode;
use crate::parser::string_like::StringLikeNode;
use crate::parser::ternary::TernaryNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;
use std::collections::VecDeque;

#[derive(Debug)]
pub enum TestNode {
    Operator(OperatorNode),
    OperatorType(OperatorTypeNode),
    String(StringNode),
    Ternary(Box<TernaryNode>),
    Variable(VariableNode),
}

impl TestNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<TestNode> {
        Self::parse_newline(tokens, false)
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
        todo!()
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
            todo!()
        }
        convert_queue_to_node(queue)
    }

    // private static TestNode parseNode(@NotNull TokenList tokens, boolean ignoreNewlines, boolean parseCurly) {
    //         TestNode node = parseInternalNode(tokens, ignoreNewlines, parseCurly);
    //         if (node instanceof PostDottableNode) {
    //             return parsePost(tokens, node, ignoreNewlines);
    //         } else {
    //             return node;
    //         }
    //     }

    fn parse_node(
        tokens: &mut TokenList,
        ignore_newlines: bool,
        parse_curly: bool,
    ) -> ParseResult<Option<TestNode>> {
        let node = Self::parse_internal_node(tokens, ignore_newlines, parse_curly)?;
        todo!()
    }

    fn parse_internal_node(
        tokens: &mut TokenList,
        ignore_newlines: bool,
        parse_curly: bool,
    ) -> ParseResult<Option<TestNode>> {
        if ignore_newlines {
            tokens.pass_newlines()?;
        }
        match tokens.first()?.token_type() {
            TokenType::OpenBrace(c) => {
                if parse_curly || *c != '{' {
                    Self::parse_open_brace(tokens, ignore_newlines).map(Option::Some)
                } else {
                    ParseResult::Ok(Option::None)
                }
            }
            TokenType::Name(_) => todo!("NameNode::parse(tokens, ignore_newlines)"),
            TokenType::Number => todo!("NumberNode::parse(tokens)"),
            TokenType::Operator(_) => todo!("OperatorTypeNode::parse(tokens)"),
            TokenType::OpFunc(_) => todo!("EscapedOperatorNode::parse(tokens, ignore_newlines)"),
            TokenType::Newline => {
                if ignore_newlines {
                    panic!("Illegal place for newline")
                } else {
                    ParseResult::Ok(Option::None)
                }
            }
            TokenType::String(_) => StringLikeNode::parse(tokens).map(|x| Some(x.into())),
            TokenType::Keyword(_) => todo!("Self::parse_keyword_node(tokens, ignore_newlines)"),
            _ => ParseResult::Ok(Option::None),
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
        let value = Self::parse_post_braces(tokens, pre, ignore_newlines);
        todo!("DottedVariableNode::parse_post_dots(tokens, value, ignore_newlines)")
    }

    fn parse_post_braces(
        tokens: &mut TokenList,
        pre: TestNode,
        ignore_newlines: bool,
    ) -> ParseResult<TestNode> {
        if ignore_newlines {
            tokens.pass_newlines()?;
        }
        while let TokenType::OpenBrace(c) = tokens.first()?.token_type() {
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

    fn parse_open_brace_no_dot(tokens: &mut TokenList) -> ParseResult<TestNode> {
        match tokens.first()?.token_type() {
            TokenType::OpenBrace(c) => match c {
                '(' => {
                    if tokens.brace_contains_kwd(Keyword::For)? {
                        todo!("ComprehensionNode::parse(tokens)")
                    } else if tokens.brace_contains_str(",")? || tokens.brace_is_empty()? {
                        todo!("LiteralNode::parse(tokens)")
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
                        todo!("RangeLiteralNode::parse(tokens)")
                    } else if tokens.brace_contains_kwd(Keyword::For)? {
                        todo!("ComprehensionNode::parse(tokens)")
                    } else {
                        todo!("LiteralNode::parse(tokens)")
                    }
                }
                '{' => {
                    if tokens.brace_contains_kwd(Keyword::For)? {
                        todo!("ComprehensionLikeNode::parse(tokens)")
                    } else if tokens.brace_contains(&TokenType::Colon)? {
                        todo!("DictLiteralNode::parse(tokens)")
                    } else {
                        todo!("LiteralNode::parse(tokens)")
                    }
                }
                x => panic!("Unknown brace found: {}", x),
            },
            _ => panic!(),
        }
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
    if !temp.is_empty() {
        todo!("Panic on invalid node")
    }
    Ok(node)
}
