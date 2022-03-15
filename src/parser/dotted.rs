use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::number::{Number, NumberNode};
use crate::parser::operator_sp::SpecialOpNameNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct DottedVariableNode {
    line_info: LineInfo,
    pre_dot: Box<TestNode>,
    post_dots: Vec<DottedVar>,
}

#[derive(Debug)]
pub struct DottedVar {
    line_info: LineInfo,
    post_dot: NameNode,
    dot_prefix: DotPrefix,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DotPrefix {
    None,
    Question,
    DoubleBang,
}

impl DottedVariableNode {
    pub fn new(line_info: LineInfo, pre_dot: Box<TestNode>, post_dots: Vec<DottedVar>) -> Self {
        Self {
            line_info,
            pre_dot,
            post_dots,
        }
    }

    pub fn empty() -> Self {
        Self {
            line_info: LineInfo::empty(),
            pre_dot: Box::new(TestNode::empty()),
            post_dots: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.pre_dot.is_empty()
    }

    pub fn get_pre_dot(&self) -> &TestNode {
        &self.pre_dot
    }

    pub fn get_post_dots(&self) -> &[DottedVar] {
        &self.post_dots
    }

    pub fn get_last(&self) -> &DottedVar {
        self.post_dots.last().expect("Dot list should not be empty")
    }

    pub fn name_string(&self) -> String {
        todo!()
    }

    pub fn parse_names_only(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<DottedVariableNode> {
        let name = VariableNode::parse_newline(tokens, ignore_newlines)?;
        let mut post_dots = Vec::new();
        while let TokenType::Dot(_) = tokens.token_type()? {
            post_dots.push(DottedVar::parse(tokens, true, ignore_newlines)?);
        }
        Ok(DottedVariableNode::new(
            name.line_info().clone(),
            Box::new(TestNode::Name(NameNode::Variable(name))),
            post_dots,
        ))
    }

    pub fn parse_names_only_list(tokens: &mut TokenList) -> ParseResult<Vec<DottedVariableNode>> {
        let mut variables = Vec::new();
        let is_braced = if tokens.token_equals("(")?
            && !tokens.brace_contains_kwds([Keyword::In, Keyword::For])?
        {
            tokens.next_tok(true)?;
            true
        } else {
            false
        };
        while let TokenType::Name(_) = tokens.token_type()? {
            variables.push(Self::parse_names_only(tokens, is_braced)?);
            if let TokenType::Comma = tokens.token_type()? {
                tokens.next_tok(is_braced)?;
            } else {
                break;
            }
        }
        if is_braced {
            if !tokens.token_equals(")")? {
                return Err(tokens.error("Unmatched braces"));
            }
            tokens.next_token()?;
        }
        Ok(variables)
    }

    pub fn parse_post_dots(
        tokens: &mut TokenList,
        pre_dot: TestNode,
        ignore_newlines: bool,
    ) -> ParseResult<TestNode> {
        if matches!(tokens.token_type()?, TokenType::Dot(_)) {
            Self::from_expr(tokens, pre_dot, ignore_newlines)
                .map(NameNode::Dotted)
                .map(TestNode::Name)
        } else {
            Ok(pre_dot)
        }
    }

    pub fn parse_name_post_dots(
        tokens: &mut TokenList,
        pre_dot: NameNode,
        ignore_newlines: bool,
    ) -> ParseResult<NameNode> {
        if matches!(tokens.token_type()?, TokenType::Dot(_)) {
            Self::from_expr(tokens, TestNode::Name(pre_dot), ignore_newlines).map(NameNode::Dotted)
        } else {
            Ok(pre_dot)
        }
    }

    pub fn parse_on_name(tokens: &mut TokenList) -> ParseResult<DottedVariableNode> {
        if let TokenType::Name(_) = tokens.token_type()? {
            Self::parse_names_only(tokens, false)
        } else {
            Ok(Self::empty())
        }
    }

    pub fn from_expr(
        tokens: &mut TokenList,
        pre_dot: TestNode,
        ignore_newlines: bool,
    ) -> ParseResult<DottedVariableNode> {
        assert!(matches!(tokens.token_type()?, TokenType::Dot(_)));
        let post_dots = DottedVar::parse_all(tokens, ignore_newlines)?;
        Ok(DottedVariableNode::new(
            pre_dot.line_info().clone(),
            Box::new(pre_dot),
            post_dots,
        ))
    }
}

impl DottedVar {
    pub fn new(line_info: LineInfo, post_dot: NameNode, dot_prefix: DotPrefix) -> DottedVar {
        DottedVar {
            line_info,
            post_dot,
            dot_prefix,
        }
    }

    pub fn get_post_dot(&self) -> &NameNode {
        &self.post_dot
    }

    pub fn get_dot_prefix(&self) -> DotPrefix {
        self.dot_prefix
    }

    fn parse_all(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<Vec<DottedVar>> {
        let mut result = Vec::new();
        while let TokenType::Dot(_) = tokens.token_type()? {
            result.push(Self::parse(tokens, false, ignore_newlines)?)
        }
        Ok(result)
    }

    fn parse(
        tokens: &mut TokenList,
        names_only: bool,
        ignore_newlines: bool,
    ) -> ParseResult<DottedVar> {
        let (info, token) = tokens.next_tok(true)?.deconstruct();
        match token {
            TokenType::Dot(prefix) => {
                let post_dot = if let TokenType::OperatorSp(op) = tokens.token_type()? {
                    if names_only {
                        return Err(tokens.default_error());
                    }
                    let op = *op;
                    let line_info = tokens.next_tok(ignore_newlines)?.deconstruct().0;
                    NameNode::SpecialOp(SpecialOpNameNode::new(line_info, op))
                } else if let TokenType::Number(_) = tokens.token_type()? {
                    let number = NumberNode::parse(tokens)?;
                    let (line_info, value) = number.deconstruct();
                    match value {
                        Number::Integer(i) => {
                            NameNode::Variable(VariableNode::new(line_info, i.to_string()))
                        }
                        Number::Decimal(_) => {
                            return Err(ParserError::Normal(ParserException::of(
                                "Numbers as a post-dot must be whole integers",
                                line_info,
                            )))
                        }
                    }
                } else {
                    NameNode::Variable(VariableNode::parse(tokens)?)
                };
                let post_dot = if !names_only {
                    NameNode::parse_post_braces(tokens, post_dot, false)?
                } else {
                    post_dot
                };
                if ignore_newlines {
                    tokens.pass_newlines()?;
                }
                Ok(DottedVar::new(info, post_dot, prefix))
            }
            _ => panic!("Expected dot here"),
        }
    }
}

impl DotPrefix {
    pub fn is_empty(&self) -> bool {
        matches!(self, DotPrefix::None)
    }
}

impl Lined for DottedVariableNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for DottedVar {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> TryFrom<&'a NameNode> for &'a DottedVariableNode {
    type Error = ();

    fn try_from(value: &'a NameNode) -> Result<Self, Self::Error> {
        match value {
            NameNode::Dotted(d) => Ok(d),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a TestNode> for &'a DottedVariableNode {
    type Error = ();

    fn try_from(value: &'a TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Name(NameNode::Dotted(d)) => Ok(d),
            _ => Err(()),
        }
    }
}
