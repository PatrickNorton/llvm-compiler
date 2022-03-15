use std::iter::Map;
use std::ops::Index;

use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::typed_arg::VarargType;

#[derive(Debug)]
pub struct TestListNode {
    line_info: LineInfo,
    values: Vec<(VarargType, TestNode)>,
}

impl TestListNode {
    pub fn empty() -> TestListNode {
        Self::new(LineInfo::empty(), vec![])
    }

    pub fn new(line_info: LineInfo, values: Vec<(VarargType, TestNode)>) -> TestListNode {
        Self { line_info, values }
    }

    pub fn from_one(value: TestNode) -> TestListNode {
        Self {
            line_info: value.line_info().clone(),
            values: vec![(VarargType::None, value)],
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn get_vararg(&self, index: usize) -> Option<&VarargType> {
        self.values.get(index).map(|(x, _)| x)
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn pairs(&self) -> impl Iterator<Item = &'_ (VarargType, TestNode)> {
        self.values.iter()
    }

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TestListNode> {
        if !ignore_newlines && matches!(tokens.token_type()?, TokenType::Newline) {
            return Ok(TestListNode::empty());
        }
        let mut values = Vec::new();
        while TestNode::next_is_test(tokens)? {
            let vararg = VarargType::parse_ignoring(tokens, ignore_newlines)?;
            let test = TestNode::parse_newline(tokens, ignore_newlines)?;
            values.push((vararg, test));
            if !matches!(tokens.token_type()?, TokenType::Comma) {
                break;
            }
            tokens.next_tok(ignore_newlines)?;
        }
        Ok(TestListNode::new(LineInfo::empty(), values))
    }

    pub fn parse_post_if(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<(TestListNode, Option<TestNode>)> {
        if !ignore_newlines && matches!(tokens.token_type()?, TokenType::Newline) {
            return Ok((TestListNode::empty(), Option::None));
        }
        let mut values = Vec::new();
        let mut post_if = Option::None;
        while TestNode::next_is_test(tokens)?
            || matches!(tokens.token_type()?, TokenType::Keyword(Keyword::If))
        {
            if parse_if_matches!(tokens, ignore_newlines, TokenType::Keyword(Keyword::If))?
                .is_some()
            {
                post_if = Option::Some(TestNode::parse_newline(tokens, ignore_newlines)?);
                break;
            }
            let vararg = VarargType::parse_ignoring(tokens, ignore_newlines)?;
            let (next, cond) = TestNode::parse_maybe_post_if(tokens, ignore_newlines)?;
            values.push((vararg, next));
            if let Option::Some(cond) = cond {
                post_if = Option::Some(cond);
                break;
            }
            if let TokenType::Comma = tokens.token_type()? {
                tokens.next_tok(ignore_newlines)?;
            } else {
                break;
            }
        }
        let node = TestListNode::new(LineInfo::empty(), values);
        Ok((node, post_if))
    }
}

impl Index<usize> for TestListNode {
    type Output = TestNode;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values.index(index).1
    }
}

type SliceIter<'a> = <&'a [(VarargType, TestNode)] as IntoIterator>::IntoIter;
type SliceFn<'a> = fn(&'a (VarargType, TestNode)) -> &'a TestNode;

impl<'a> IntoIterator for &'a TestListNode {
    type Item = &'a TestNode;

    type IntoIter = Map<SliceIter<'a>, SliceFn<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter().map(|x| &x.1)
    }
}

impl Lined for TestListNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
