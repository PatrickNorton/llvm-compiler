use crate::parser::comprehension::{ComprehensionNode, DictComprehensionNode};
use crate::parser::literal::{DictLiteralNode, LiteralNode};
use crate::parser::name::NameNode;
use crate::parser::range::RangeLiteralNode;
use crate::parser::string_like::StringLikeNode;
use crate::parser::test_node::TestNode;
use std::convert::TryFrom;

#[derive(Debug)]
pub enum PostDottableNode {
    Comprehension(ComprehensionNode),
    DictComp(DictComprehensionNode),
    DictLiteral(DictLiteralNode),
    Literal(LiteralNode),
    Name(NameNode),
    RangeLiteral(RangeLiteralNode),
    String(StringLikeNode),
}

impl TryFrom<TestNode> for PostDottableNode {
    type Error = TestNode;

    fn try_from(value: TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Comprehension(c) => Result::Ok(PostDottableNode::Comprehension(c)),
            TestNode::DictComp(d) => Result::Ok(PostDottableNode::DictComp(d)),
            TestNode::DictLiteral(l) => Result::Ok(PostDottableNode::DictLiteral(l)),
            TestNode::Literal(l) => Result::Ok(PostDottableNode::Literal(l)),
            TestNode::Formatted(f) => {
                Result::Ok(PostDottableNode::String(StringLikeNode::Formatted(f)))
            }
            TestNode::Name(n) => Result::Ok(PostDottableNode::Name(n)),
            TestNode::Range(r) => Result::Ok(PostDottableNode::RangeLiteral(r)),
            TestNode::String(s) => {
                Result::Ok(PostDottableNode::String(StringLikeNode::Standard(s)))
            }
            x => Result::Err(x),
        }
    }
}

impl From<PostDottableNode> for TestNode {
    fn from(x: PostDottableNode) -> Self {
        match x {
            PostDottableNode::Comprehension(c) => TestNode::Comprehension(c),
            PostDottableNode::DictComp(d) => TestNode::DictComp(d),
            PostDottableNode::DictLiteral(d) => TestNode::DictLiteral(d),
            PostDottableNode::Literal(l) => TestNode::Literal(l),
            PostDottableNode::RangeLiteral(s) => TestNode::Range(s),
            PostDottableNode::String(s) => match s {
                StringLikeNode::Standard(s) => TestNode::String(s),
                StringLikeNode::Formatted(f) => TestNode::Formatted(f),
            },
            PostDottableNode::Name(n) => TestNode::Name(n),
        }
    }
}
