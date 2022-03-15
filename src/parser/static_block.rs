use std::collections::HashSet;

use once_cell::sync::Lazy;

use crate::parser::descriptor::{DescriptorNode, STATIC_BLOCK_VALID};
use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct StaticBlockNode {
    line_info: LineInfo,
    body: StatementBodyNode,
}

impl StaticBlockNode {
    pub fn new(line_info: LineInfo, body: StatementBodyNode) -> StaticBlockNode {
        Self { line_info, body }
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &STATIC_BLOCK_VALID
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        assert!(descriptors.is_empty());
    }

    pub fn get_descriptors(&self) -> &HashSet<DescriptorNode> {
        static EMPTY: Lazy<HashSet<DescriptorNode>> = Lazy::new(HashSet::new);
        &EMPTY
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<StaticBlockNode> {
        assert!(tokens.token_equals("static")? && tokens.get_token(1)?.equals("{"));
        let (line_info, _) = tokens.next_token()?.deconstruct();
        Ok(StaticBlockNode::new(
            line_info,
            StatementBodyNode::parse(tokens)?,
        ))
    }
}

impl Lined for StaticBlockNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
