use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::string_like::{StringLikeNode, StringPrefix};
use std::collections::HashSet;

#[derive(Debug)]
pub struct StringNode {
    line_info: LineInfo,
    prefixes: HashSet<StringPrefix>,
    contents: String,
}

impl StringNode {
    pub fn new(
        line_info: LineInfo,
        prefixes: HashSet<StringPrefix>,
        contents: String,
    ) -> StringNode {
        StringNode {
            line_info,
            prefixes,
            contents,
        }
    }

    pub fn get_prefixes(&self) -> &HashSet<StringPrefix> {
        &self.prefixes
    }

    pub fn get_contents(&self) -> &str {
        &self.contents
    }

    pub fn parse(text: String, info: LineInfo) -> ParseResult<StringNode> {
        let prefixes = StringLikeNode::get_prefixes(&text)?;
        let contents = StringLikeNode::get_contents(&text);
        assert!(!prefixes.contains(&StringPrefix::Formatted));
        if !prefixes.contains(&StringPrefix::Raw) {
            let contents = StringLikeNode::process_escapes(contents, &info)?;
            Ok(StringNode::new(info, prefixes, contents))
        } else {
            Ok(StringNode::new(info, prefixes, contents.to_string()))
        }
    }
}

impl Lined for StringNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
