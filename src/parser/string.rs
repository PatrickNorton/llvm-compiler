use crate::parser::error::ParseResult;
use crate::parser::line_info::LineInfo;
use crate::parser::string_like::{StringLikeNode, StringPrefix};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
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

    pub fn parse(tokens: &mut TokenList) -> ParseResult<StringNode> {
        let (token, info) = tokens.next_token()?.deconstruct();
        match token {
            TokenType::String(text) => {
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
            _ => panic!("Expected a token"),
        }
    }
}
