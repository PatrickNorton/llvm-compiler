use crate::parser::error::ParseResult;
use crate::parser::line_info::LineInfo;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct VariableNode {
    line_info: LineInfo,
    name: String,
}

impl VariableNode {
    pub const fn new(line_info: LineInfo, name: String) -> VariableNode {
        VariableNode { line_info, name }
    }

    pub fn empty() -> Self {
        Self {
            line_info: LineInfo::empty(),
            name: String::new(),
        }
    }

    pub fn parse_on_name(tokens: &mut TokenList) -> ParseResult<VariableNode> {
        if matches!(tokens.token_type()?, TokenType::Name(_)) {
            Self::parse(tokens)
        } else {
            ParseResult::Ok(Self::empty())
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<VariableNode> {
        let (token_type, line_info) = tokens.next_token()?.deconstruct();
        match token_type {
            TokenType::Name(string) => ParseResult::Ok(VariableNode::new(line_info, string)),
            _ => ParseResult::Err(tokens.error_expected("name")),
        }
    }

    pub fn parse_ellipsis(tokens: &mut TokenList) -> ParseResult<VariableNode> {
        let next = tokens.next_token()?;
        assert!(matches!(next.token_type(), TokenType::Ellipsis));
        let info = next.deconstruct().1;
        ParseResult::Ok(VariableNode::new(info, "...".to_string()))
    }
}
