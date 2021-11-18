use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::line_info::LineInfo;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct StatementBodyNode {
    line_info: LineInfo,
    statements: Vec<IndependentNode>,
}

impl StatementBodyNode {
    pub fn new(line_info: LineInfo, statements: Vec<IndependentNode>) -> Self {
        Self {
            line_info,
            statements,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<StatementBodyNode> {
        if !tokens.token_equals("{")? {
            return Err(tokens.error("The body of a function must be enclosed in curly brackets"));
        }
        let (line_info, _) = tokens.next_tok(true)?.deconstruct();
        let st = Self::parse_until_token(line_info, tokens, ["{"])?;
        assert!(tokens.token_equals("}")?);
        tokens.next_token()?;
        Ok(st)
    }
    //     }

    fn parse_until_token<const N: usize>(
        line_info: LineInfo,
        tokens: &mut TokenList,
        values: [&'static str; N],
    ) -> ParseResult<StatementBodyNode> {
        let mut statements = Vec::new();
        while !values.contains(&tokens.first()?.get_sequence()) {
            statements.push(IndependentNode::parse(tokens)?);
            if !values.contains(&tokens.first()?.get_sequence()) {
                tokens.expect_newline()?;
            }
        }
        Ok(StatementBodyNode::new(line_info, statements))
    }
}
