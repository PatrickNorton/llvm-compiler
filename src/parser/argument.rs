use crate::parser::comprehension::ComprehensionNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct ArgumentNode {
    line_info: LineInfo,
    variable: VariableNode,
    vararg: String,
    argument: TestNode,
}

impl ArgumentNode {
    pub fn from_test_node(node: TestNode) -> ArgumentNode {
        Self::new(node.line_info().clone(), node)
    }

    pub fn new(line_info: LineInfo, node: TestNode) -> ArgumentNode {
        Self::with_variable(line_info, VariableNode::empty(), String::new(), node)
    }

    pub fn from_vararg(variable: VariableNode, vararg: String, argument: TestNode) -> ArgumentNode {
        Self::with_variable(variable.line_info().clone(), variable, vararg, argument)
    }

    pub fn with_variable(
        line_info: LineInfo,
        variable: VariableNode,
        vararg: String,
        argument: TestNode,
    ) -> ArgumentNode {
        ArgumentNode {
            line_info,
            variable,
            vararg,
            argument,
        }
    }

    pub fn into_argument(self) -> TestNode {
        self.argument
    }

    pub fn parse_list(tokens: &mut TokenList) -> ParseResult<Vec<ArgumentNode>> {
        if !tokens.token_equals("(")? {
            return Err(tokens.error("Function call must start with open-paren"));
        }
        if tokens.brace_contains_kwd(Keyword::For)? {
            let comp = TestNode::Comprehension(ComprehensionNode::parse(tokens)?);
            return Ok(vec![ArgumentNode::from_test_node(comp)]);
        }
        tokens.next_tok(true)?;
        if tokens.next_if_equals(")")?.is_some() {
            return Ok(vec![]);
        }
        let args = Self::parse_brace_free_list(tokens)?;
        tokens.expect(")", false)?;
        Ok(args)
    }

    pub fn parse_brace_free_list(tokens: &mut TokenList) -> ParseResult<Vec<ArgumentNode>> {
        let mut args = Vec::new();
        while tokens.token_eq_either("*", "**")? || TestNode::next_is_test(tokens)? {
            let offset = if tokens.token_eq_either("*", "**")? {
                1
            } else {
                0
            };
            let var;
            if matches!(tokens.get_token(offset)?.token_type(), TokenType::Name(_))
                && after_var_is_eq(tokens, offset)?
            {
                var = VariableNode::parse(tokens)?;
                tokens.next_tok(true)?;
            } else {
                var = VariableNode::empty();
            }
            let vararg = if tokens.token_eq_either("*", "**")? {
                tokens.next_tok(true)?.into_sequence()
            } else {
                String::new()
            };
            let argument = TestNode::parse_newline(tokens, true)?;
            args.push(ArgumentNode::from_vararg(var, vararg, argument));
            if !tokens.token_equals(",")? {
                break;
            }
            tokens.next_tok(true)?;
        }
        Ok(args)
    }

    pub fn from_test_nodes(nodes: Vec<TestNode>) -> Vec<ArgumentNode> {
        nodes
            .into_iter()
            .map(ArgumentNode::from_test_node)
            .collect()
    }
}

#[inline]
fn after_var_is_eq(tokens: &mut TokenList, offset: usize) -> ParseResult<bool> {
    let var_size = tokens.size_of_variable_at(offset)?;
    Ok(tokens.get_token(var_size)?.equals("="))
}
