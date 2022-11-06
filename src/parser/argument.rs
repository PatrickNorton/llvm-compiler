use crate::parser::comprehension::ComprehensionNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

/// An argument to a function call (or similar).
///
/// # Grammar
/// ```text
/// ["*"|"**"] [VariableNode "="] TestNode
/// ```
#[derive(Debug)]
pub struct ArgumentNode {
    line_info: LineInfo,
    variable: VariableNode,
    // TODO? Make into an enum
    vararg: String,
    argument: TestNode,
}

impl ArgumentNode {
    /// Creates a simple argument from an expression.
    pub fn from_test_node(node: TestNode) -> ArgumentNode {
        Self::new(node.line_info().clone(), node)
    }

    /// Creates an argument from an expression with the given [`LineInfo`].
    pub fn new(line_info: LineInfo, node: TestNode) -> ArgumentNode {
        Self::with_variable(line_info, VariableNode::empty(), String::new(), node)
    }

    /// Creates an argument with a variable and vararg.
    ///
    /// The valid vararg values are `""`, `"*"`, and `"**"`.
    pub fn from_vararg(variable: VariableNode, vararg: String, argument: TestNode) -> ArgumentNode {
        Self::with_variable(variable.line_info().clone(), variable, vararg, argument)
    }

    /// Creates an argument from its component parts.
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

    /// Takes the argument expression from `self` by value.
    ///
    /// This is the by-value equivalent of [`Self::get_argument`].
    pub fn into_argument(self) -> TestNode {
        self.argument
    }

    /// The argument keyword.
    ///
    /// If the argument has no keyword, this method returns
    /// [`VariableNode::empty`].
    pub fn get_variable(&self) -> &VariableNode {
        &self.variable
    }

    /// The argument value.
    ///
    /// If you want this by value, use [`Self::into_argument`].
    pub fn get_argument(&self) -> &TestNode {
        &self.argument
    }

    /// The vararg associated with the argument.
    ///
    /// This can only return three values:
    /// * `""`: The argument has no vararg
    /// * `"*"`: A single-star (iterator-based) vararg
    /// * `"**"`: A double-star (dictionary) vararg
    pub fn get_vararg(&self) -> &str {
        &self.vararg
    }

    /// If the argument has a vararg.
    ///
    /// This is true if and only if [`self.get_vararg()`](Self::get_vararg)
    /// is not empty.
    pub fn is_vararg(&self) -> bool {
        !self.vararg.is_empty()
    }

    /// Parses a list of arguments from the [`TokenList`].
    ///
    /// This is intended for use in parsing argument lists for functions. As
    /// such, it contains checks for opening and closing braces. For parsing a
    /// list without braces, use [`Self::parse_brace_free_list`].
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

    /// Parses a list of arguments from the [`TokenList`] without parsing
    /// opening or closing braces.
    ///
    /// The equivalent of this method that *does* check braces is
    /// [`Self::parse_list`].
    pub fn parse_brace_free_list(tokens: &mut TokenList) -> ParseResult<Vec<ArgumentNode>> {
        let mut args = Vec::new();
        while tokens.token_eq_either("*", "**")? || TestNode::next_is_test(tokens)? {
            let offset = usize::from(tokens.token_eq_either("*", "**")?);
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

    /// Creates a list of [`ArgumentNodes`](ArgumentNode) from a list of
    /// [`TestNodes`](TestNode).
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

impl Lined for ArgumentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
