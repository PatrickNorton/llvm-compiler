use crate::parser::error::ParseResult;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

/// The class representing a container literal.
///
/// This is not used to represent dictionary literals due to their special
/// syntax, for that, use [`DictLiteralNode`].
///
/// # Syntax
///
/// ```text
/// OPEN_BRACE [[["*"] TestNode *("," ["*"] TestNode) [","]]] CLOSE_BRACE
/// ```
#[derive(Debug)]
pub struct LiteralNode {
    line_info: LineInfo,
    brace_type: char,
    builders: Vec<(String, TestNode)>,
}

/// The class representing a dictionary literal.
///
/// This class is separate from [`LiteralNode`] because it has a slightly
/// different syntax and nodal representation.
///
/// See also the difference between [`ComprehensionNode`] and
/// [`DictComprehensionNode`].
///
/// # Syntax
///
/// ```text
/// "{" TestNode ":" TestNode *("," TestNode ":" TestNode) [","] "}"
/// ```
///
/// Note that empty dict literals have a distinct syntax, which is:
///
/// ```text
/// "{" ":" "}"
/// ```
#[derive(Debug)]
pub struct DictLiteralNode {
    line_info: LineInfo,
    values: Vec<(TestNode, TestNode)>,
}

impl LiteralNode {
    pub fn new(line_info: LineInfo, brace_type: char, builders: Vec<(String, TestNode)>) -> Self {
        Self {
            line_info,
            brace_type,
            builders,
        }
    }

    /// Consumes the [`LiteralNode`] and returns the list of values within it.
    pub fn into_builders(self) -> Vec<(String, TestNode)> {
        self.builders
    }

    /// The type of brace used to define the node.
    ///
    /// # Definitions by brace type
    ///
    /// - `(`: Generator literal
    /// - `[`: List literal
    /// - `{`: Set literal
    pub fn get_brace_type(&self) -> char {
        self.brace_type
    }

    /// Returns a reference to the nodes & varargs in this node.
    pub fn get_builders(&self) -> &[(String, TestNode)] {
        &self.builders
    }

    /// Parses a [`LiteralNode`] from the given list of tokens.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<LiteralNode> {
        let (line_info, brace_type) = match tokens.next_tok(true)?.deconstruct() {
            (l, TokenType::OpenBrace(c)) => (l, c),
            _ => panic!("Expected an open brace"),
        };
        let matching_brace = TokenList::matching_brace(brace_type);
        let mut values = Vec::new();
        while !matches!(tokens.token_type()?, TokenType::CloseBrace(c) if c == &matching_brace) {
            if let TokenType::CloseBrace(_) = tokens.token_type()? {
                let err_str = format!(
                    "Unmatched brace: {} does not match {:?}",
                    matching_brace,
                    tokens.first()?
                );
                return Err(tokens.error(err_str));
            }
            let splat = if let Option::Some(tok) =
                tokens.next_if_ignoring(true, |x| x.equals("*") || x.equals("**"))?
            {
                tok.into_sequence()
            } else {
                String::new()
            };
            let value = TestNode::parse_newline(tokens, true)?;
            values.push((splat, value));
            if tokens.next_if_ignoring(true, |x| x.equals(","))?.is_none() {
                break;
            }
        }
        tokens.next_token()?;
        Ok(LiteralNode::new(line_info, brace_type, values))
    }
}

impl DictLiteralNode {
    pub fn new(line_info: LineInfo, values: Vec<(TestNode, TestNode)>) -> Self {
        Self { line_info, values }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn get_values(&self) -> &[(TestNode, TestNode)] {
        &self.values
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DictLiteralNode> {
        assert!(tokens.token_equals("{")?);
        let info = tokens.next_tok(true)?.deconstruct().0;
        let mut values = Vec::new();
        if parse_if_matches!(tokens, true, TokenType::Colon)?.is_some() {
            tokens.expect("}", false)?;
            return Ok(DictLiteralNode::new(info, Vec::new()));
        }
        loop {
            let key = if tokens.token_equals("**")? {
                tokens.next_tok(true)?;
                TestNode::empty()
            } else {
                let key = TestNode::parse_newline(tokens, true)?;
                if !tokens.token_equals(":")? {
                    return Err(tokens.error("Dict comprehension must have colon"));
                }
                tokens.next_tok(true)?;
                key
            };
            let value = TestNode::parse_newline(tokens, true)?;
            values.push((key, value));
            if !tokens.token_equals(",")? {
                if !tokens.token_equals("}")? {
                    return Err(tokens.error("Unmatched brace"));
                } else {
                    break;
                }
            }
            tokens.next_tok(true)?;
            if !tokens.token_equals("}")? {
                break;
            }
        }
        tokens.next_token()?;
        Ok(DictLiteralNode::new(info, values))
    }
}

impl Lined for LiteralNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for DictLiteralNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> TryFrom<&'a TestNode> for &'a LiteralNode {
    type Error = ();

    fn try_from(value: &'a TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Literal(l) => Ok(l),
            _ => Err(()),
        }
    }
}
