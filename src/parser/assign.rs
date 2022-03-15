use std::collections::HashSet;

use crate::parser::aug_assign::AugmentedAssignmentNode;
use crate::parser::base::IndependentNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::dotted::DottedVariableNode;
use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::name::NameNode;
use crate::parser::number::NumberNode;
use crate::parser::string_like::StringLikeNode;
use crate::parser::test_list::TestListNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub enum AssignStatementNode {
    Augmented(AugmentedAssignmentNode),
    Declared(DeclaredAssignmentNode),
    Normal(AssignmentNode),
}

#[derive(Debug)]
pub enum AssignableNode {
    Name(NameNode),
}

#[derive(Debug)]
pub struct AssignmentNode {
    is_colon: bool,
    name: Vec<AssignableNode>,
    value: TestListNode,
    line_info: LineInfo,
    mutability: Option<DescriptorNode>,
}

impl AssignStatementNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssignStatementNode> {
        if let TokenType::Keyword(Keyword::Var) = tokens.token_type()? {
            DeclaredAssignmentNode::parse(tokens).map(AssignStatementNode::Declared)
        } else if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            AssignmentNode::parse(tokens).map(AssignStatementNode::Normal)
        } else {
            let var_size = tokens.size_of_variable()?;
            if let TokenType::Assign(_) | TokenType::Comma = tokens.token_type_at(var_size)? {
                AssignmentNode::parse(tokens).map(AssignStatementNode::Normal)
            } else {
                DeclaredAssignmentNode::parse(tokens).map(AssignStatementNode::Declared)
            }
        }
    }
}

impl AssignableNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssignableNode> {
        assert!(tokens.line_contains(|x| matches!(x.token_type(), TokenType::Assign(_)))?);
        match tokens.token_type()? {
            TokenType::Name(_) => NameNode::parse(tokens).map(AssignableNode::Name),
            TokenType::Number(_) => {
                if let TokenType::Dot(_) = tokens.token_type_at(1)? {
                    let num = NumberNode::parse(tokens)?;
                    DottedVariableNode::from_expr(tokens, TestNode::Number(num), false)
                        .map(NameNode::Dotted)
                        .map(AssignableNode::Name)
                } else {
                    Err(tokens.error("Cannot assign to numeric literal"))
                }
            }
            TokenType::String(_) => {
                if let TokenType::Dot(_) = tokens.token_type_at(1)? {
                    let string = StringLikeNode::parse(tokens)?;
                    DottedVariableNode::from_expr(tokens, string.into(), false)
                        .map(NameNode::Dotted)
                        .map(AssignableNode::Name)
                } else {
                    Err(tokens.error("Cannot assign to string literal"))
                }
            }
            TokenType::OpenBrace(_) => {
                let t = TestNode::parse_open_brace(tokens, false)?;
                if let TokenType::Dot(_) = tokens.token_type()? {
                    DottedVariableNode::from_expr(tokens, t, false)
                        .map(NameNode::Dotted)
                        .map(AssignableNode::Name)
                } else {
                    match t.try_into() {
                        Result::Ok(t) => Ok(t),
                        Err(t) => Err(ParserError::Normal(ParserException::of(
                            "Cannot assign to node",
                            t,
                        ))),
                    }
                }
            }
            _ => Err(tokens.error_with_first("Un-assignable value")),
        }
    }
}

impl AssignmentNode {
    pub fn new(
        is_colon: bool,
        name: Vec<AssignableNode>,
        value: TestListNode,
        line_info: LineInfo,
        mutability: Option<DescriptorNode>,
    ) -> Self {
        Self {
            is_colon,
            name,
            value,
            line_info,
            mutability,
        }
    }

    pub fn is_colon(&self) -> bool {
        self.is_colon
    }

    pub fn get_names(&self) -> &[AssignableNode] {
        &self.name
    }

    pub fn get_values(&self) -> &TestListNode {
        &self.value
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        todo!()
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        debug_assert!(descriptors.len() == 1 && self.mutability.is_none());
        self.mutability = descriptors.into_iter().next();
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<AssignmentNode> {
        let mut names = Vec::new();
        while !matches!(tokens.token_type()?, TokenType::Assign(_)) {
            names.push(AssignableNode::parse(tokens)?);
            if !matches!(tokens.token_type()?, TokenType::Comma) {
                break;
            }
            tokens.next_token()?;
        }
        let is_colon = tokens.token_equals(":=")?;
        tokens.next_token()?;
        let value = TestListNode::parse(tokens, false)?;
        let line_info = names[0].line_info().clone();
        Ok(AssignmentNode::new(
            is_colon,
            names,
            value,
            line_info,
            Option::None,
        ))
    }
}

impl TryFrom<TestNode> for AssignableNode {
    type Error = TestNode;

    fn try_from(value: TestNode) -> Result<Self, Self::Error> {
        match value {
            TestNode::Name(n) => Ok(AssignableNode::Name(n)),
            x => Err(x),
        }
    }
}

impl From<AssignableNode> for TestNode {
    fn from(node: AssignableNode) -> Self {
        match node {
            AssignableNode::Name(n) => TestNode::Name(n),
        }
    }
}

impl From<AssignStatementNode> for IndependentNode {
    fn from(node: AssignStatementNode) -> Self {
        match node {
            AssignStatementNode::Augmented(a) => IndependentNode::AugAssign(a),
            AssignStatementNode::Declared(d) => IndependentNode::DeclaredAssign(d),
            AssignStatementNode::Normal(n) => IndependentNode::Assign(n),
        }
    }
}

impl Lined for AssignableNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            AssignableNode::Name(name) => name.line_info(),
        }
    }
}

impl Lined for AssignmentNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
