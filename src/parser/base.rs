use crate::parser::error::ParseResult;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub enum IndependentNode {
    Test(TestNode),
}

impl IndependentNode {
    pub fn parse(tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        tokens.pass_newlines()?;
        match tokens.token_type()? {
            TokenType::Keyword(k) => k.parse_left(tokens),
            TokenType::Descriptor(_) => todo!("DescribableNode::parse(tokens)"),
            TokenType::OpenBrace(_) | TokenType::Name(_) => Self::parse_left_variable(tokens),
            TokenType::CloseBrace(_) => Err(tokens.error("Unmatched close brace")),
            TokenType::AugAssign(_) => Err(tokens.error("Unexpected operator")),
            TokenType::Operator(_)
            | TokenType::String(_)
            | TokenType::Number
            | TokenType::OpFunc(_) => Ok(IndependentNode::Test(TestNode::parse(tokens)?)),
            TokenType::Assign(_) => Err(tokens.error("Unexpected assignment")),
            TokenType::OperatorSp(_) => {
                if matches!(tokens.token_type_at(1)?, TokenType::Assign(_)) {
                    todo!("SpecialOpAssignmentNode::parse(tokens)")
                } else {
                    todo!("OperatorDefinitionNode::parse(tokens)")
                }
            }
            TokenType::Ellipsis => VariableNode::parse_ellipsis(tokens)
                .map(TestNode::Variable)
                .map(IndependentNode::Test),
            TokenType::At => todo!("DecoratableNode::parse_left_decorator(tokens)"),
            TokenType::Dollar => todo!("AnnotatableNode::parse_left_annotation(tokens)"),
            TokenType::Epsilon => Err(tokens.error("Unexpected EOF")),
            TokenType::Arrow
            | TokenType::DoubleArrow
            | TokenType::Increment(_)
            | TokenType::Colon
            | TokenType::Dot
            | TokenType::Comma => Err(tokens.default_error()),
            _ => Err(tokens.error("Nonexistent token found")),
        }
    }

    fn parse_left_variable(tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        assert!(matches!(
            tokens.token_type()?,
            TokenType::Name(_) | TokenType::OpenBrace(_)
        ));
        let var_size = tokens.size_of_variable()?;
        if is_assignment(tokens)? || is_normal_assignment(tokens)? {
            todo!("return AssignStatementNode::parse(tokens)")
        }
        let after_var = tokens.get_token(var_size)?;
        if matches!(after_var.token_type(), TokenType::AugAssign(_)) {
            todo!("AugmentedAssignmentNode::parse(tokens)")
        } else if matches!(after_var.token_type(), TokenType::Increment(_)) {
            todo!("SimpleStatementNode::parse_inc_dec(tokens)")
        } else if is_declaration(tokens)? {
            todo!("DeclarationNode::parse(tokens)")
        } else {
            TestNode::parse(tokens).map(IndependentNode::Test)
        }
    }
}

fn is_assignment(tokens: &mut TokenList) -> ParseResult<bool> {
    todo!()
}

fn is_normal_assignment(tokens: &mut TokenList) -> ParseResult<bool> {
    todo!()
}

fn is_declaration(tokens: &mut TokenList) -> ParseResult<bool> {
    todo!()
}
