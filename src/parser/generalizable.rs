use crate::parser::base::IndependentNode;
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::Lined;
use crate::parser::method::MethodDefinitionNode;
use crate::parser::operator_def::OperatorDefinitionNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;

#[derive(Debug)]
pub enum GeneralizableNode {
    Function(FunctionDefinitionNode),
    Method(MethodDefinitionNode),
    Operator(OperatorDefinitionNode),
}

impl GeneralizableNode {
    pub fn add_generics(&mut self, generics: Vec<TypeNode>) {
        match self {
            GeneralizableNode::Function(f) => f.add_generics(generics),
            GeneralizableNode::Method(m) => m.add_generics(generics),
            GeneralizableNode::Operator(o) => o.add_generics(generics),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<GeneralizableNode> {
        let (_, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Generic)));
        let types = TypeNode::parse_list(tokens, false)?;
        tokens.pass_newlines()?;
        match GeneralizableNode::try_from(IndependentNode::parse(tokens)?) {
            Result::Ok(mut node) => {
                node.add_generics(types);
                Ok(node)
            }
            Result::Err(generalized) => Err(ParserException::of(
                "Attempted to generalize non-generalizable node",
                generalized,
            )
            .into()),
        }
    }
}

impl TryFrom<IndependentNode> for GeneralizableNode {
    type Error = IndependentNode;

    fn try_from(value: IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::FunctionDef(f) => Ok(GeneralizableNode::Function(f)),
            IndependentNode::Method(m) => Ok(GeneralizableNode::Method(m)),
            IndependentNode::OpDef(o) => Ok(GeneralizableNode::Operator(o)),
            val => Err(val),
        }
    }
}

impl From<GeneralizableNode> for IndependentNode {
    fn from(node: GeneralizableNode) -> Self {
        match node {
            GeneralizableNode::Function(f) => IndependentNode::FunctionDef(f),
            GeneralizableNode::Method(m) => IndependentNode::Method(m),
            GeneralizableNode::Operator(o) => IndependentNode::OpDef(o),
        }
    }
}

impl Lined for GeneralizableNode {
    fn line_info(&self) -> &super::line_info::LineInfo {
        match self {
            GeneralizableNode::Function(f) => f.line_info(),
            GeneralizableNode::Method(m) => m.line_info(),
            GeneralizableNode::Operator(o) => o.line_info(),
        }
    }
}
