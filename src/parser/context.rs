use std::collections::HashSet;

use crate::parser::argument::ArgumentNode;
use crate::parser::base::IndependentNode;
use crate::parser::class_def::{ClassBodyNode, ClassStatementNode};
use crate::parser::descriptor::{DescriptorNode, CONTEXT_VALID};
use crate::parser::error::{ParseResult, ParserException};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::name::NameNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::typed_arg::TypedArgumentListNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct ContextDefinitionNode {
    line_info: LineInfo,
    name: VariableNode,
    args: TypedArgumentListNode,
    enter: StatementBodyNode,
    exit: StatementBodyNode,
    exit_args: Vec<ArgumentNode>,
    others: ClassBodyNode,
    descriptors: HashSet<DescriptorNode>,
    annotations: Vec<NameNode>,
    decorators: Vec<NameNode>,
}

impl ContextDefinitionNode {
    pub fn new(
        line_info: LineInfo,
        name: VariableNode,
        args: TypedArgumentListNode,
        enter: StatementBodyNode,
        exit: StatementBodyNode,
        exit_args: Vec<ArgumentNode>,
        others: ClassBodyNode,
    ) -> Self {
        Self {
            line_info,
            name,
            args,
            enter,
            exit,
            exit_args,
            others,
            descriptors: HashSet::new(),
            annotations: Vec::new(),
            decorators: Vec::new(),
        }
    }

    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        &CONTEXT_VALID
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        self.descriptors = descriptors;
    }

    pub fn get_annotations(&self) -> &Vec<NameNode> {
        &self.annotations
    }

    pub fn add_annotations(&mut self, annotations: Vec<NameNode>) {
        self.annotations = annotations;
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ContextDefinitionNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Context)));
        let name = VariableNode::parse_on_name(tokens)?;
        let args = TypedArgumentListNode::parse_on_open_brace(tokens)?;
        if tokens.next_if_equals("{")?.is_none() {
            return Err(tokens.error("Context managers must be followed by a curly brace"));
        }
        let mut enter = Option::None;
        let mut exit = Option::None;
        let mut exit_args = Vec::new();
        let mut others = Vec::new();
        while !tokens.token_equals("}")? {
            if parse_if_matches!(tokens, TokenType::Keyword(Keyword::Enter))?.is_some() {
                if enter.is_none() {
                    enter = Some(StatementBodyNode::parse(tokens)?)
                } else {
                    return Err(tokens.error("Cannot have multiple definitions of enter"));
                }
            } else if parse_if_matches!(tokens, TokenType::Keyword(Keyword::Exit))?.is_some() {
                if tokens.token_equals("(")? {
                    exit_args = ArgumentNode::parse_list(tokens)?;
                }
                if exit.is_none() {
                    exit = Some(StatementBodyNode::parse(tokens)?)
                } else {
                    return Err(tokens.error("Cannot have multiple definitions of exit"));
                }
            } else {
                let stmt = IndependentNode::parse(tokens)?;
                tokens.expect_newline()?;
                match ClassStatementNode::try_from(stmt) {
                    Result::Ok(stmt) => others.push(stmt),
                    Result::Err(stmt) => {
                        return Err(ParserException::of(
                            "Illegal statement for context definition",
                            stmt,
                        )
                        .into())
                    }
                }
            }
            tokens.pass_newlines()?;
        }
        if tokens.next_if_equals("}")?.is_none() {
            return Err(tokens.error("Conxtet manager must end with a close curly brace"));
        }
        Ok(ContextDefinitionNode::new(
            info,
            name,
            args,
            enter.unwrap_or_default(),
            exit.unwrap_or_default(),
            exit_args,
            ClassBodyNode::from_vec(others),
        ))
    }
}

impl Lined for ContextDefinitionNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
