use crate::parser::base::IndependentNode;
use crate::parser::error::ParseResult;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::tokenizer::Tokenizer;
use std::fs::File;
use std::io;
use std::path::PathBuf;

#[derive(Debug)]
pub struct Parser {
    tokens: TokenList,
    top: TopNode,
}

#[derive(Debug)]
pub struct TopNode {
    path: PathBuf,
    nodes: Vec<IndependentNode>,
}

impl Parser {
    pub fn parse(path: PathBuf, mut tokens: TokenList) -> ParseResult<TopNode> {
        let mut nodes = Vec::new();
        tokens.pass_newlines()?;
        while !matches!(tokens.token_type()?, TokenType::Epsilon) {
            nodes.push(IndependentNode::parse(&mut tokens)?);
            if matches!(tokens.token_type()?, TokenType::Epsilon) {
                break;
            }
            tokens.expect_newline()?;
        }
        Ok(TopNode::new(path, nodes))
    }

    pub fn parse_file(f: PathBuf) -> io::Result<ParseResult<TopNode>> {
        let file = File::open(&f)?;
        Ok(Tokenizer::parse(file).and_then(|t| Self::parse(f, t)))
    }
}

impl TopNode {
    fn new(path: PathBuf, nodes: Vec<IndependentNode>) -> Self {
        Self { path, nodes }
    }
}

impl<'a> IntoIterator for &'a TopNode {
    type Item = &'a IndependentNode;

    type IntoIter = <&'a [IndependentNode] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.iter()
    }
}
