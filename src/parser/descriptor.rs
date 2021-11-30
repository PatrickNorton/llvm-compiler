use crate::macros::{hash_map, hash_set};
use crate::parser::assign::{AssignStatementNode, AssignmentNode};
use crate::parser::base::IndependentNode;
use crate::parser::declaration::DeclarationNode;
use crate::parser::error::{ParseResult, ParserError, ParserException};
use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::interface::InterfaceStatementNode;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::line_matches;
use crate::parser::operator_def::OperatorDefinitionNode;
use crate::parser::token::{Token, TokenType};
use crate::parser::token_list::TokenList;
use itertools::Itertools;
use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Formatter};
use unicode_xid::UnicodeXID;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DescriptorNode {
    Public,
    Private,
    Protected,
    Pubget,
    Static,
    Mut,
    Mref,
    Readonly,
    Final,
    Nonfinal,
    Native,
    Generator,
    Synced,
    Auto,
    Const,
}

#[derive(Debug)]
pub enum DescribableNode {
    Assign(AssignmentNode),
    Function(FunctionDefinitionNode),
    Interface(InterfaceStatementNode),
}

static MUT_NODES: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Mut,
        DescriptorNode::Mref,
        DescriptorNode::Readonly,
        DescriptorNode::Final,
    )
});

static ACCESS: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Pubget,
        DescriptorNode::Protected
    )
});
static STATIC_SET: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!(DescriptorNode::Static));
static MOD_SET: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Mut,
        DescriptorNode::Mref,
        DescriptorNode::Readonly
    )
});
static FINAL_SET: Lazy<HashSet<DescriptorNode>> =
    Lazy::new(|| hash_set!(DescriptorNode::Final, DescriptorNode::Nonfinal));
static NATIVE_SET: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!(DescriptorNode::Native));
static GENERATOR_SET: Lazy<HashSet<DescriptorNode>> =
    Lazy::new(|| hash_set!(DescriptorNode::Generator));
static SYNCED_SET: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!(DescriptorNode::Synced));
static CONST_SET: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!(DescriptorNode::Const));
static AUTO_SET: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!(DescriptorNode::Auto));

static SETS: Lazy<Vec<&'static HashSet<DescriptorNode>>> = Lazy::new(|| {
    vec![
        &*ACCESS,
        &*STATIC_SET,
        &*MOD_SET,
        &*FINAL_SET,
        &*NATIVE_SET,
        &*GENERATOR_SET,
        &*SYNCED_SET,
        &*CONST_SET,
        &*AUTO_SET,
    ]
});

pub static DEFINITION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Protected,
        DescriptorNode::Mut,
        DescriptorNode::Final,
        DescriptorNode::Nonfinal,
        DescriptorNode::Static,
        DescriptorNode::Native,
        DescriptorNode::Const,
    )
});
pub static FUNCTION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Generator,
        DescriptorNode::Synced,
        DescriptorNode::Native,
    )
});
pub static DECLARATION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Pubget,
        DescriptorNode::Protected,
        DescriptorNode::Mut,
        DescriptorNode::Mref,
        DescriptorNode::Readonly,
        DescriptorNode::Final,
        DescriptorNode::Static,
        DescriptorNode::Native,
    )
});
pub static CONTEXT_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Protected,
        DescriptorNode::Mut,
        DescriptorNode::Generator,
        DescriptorNode::Final,
        DescriptorNode::Static,
        DescriptorNode::Synced,
        DescriptorNode::Native,
    )
});
pub static METHOD_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Protected,
        DescriptorNode::Mut,
        DescriptorNode::Final,
        DescriptorNode::Nonfinal,
        DescriptorNode::Static,
        DescriptorNode::Native,
        DescriptorNode::Generator,
        DescriptorNode::Synced,
    )
});
pub static STATIC_BLOCK_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!());
pub static INTERFACE_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Public,
        DescriptorNode::Private,
        DescriptorNode::Protected,
        DescriptorNode::Mut,
        DescriptorNode::Final,
        DescriptorNode::Nonfinal,
        DescriptorNode::Static,
        DescriptorNode::Native,
        DescriptorNode::Auto,
    )
});
static VALUES: Lazy<HashMap<&'static str, DescriptorNode>> = Lazy::new(|| {
    hash_map!(
        "public" => DescriptorNode::Public,
        "private" => DescriptorNode::Private,
        "protected" => DescriptorNode::Protected,
        "pubget" => DescriptorNode::Pubget,
        "static" => DescriptorNode::Static,
        "mut" => DescriptorNode::Mut,
        "mref" => DescriptorNode::Mref,
        "readonly" => DescriptorNode::Readonly,
        "final" => DescriptorNode::Final,
        "nonfinal" => DescriptorNode::Nonfinal,
        "native" => DescriptorNode::Native,
        "generator" => DescriptorNode::Generator,
        "synced" => DescriptorNode::Synced,
        "auto" => DescriptorNode::Auto,
        "const" => DescriptorNode::Const,
    )
});

impl DescriptorNode {
    pub fn name(&self) -> &'static str {
        match self {
            DescriptorNode::Public => "public",
            DescriptorNode::Private => "private",
            DescriptorNode::Protected => "protected",
            DescriptorNode::Pubget => "pubget",
            DescriptorNode::Static => "static",
            DescriptorNode::Mut => "mut",
            DescriptorNode::Mref => "mref",
            DescriptorNode::Readonly => "readonly",
            DescriptorNode::Final => "final",
            DescriptorNode::Nonfinal => "nonfinal",
            DescriptorNode::Native => "native",
            DescriptorNode::Generator => "generator",
            DescriptorNode::Synced => "synced",
            DescriptorNode::Auto => "auto",
            DescriptorNode::Const => "const",
        }
    }

    pub fn is_mut_node(&self) -> bool {
        MUT_NODES.contains(self)
    }

    fn deconstruct_token(token: Token) -> Option<(DescriptorNode, LineInfo)> {
        match token.deconstruct() {
            (line_info, TokenType::Descriptor(descr)) => Option::Some((descr, line_info)),
            _ => Option::None,
        }
    }

    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        for (key, value) in &*VALUES {
            if input.starts_with(key)
                && input[key.len()..]
                    .chars()
                    .next()
                    .map_or_else(|| true, |x| !UnicodeXID::is_xid_continue(x))
            {
                return Option::Some((TokenType::Descriptor(*value), key.len()));
            }
        }
        Option::None
    }

    pub fn parse_list(tokens: &mut TokenList) -> ParseResult<HashSet<DescriptorNode>> {
        let mut sets_num = 0;
        let mut descriptors = HashSet::new();
        while matches!(tokens.token_type()?, TokenType::Descriptor(_)) {
            let (node, line_info) = Self::deconstruct_token(tokens.next_token()?).unwrap();
            loop {
                sets_num += 1;
                if sets_num > SETS.len() {
                    return ParseResult::Err(
                        ParserException::of("Illegal descriptor combination", line_info).into(),
                    );
                }
                if SETS[sets_num - 1].contains(&node) {
                    break;
                }
            }
            descriptors.insert(node);
        }
        ParseResult::Ok(descriptors)
    }
}

impl DescribableNode {
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        match self {
            DescribableNode::Assign(a) => a.valid_descriptors(),
            DescribableNode::Function(f) => f.valid_descriptors(),
            DescribableNode::Interface(i) => i.valid_descriptors(),
        }
    }

    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        match self {
            DescribableNode::Assign(a) => a.add_descriptors(descriptors),
            DescribableNode::Function(f) => f.add_descriptors(descriptors),
            DescribableNode::Interface(i) => i.add_descriptors(descriptors),
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<DescribableNode> {
        let descriptors = if let TokenType::Descriptor(_) = tokens.token_type()? {
            DescriptorNode::parse_list(tokens)?
        } else {
            HashSet::new()
        };
        if !descriptors.is_disjoint(&MUT_NODES) {
            Self::parse_mutability(tokens, descriptors)
        } else {
            Self::finish_parse(IndependentNode::parse(tokens)?, descriptors)
        }
    }

    fn parse_mutability(
        tokens: &mut TokenList,
        descriptors: HashSet<DescriptorNode>,
    ) -> ParseResult<DescribableNode> {
        assert!(!descriptors.is_disjoint(&MUT_NODES));
        if let TokenType::Keyword(Keyword::Var) = tokens.token_type()? {
            Self::finish_parse(IndependentNode::parse_var(tokens)?, descriptors)
        } else if let TokenType::Keyword(_) = tokens.token_type()? {
            Self::finish_parse(IndependentNode::parse(tokens)?, descriptors)
        } else if let TokenType::OperatorSp(_) = tokens.token_type()? {
            Self::finish_parse(
                IndependentNode::OpDef(OperatorDefinitionNode::parse(tokens)?),
                descriptors,
            )
        } else if line_matches!(tokens, TokenType::Assign(_))? {
            Self::finish_parse(AssignStatementNode::parse(tokens)?.into(), descriptors)
        } else if line_matches!(tokens, TokenType::AugAssign(_))? {
            Err(tokens.error("mut cannot be used in augmented assignment"))
        } else {
            Self::finish_parse(DeclarationNode::parse(tokens)?.into(), descriptors)
        }
    }

    fn finish_parse(
        stmt: IndependentNode,
        descriptors: HashSet<DescriptorNode>,
    ) -> ParseResult<DescribableNode> {
        match DescribableNode::try_from(stmt) {
            Result::Ok(mut statement) => {
                if !statement.valid_descriptors().is_superset(&descriptors) {
                    Err(ParserError::Normal(ParserException::of(
                        Self::error_message(&statement, &descriptors),
                        statement,
                    )))
                } else {
                    statement.add_descriptors(descriptors);
                    Ok(statement)
                }
            }
            Result::Err(stmt) => {
                Err(ParserException::of("Descriptor not allowed in statement", stmt).into())
            }
        }
    }

    fn error_message(statement: &DescribableNode, descriptors: &HashSet<DescriptorNode>) -> String {
        let disjoint = descriptors.difference(statement.valid_descriptors());
        format!(
            "Invalid descriptor(s): {} not allowed in statement",
            disjoint.map(|x| x.name()).format(", ")
        )
    }
}

impl fmt::Display for DescriptorNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.name(), f)
    }
}

impl Lined for DescribableNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            DescribableNode::Assign(a) => a.line_info(),
            DescribableNode::Function(f) => f.line_info(),
            DescribableNode::Interface(i) => i.line_info(),
        }
    }
}

impl TryFrom<IndependentNode> for DescribableNode {
    type Error = IndependentNode;

    fn try_from(value: IndependentNode) -> Result<Self, Self::Error> {
        Ok(match value {
            IndependentNode::Assign(a) => DescribableNode::Assign(a),
            IndependentNode::FunctionDef(f) => DescribableNode::Function(f),
            value => DescribableNode::Interface(InterfaceStatementNode::try_from(value)?),
        })
    }
}

impl From<DescribableNode> for IndependentNode {
    fn from(node: DescribableNode) -> Self {
        match node {
            DescribableNode::Assign(a) => IndependentNode::Assign(a),
            DescribableNode::Function(f) => IndependentNode::FunctionDef(f),
            DescribableNode::Interface(i) => i.into(),
        }
    }
}
