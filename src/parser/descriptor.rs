use crate::parser::error::{ParseResult, ParserException};
use crate::parser::line_info::LineInfo;
use crate::parser::token::{Token, TokenType};
use crate::parser::token_list::TokenList;
use crate::{hash_map, hash_set};
use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Formatter;

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

static DEFINITION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
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
static FUNCTION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Generator,
        DescriptorNode::Synced,
        DescriptorNode::Native,
    )
});
static DECLARATION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
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
static CONTEXT_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
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
static METHOD_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
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
static STATIC_BLOCK_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!());
static INTERFACE_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
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
    /// Get a `DescriptorNode` based on the string given.
    pub fn find(name: &str) -> DescriptorNode {
        VALUES[name]
    }

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
            if input.starts_with(key) {
                return Option::Some((TokenType::Descriptor(*value), input.len()));
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

impl fmt::Display for DescriptorNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.name(), f)
    }
}
