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

/// A node representing a descriptor, such as `public` or `private`.
///
/// These are prepended to certain nodes, all of which are members of the
/// [`DescribableNode`] enum; see that for more details.
///
/// # Ordering
///
/// Descriptors must appear in a certain order when prefixing a statement. That order
/// is:
///
/// 1. Access-level descriptors
/// 2. Mutability descriptors
/// 3. Inheritance descriptors
/// 4. `native`
/// 5. `generator`
/// 6. `synced`
/// 7. `const`
/// 8. `auto`
///
/// # Categories
///
/// Descriptors are separated into several categories based on type. For the
/// most part, these are mutually exclusive per statement, e.g. no more than one
/// of each category may be included as part of any statement. Note that some
/// descriptors may appear in different categories depending on context, for
/// example [`final`](DescriptorNode::Final), which may be either a mutability
/// descriptor or an inheritance descriptor.
///
/// ## Access-level descriptors
///
/// The four access-level descriptors are
/// - [`public`](DescriptorNode::Public)
/// - [`private`](DescriptorNode::Private)
/// - [`protected`](DescriptorNode::Protected)
/// - [`pubget`](DescriptorNode::Pubget)
///
/// These descriptors each correspond to an [access level](AccessLevel). No more
/// than one of these may exist per declaration, however, having none is legal
/// and defaults to [`AccessLevel::File`], where such descriptors are allowed.
///
/// ## Mutability descriptors
///
/// The four mutability descriptors are
/// - [`mut`](DescriptorNode::Mut)
/// - [`mref`](DescriptorNode::Mref)
/// - [`readonly`](DescriptorNode::Readonly)
/// - [`final`](DescriptorNode::Final)
///
/// These descriptors each correspond to a different type of variable
/// mutability. Each descriptor is mutually exclusive, however having none is
/// legal and defaults to immutability (which is not representable by any
/// descriptor).
///
/// Not all describable nodes take a mutability descriptor. For example, class
/// and class-like definitions do not, as there is no meaning that makes sense
/// for them. In addition, `final` is both a mutability descriptor and an
/// inheritance descriptor. There are no statements that can take both of those
/// categories, so there is no possibility for confusion.
///
/// ## Inheritance descriptors
///
/// The two inheritance descriptors are
/// - [`final`](DescriptorNode::Final)
/// - [`nonfinal`](DescriptorNode::Nonfinal)
///
/// These descriptors correspond to class (and class-like) inheritability:
/// `final` means the class may not be inherited from and `nonfinal` means it
/// may. These two descriptors are mutually exclusive. When neither is specified
/// and one was expected, `final` is the default. `final` may also be a
/// mutability descriptor depending on context; there are no statements on which
/// both are legal, so the meaning is always clear.
///
/// ## Miscellaneous descriptors
///
/// All other descriptors are miscellaneous. These are
/// - [`static`](DescriptorNode::Static)
/// - [`native`](DescriptorNode::Native)
/// - [`generator`](DescriptorNode::Generator)
/// - [`synced`](DescriptorNode::Synced)
/// - [`auto`](DescriptorNode::Auto)
/// - [`const`](DescriptorNode::Const)
///
/// These descriptors all have unique meanings; see their respective
/// documentation for details. At present, none within this set are mutually
/// exclusive, although not all are available on all declaration types.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DescriptorNode {
    /// The node representing the `public` descriptor.
    ///
    /// This corresponds to the `public` access level, meaning the described
    /// object may be accessed from any place within the codebase.
    Public,

    /// The node representing the `private` descriptor.
    ///
    /// This corresponds to the `private` access level, meaning the described
    /// object may only be accessed by the class within which it is defined.
    Private,

    /// The node representing the `protected` descriptor.
    ///
    /// This corresponds to the `protected` access level, meaning the described
    /// object may only be accessed by the class within which it is defined, or
    /// any subclass of that class.
    Protected,

    /// The node representing the `pubget` descriptor.
    ///
    /// This corresponds to the `pubget` access level, corresponding to the
    /// `public` level for immutable access, but the `private` level for mutable
    /// access.
    Pubget,

    /// The node repsresenting the `static` descriptor.
    ///
    /// When used on a method definition, it means the method is a static
    /// method, i.e. it is not used on a class instance, but the class itself.
    /// When used on a variable definition, it means the variable is static, so
    /// it is only initialized once per run, which occurs the first time it is
    /// reached.
    Static,

    /// The node representing the `mut` descriptor.
    ///
    /// This represents the `mut` mutability type, meaning both the variable is
    /// mutable and the underlying data is mutable. When used on a type, it
    /// means the type is mutable.
    Mut,

    /// The node representing the `mref` descriptor.
    ///
    /// This represents the `mref` mutability type, meaning the variable is
    /// mutable, but the underlying data is immutable.
    Mref,

    /// The node representing the `readonly` descriptor.
    ///
    /// This represents the `readonly` mutability type, meaning the variable
    /// cannot be explicitly changed, but may change value regardless. One
    /// example of this is properties without a setter.
    Readonly,

    /// The node representing the `final` descriptor.
    ///
    /// When used as a mutability descriptor, it represents the `final` mutability
    /// type. This means the variable cannot be changed, but the underlying data
    /// may.
    ///
    /// When used as an inheritance descriptor it represents the `final`
    /// inheritance type. This means no subclasses may inherit from the
    /// described class.
    Final,

    /// The node representing the `nonfinal` descriptor.
    ///
    /// This represents the `nonfinal` inheritance type, meaning the described
    /// class may be inherited from.
    Nonfinal,

    /// The node representing the `native` descriptor.
    ///
    /// A function or method described with `native` is defined in external code
    /// using the FFI interface. It is therefore not allowed to have a non-empty
    /// body.
    Native,

    /// The node representing the `generator` descriptor.
    ///
    /// A function or method described with `generator` is a generator function.
    /// This makes it implicitly an iterator and is allowed to use `yield`
    /// statements.
    Generator,

    /// The node representing the `synced` descriptor.
    ///
    /// A function or method with the `synced` descriptor is synchronized across
    /// threads, and may only be run by one thread at any given time.
    Synced,

    /// The node representing the `auto` descriptor.
    ///
    /// An interface with the `auto` descriptor is an auto interface, meaning it
    /// is automatically implemented on any class that fulfills its contract.
    Auto,

    /// The node representing the `const` descriptor.
    ///
    /// A class or enum with this descriptor is a const class. Const classes
    /// feature no interior mutability, and therefore may freely be cast between
    /// their non-`mut` and `mut` versions. Furthermore, every superclass of a
    /// const class, as well as all data members, must themselves be `const`
    /// classes.
    Const,
}

// TODO: Make this a trait
/// Any node which may be preceded by descriptors.
///
/// This is mostly used through the [`DescribableNode::parse`] method, which
/// allows parsing of any statement beginning with a [descriptor
/// token](DescriptorNode).
#[derive(Debug)]
pub enum DescribableNode {
    Assign(AssignmentNode),
    Function(FunctionDefinitionNode),
    Interface(InterfaceStatementNode),
}

// FIXME? Make these slices instead of sets

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

/// The set of descriptors valid for use on a [`DefinitionNode`].
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

/// The set of descriptors valid for use on a [`FunctionDefinitionNode`].
pub static FUNCTION_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| {
    hash_set!(
        DescriptorNode::Generator,
        DescriptorNode::Synced,
        DescriptorNode::Native,
    )
});

/// The set of descriptors valid for use on a [`DeclarationNode`].
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

/// The set of descriptors valid for use on a [`ContextDefinitionNode`].
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

/// The set of descriptors valid for use on a [`MethodDefinitionNode`].
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

/// The set of descriptors valid for use on a [`StaticBlockNode`].
///
/// There are no descriptors valid on this node (besides the `static` at the
/// beginning of the block, but that is parsed as part of the node, not here),
/// so the set is empty.
pub static STATIC_BLOCK_VALID: Lazy<HashSet<DescriptorNode>> = Lazy::new(|| hash_set!());

/// The set of descriptors valid for use on an [`InterfaceDefinitionNode`].
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
    /// The textual representation of the descriptor.
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

    /// Returns whether or not the descriptor is a mutability descriptor.
    pub fn is_mut_node(&self) -> bool {
        MUT_NODES.contains(self)
    }

    fn deconstruct_token(token: Token) -> Option<(DescriptorNode, LineInfo)> {
        match token.deconstruct() {
            (line_info, TokenType::Descriptor(descr)) => Option::Some((descr, line_info)),
            _ => Option::None,
        }
    }

    /// Attempts to parse a [`DescriptorNode`] off the front of the given
    /// string.
    ///
    /// If the string does not begin with a descriptor, [`None`] is returned.
    /// Otherwise, two values are returned: a [`TokenType::Descriptor`]
    /// containing the parsed descriptor, and a [`usize`] with the number of
    /// characters parsed.
    pub fn pattern(input: &str) -> Option<(TokenType, usize)> {
        for (key, value) in &*VALUES {
            if input.starts_with(key)
                && input[key.len()..]
                    .chars()
                    .next()
                    .map_or(true, |x| !UnicodeXID::is_xid_continue(x))
            {
                return Option::Some((TokenType::Descriptor(*value), key.len()));
            }
        }
        Option::None
    }

    /// Parses a list of descriptors from the given [`TokenList`].
    ///
    /// This only parses the descriptors, not anything that may be described by
    /// them. This will throw an error if two mutually-exclusive descriptors are
    /// parsed, or if the descriptors come in the wrong order. However, it will
    /// stop at the first non-descriptor node it finds, and it does not ensure
    /// that the descriptors are valid for the node that follows.
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
    /// The set of descriptors that are valid to use on the given node.
    ///
    /// This is simply a forwarding implementation to the constituent variants'
    /// methods.
    pub fn valid_descriptors(&self) -> &'static HashSet<DescriptorNode> {
        match self {
            DescribableNode::Assign(a) => a.valid_descriptors(),
            DescribableNode::Function(f) => f.valid_descriptors(),
            DescribableNode::Interface(i) => i.valid_descriptors(),
        }
    }

    /// Adds the given descriptors to the current node.
    ///
    /// This is simply a forwarding implementation to the constituent variants'
    /// methods.
    pub fn add_descriptors(&mut self, descriptors: HashSet<DescriptorNode>) {
        match self {
            DescribableNode::Assign(a) => a.add_descriptors(descriptors),
            DescribableNode::Function(f) => f.add_descriptors(descriptors),
            DescribableNode::Interface(i) => i.add_descriptors(descriptors),
        }
    }

    /// Parses a [`DescribableNode`] from the given list of tokens.
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
