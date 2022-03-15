use itertools::Itertools;

use crate::parser::descriptor::DescriptorNode;
use crate::parser::dotted::DottedVariableNode;
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::operator::OperatorTypeNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct TypeNode {
    line_info: LineInfo,
    name: DottedVariableNode,
    sub_types: Vec<TypeNode>,
    is_vararg: bool,
    optional: bool,
    mutability: Option<DescriptorNode>,
}

#[derive(Debug)]
pub enum TypeLikeNode {
    Type(TypeNode),
    Var(LineInfo),
}

impl TypeNode {
    pub fn from_dotted_var(name: DottedVariableNode, optional: bool) -> TypeNode {
        TypeNode {
            line_info: name.line_info().clone(),
            name,
            sub_types: Vec::new(),
            is_vararg: false,
            optional,
            mutability: Option::None,
        }
    }

    pub fn new(
        name: DottedVariableNode,
        sub_types: Vec<TypeNode>,
        is_vararg: bool,
        optional: bool,
    ) -> TypeNode {
        Self::new_mutability(name, sub_types, is_vararg, optional, None)
    }

    pub fn new_mutability(
        name: DottedVariableNode,
        sub_types: Vec<TypeNode>,
        is_vararg: bool,
        optional: bool,
        mutability: Option<DescriptorNode>,
    ) -> TypeNode {
        TypeNode {
            line_info: name.line_info().clone(),
            name,
            sub_types,
            is_vararg,
            optional,
            mutability,
        }
    }

    pub fn get_name(&self) -> &DottedVariableNode {
        &self.name
    }

    pub fn get_subtypes(&self) -> &[TypeNode] {
        &self.sub_types
    }

    pub fn is_optional(&self) -> bool {
        self.optional
    }

    pub fn get_mutability(&self) -> Option<DescriptorNode> {
        self.mutability
    }

    pub fn is_vararg(&self) -> bool {
        self.is_vararg
    }

    // FIXME? Should these be the same?
    pub fn str_name(&self) -> &str {
        <&VariableNode>::try_from(self.name.get_pre_dot())
            .unwrap()
            .get_name()
    }

    // FIXME: This is hideous and should be redone--types should get a special type for their names
    pub fn str_names(&self) -> String {
        let post_dots = self.name.get_post_dots();
        if post_dots.is_empty() {
            self.str_name().to_owned()
        } else {
            format!(
                "{}.{}",
                self.str_name(),
                post_dots
                    .iter()
                    .map(|x| x.get_post_dot().str_name())
                    .format(".")
            )
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<TypeNode> {
        Self::parse_newline(tokens, false)
    }

    pub fn parse_newline(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TypeNode> {
        Self::parse_internal(tokens, false, false, ignore_newlines)
    }

    pub fn parse_ret_val(
        tokens: &mut TokenList,
        ignore_newlines: bool,
    ) -> ParseResult<Vec<TypeNode>> {
        if parse_if_matches!(tokens, ignore_newlines, TokenType::Arrow)?.is_some() {
            Self::parse_list(tokens, ignore_newlines)
        } else {
            Ok(Vec::new())
        }
    }

    pub fn parse_list_on_keyword(
        tokens: &mut TokenList,
        keyword: Keyword,
    ) -> ParseResult<Vec<TypeNode>> {
        tokens
            .next_if(|x| x.is_kwd(keyword))?
            .map_or_else(|| Ok(Vec::new()), |_| Self::parse_list(tokens, false))
    }

    pub fn parse_list(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<Vec<TypeNode>> {
        let mut types = Vec::new();
        while let TokenType::Name(_) | TokenType::Descriptor(_) = tokens.token_type()? {
            types.push(Self::parse_newline(tokens, ignore_newlines)?);
            if !matches!(tokens.token_type()?, TokenType::Comma) {
                break;
            }
            tokens.next_tok(ignore_newlines)?;
        }
        Ok(types)
    }

    pub fn set_mutability(&mut self, descriptor: DescriptorNode) {
        self.mutability = Some(descriptor);
    }

    fn parse_internal(
        tokens: &mut TokenList,
        allow_empty: bool,
        is_vararg: bool,
        ignore_newlines: bool,
    ) -> ParseResult<TypeNode> {
        let mutability = if let TokenType::Descriptor(DescriptorNode::Mut) = tokens.token_type()? {
            tokens.next_token()?;
            Some(DescriptorNode::Mut)
        } else {
            None
        };
        let main = if !matches!(tokens.token_type()?, TokenType::Name(_)) {
            if allow_empty && tokens.token_equals("[")? {
                DottedVariableNode::empty()
            } else {
                return Err(tokens.error_expected("type name"));
            }
        } else {
            DottedVariableNode::parse_names_only(tokens, ignore_newlines)?
        };
        if !tokens.token_equals("[")? {
            let optional = parse_if_matches!(
                tokens,
                true,
                TokenType::Operator(OperatorTypeNode::Optional)
            )?
            .is_some();
            return Ok(TypeNode::from_dotted_var(main, optional));
        }
        tokens.next_tok(true)?;
        let mut subtypes = Vec::new();
        while !tokens.token_equals("]")? {
            let subclass_is_vararg = if tokens.token_eq_either("*", "**")? {
                tokens.next_tok(true)?;
                true
            } else {
                false
            };
            let subtype = if let TokenType::Descriptor(descriptor) = *(tokens.token_type()?) {
                tokens.next_tok(true)?;
                if descriptor.is_mut_node() {
                    let mut subtype = Self::parse_internal(tokens, true, subclass_is_vararg, true)?;
                    subtype.mutability = Some(descriptor);
                    subtype
                } else {
                    return Err(tokens.error("Invalid descriptor for type"));
                }
            } else {
                Self::parse_internal(tokens, true, subclass_is_vararg, true)?
            };
            subtypes.push(subtype);
            if parse_if_matches!(tokens, true, TokenType::Comma)?.is_some() {
                continue;
            }
            tokens.pass_newlines()?;
            if !tokens.token_equals("]")? {
                return Err(tokens.error("Comma must separate subtypes"));
            }
        }
        tokens.next_tok(ignore_newlines)?;
        let optional = parse_if_matches!(
            tokens,
            ignore_newlines,
            TokenType::Operator(OperatorTypeNode::Optional)
        )?
        .is_some();
        Ok(TypeNode::new_mutability(
            main, subtypes, is_vararg, optional, mutability,
        ))
    }

    pub fn next_is_type(tokens: &mut TokenList) -> ParseResult<bool> {
        let first = tokens.first()?;
        Ok(match first.token_type() {
            TokenType::Name(_) | TokenType::Keyword(Keyword::Var) => true,
            TokenType::Descriptor(d) => d.is_mut_node(),
            _ => first.get_sequence() == "[" || first.get_sequence() == "*",
        })
    }
}

impl TypeLikeNode {
    pub fn is_decided(&self) -> bool {
        match self {
            TypeLikeNode::Type(_) => true,
            TypeLikeNode::Var(_) => false,
        }
    }

    pub fn set_mutability(&mut self, descriptor: DescriptorNode) {
        match self {
            TypeLikeNode::Type(t) => t.set_mutability(descriptor),
            TypeLikeNode::Var(_) => todo!(),
        }
    }

    pub fn parse(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TypeLikeNode> {
        match *tokens.token_type()? {
            TokenType::Keyword(Keyword::Var) => {
                let (line_info, _) = tokens.next_tok(ignore_newlines)?.deconstruct();
                Ok(TypeLikeNode::Var(line_info))
            }
            TokenType::Descriptor(descriptor) => {
                tokens.next_tok(ignore_newlines)?;
                if descriptor.is_mut_node() {
                    let mut node = Self::parse_no_mut(tokens, ignore_newlines)?;
                    node.set_mutability(descriptor);
                    Ok(node)
                } else {
                    Err(tokens.error("Invalid descriptor for type"))
                }
            }
            _ => Self::parse_no_mut(tokens, ignore_newlines),
        }
    }

    pub fn as_type(&self) -> &TypeNode {
        match self {
            TypeLikeNode::Type(x) => x,
            TypeLikeNode::Var(_) => panic!("Expected a known type"),
        }
    }

    fn parse_no_mut(tokens: &mut TokenList, ignore_newlines: bool) -> ParseResult<TypeLikeNode> {
        if tokens
            .next_if_ignoring(ignore_newlines, |x| x.equals("("))?
            .is_some()
        {
            let type_var = Self::parse_no_mut(tokens, true)?;
            if !tokens.token_equals(")")? {
                return Err(tokens.error_expected(")"));
            }
            tokens.next_tok(ignore_newlines)?;
            Ok(type_var)
        } else {
            assert!(matches!(
                tokens.token_type()?,
                TokenType::Name(_) | TokenType::Keyword(Keyword::Var)
            ));
            TypeNode::parse_newline(tokens, ignore_newlines).map(TypeLikeNode::Type)
        }
    }

    pub fn size_of_type(tokens: &mut TokenList, start: usize) -> ParseResult<usize> {
        if let TokenType::Keyword(Keyword::Var) = tokens.token_type()? {
            return Ok(start + 1);
        }
        let mut net_braces = 0usize;
        let mut previous = Option::None;
        for i in start.. {
            let token = tokens.get_token(i)?;
            match token.token_type() {
                TokenType::OpenBrace(ch) => match ch {
                    '(' => {
                        if !matches!(
                            previous,
                            None | Some(
                                TokenType::Comma | TokenType::Operator(_) | TokenType::OpenBrace(_)
                            )
                        ) {
                            return Ok(0);
                        } else {
                            net_braces += 1;
                        }
                    }
                    '{' => {
                        return Ok(if net_braces == 0 {
                            i.saturating_sub(1)
                        } else {
                            0
                        })
                    }
                    '[' => {
                        if !matches!(
                            previous,
                            None | Some(TokenType::Name(_) | TokenType::Epsilon)
                        ) {
                            return Ok(0);
                        } else {
                            net_braces += 1;
                        }
                    }
                    _ => panic!("Unknown brace type {}", ch),
                },
                TokenType::CloseBrace(_) => net_braces -= 1,
                TokenType::Name(_) => {
                    if let Some(
                        TokenType::Name(_)
                        | TokenType::CloseBrace(_)
                        | TokenType::Operator(OperatorTypeNode::Optional),
                    ) = previous
                    {
                        return Ok(i);
                    }
                }
                TokenType::Dot(_) => {
                    if !matches!(previous, Some(TokenType::Name(_))) {
                        return Ok(0);
                    }
                }
                TokenType::Operator(op) => match op {
                    OperatorTypeNode::Multiply
                        if matches!(previous, Some(TokenType::Comma | TokenType::OpenBrace(_))) =>
                    {
                        return Ok(0)
                    }
                    OperatorTypeNode::Optional
                    | OperatorTypeNode::Multiply
                    | OperatorTypeNode::BoolOr
                    | OperatorTypeNode::BoolAnd => {}
                    _ => return Ok(0),
                },
                TokenType::Comma | TokenType::Newline => {
                    if net_braces == 0 {
                        return Ok(i);
                    }
                }
                _ => return Ok(if net_braces == 0 { i } else { 0 }),
            }
            previous = if let TokenType::Newline = token.token_type() {
                previous
            } else {
                Some(token.token_type().clone())
            };
        }
        unreachable!("Infinite loop should never be broken from")
    }
}

impl Lined for TypeNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for TypeLikeNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            TypeLikeNode::Type(t) => t.line_info(),
            TypeLikeNode::Var(l) => l,
        }
    }
}
