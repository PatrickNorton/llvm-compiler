use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::{TypeLikeNode, TypeNode};
use crate::parser::variable::VariableNode;
use std::mem::take;

#[derive(Debug)]
pub struct TypedArgumentNode {
    line_info: LineInfo,
    type_val: TypeLikeNode,
    name: VariableNode,
    default_val: TestNode,
    is_vararg: bool,
    vararg_type: VarargType,
}

#[derive(Debug)]
pub struct TypedArgumentListNode {
    line_info: LineInfo,
    position_args: Vec<TypedArgumentNode>,
    normal_args: Vec<TypedArgumentNode>,
    name_args: Vec<TypedArgumentNode>,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum VarargType {
    None,
    Single,
    Double,
}

impl TypedArgumentNode {
    pub fn new(
        line_info: LineInfo,
        type_val: TypeLikeNode,
        name: VariableNode,
        default_val: TestNode,
        is_vararg: bool,
        vararg_type: VarargType,
    ) -> Self {
        Self {
            line_info,
            type_val,
            name,
            default_val,
            is_vararg,
            vararg_type,
        }
    }

    pub fn parse(tokens: &mut TokenList, is_typed: bool) -> ParseResult<TypedArgumentNode> {
        let vararg_type = VarargType::parse(tokens)?;
        Self::parse_vararg(tokens, is_typed, vararg_type)
    }

    pub fn parse_allowing_empty(
        tokens: &mut TokenList,
        allow_untyped: bool,
        type_decided: bool,
    ) -> ParseResult<Option<TypedArgumentNode>> {
        tokens.pass_newlines()?;
        let vararg_type = VarargType::parse(tokens)?;
        if !type_decided {
            let arg_is_untyped = Self::argument_is_untyped(tokens)?;
            Self::parse_vararg(tokens, !arg_is_untyped, vararg_type).map(Option::Some)
        } else {
            Self::parse_vararg(tokens, !allow_untyped, vararg_type).map(Option::Some)
        }
    }

    fn parse_vararg(
        tokens: &mut TokenList,
        is_typed: bool,
        vararg_type: VarargType,
    ) -> ParseResult<TypedArgumentNode> {
        if let TokenType::Keyword(Keyword::Var) = tokens.token_type()? {
            return Err(tokens.error("var is not allowed in a typed argument"));
        }
        let type_var = if is_typed {
            TypeLikeNode::parse(tokens, true)?
        } else {
            todo!("TypeNode::var()")
        };
        let var = VariableNode::parse(tokens)?;
        tokens.pass_newlines()?;
        let default_value = TestNode::parse_on_str(tokens, "=", true)?;
        Ok(TypedArgumentNode::new(
            type_var.line_info().clone(),
            type_var,
            var,
            default_value,
            !vararg_type.is_empty(),
            vararg_type,
        ))
    }

    fn argument_is_untyped(tokens: &mut TokenList) -> ParseResult<bool> {
        let is_vararg = tokens.token_eq_either("*", "**")?;
        let size = TypeLikeNode::size_of_type(tokens, is_vararg.into())?;
        let newlines_at = tokens.number_of_newlines(size)?;
        Ok(size == 0
            || !matches!(
                tokens.token_type_at(size + newlines_at)?,
                TokenType::Name(_)
            ))
    }
}

impl TypedArgumentListNode {
    pub fn new(
        line_info: LineInfo,
        position_args: Vec<TypedArgumentNode>,
        normal_args: Vec<TypedArgumentNode>,
        name_args: Vec<TypedArgumentNode>,
    ) -> Self {
        Self {
            line_info,
            position_args,
            normal_args,
            name_args,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<TypedArgumentListNode> {
        Self::parse_inner(tokens, false)
    }

    pub fn parse_untyped(tokens: &mut TokenList) -> ParseResult<TypedArgumentListNode> {
        Self::parse_inner(tokens, true)
    }

    pub fn parse_optional_parens(tokens: &mut TokenList) -> ParseResult<TypedArgumentListNode> {
        if tokens.token_equals("(")? {
            Self::parse_untyped(tokens)
        } else {
            let line_info = tokens.line_info()?.clone();
            Self::parse_inside_parens(tokens, line_info, true)
        }
    }

    fn parse_inner(
        tokens: &mut TokenList,
        allow_untyped: bool,
    ) -> ParseResult<TypedArgumentListNode> {
        assert!(tokens.token_equals("(")?);
        let info = tokens.next_tok(true)?.deconstruct().0;
        let list = Self::parse_inside_parens(tokens, info, allow_untyped)?;
        tokens.expect(")", false)?;
        Ok(list)
    }

    fn parse_inside_parens(
        tokens: &mut TokenList,
        info: LineInfo,
        mut allow_untyped: bool,
    ) -> ParseResult<TypedArgumentListNode> {
        let mut untyped_decided = !allow_untyped;
        let mut pos_args = None;
        let mut args = Vec::new();
        let mut kw_args = Vec::new();
        let mut current_is_kw = false;
        let mut current_arg_list = &mut args;
        while TypeNode::next_is_type(tokens)? || tokens.token_equals("/")? {
            if tokens.token_equals("/")? {
                if pos_args.is_none() || current_is_kw {
                    return Err(tokens.error("Illegal use of name-only tokens"));
                }
                pos_args = Some(take(&mut args));
                current_arg_list = &mut args;
                tokens.next_tok(true)?;
            } else if tokens.token_equals("*")? && !current_is_kw {
                let next = TypedArgumentNode::parse_allowing_empty(
                    tokens,
                    allow_untyped,
                    untyped_decided,
                )?;
                if let Option::Some(n) = next {
                    let is_decided = n.type_val.is_decided();
                    current_arg_list.push(n);
                    if !untyped_decided {
                        allow_untyped = !is_decided;
                    }
                } else {
                    current_arg_list = &mut kw_args;
                    current_is_kw = true;
                }
            } else {
                if !untyped_decided {
                    allow_untyped = TypedArgumentNode::argument_is_untyped(tokens)?;
                    untyped_decided = true;
                }
                current_arg_list.push(TypedArgumentNode::parse(tokens, !allow_untyped)?);
                tokens.pass_newlines()?;
            }
        }
        Ok(TypedArgumentListNode::new(
            info,
            pos_args.unwrap_or_else(Vec::new),
            args,
            kw_args,
        ))
    }
}

impl VarargType {
    pub fn is_empty(&self) -> bool {
        self == &VarargType::None
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<VarargType> {
        Ok(match tokens.first()?.get_sequence() {
            "*" => {
                tokens.next_tok(true)?;
                VarargType::Single
            }
            "**" => {
                tokens.next_tok(true)?;
                VarargType::Double
            }
            _ => VarargType::None,
        })
    }
}
