use crate::parser::annotation::AnnotatableNode;
use crate::parser::assert::AssertStatementNode;
use crate::parser::assign::{AssignStatementNode, AssignmentNode};
use crate::parser::aug_assign::AugmentedAssignmentNode;
use crate::parser::break_stmt::BreakStatementNode;
use crate::parser::class_def::ClassDefinitionNode;
use crate::parser::context::ContextDefinitionNode;
use crate::parser::continue_stmt::ContinueStatementNode;
use crate::parser::declaration::DeclarationNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::defer::DeferStatementNode;
use crate::parser::delete::DeleteStatementNode;
use crate::parser::descriptor::DescribableNode;
use crate::parser::do_stmt::DoStatementNode;
use crate::parser::dotimes::DotimesStatementNode;
use crate::parser::enum_def::EnumDefinitionNode;
use crate::parser::error::ParseResult;
use crate::parser::for_loop::ForStatementNode;
use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::if_stmt::IfStatementNode;
use crate::parser::import::ImportExportNode;
use crate::parser::inc_dec::IncDecNode;
use crate::parser::interface::InterfaceDefinitionNode;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::line_matches;
use crate::parser::method::MethodDefinitionNode;
use crate::parser::name::NameNode;
use crate::parser::operator_def::{OperatorDefinitionNode, SpecialOpAssignmentNode};
use crate::parser::property::PropertyDefinitionNode;
use crate::parser::return_stmt::ReturnStatementNode;
use crate::parser::synchronized::SynchronizedStatementNode;
use crate::parser::test_node::TestNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::try_stmt::TryStatementNode;
use crate::parser::type_node::TypeLikeNode;
use crate::parser::typedef::TypedefStatementNode;
use crate::parser::union_def::UnionDefinitionNode;
use crate::parser::variable::VariableNode;
use crate::parser::while_stmt::WhileStatementNode;
use crate::parser::with_stmt::WithStatementNode;
use crate::parser::yield_stmt::YieldStatementNode;

use super::decorator;
use super::derived_op::DerivedOperatorNode;

/// Any node that can function as an independent statement.
///
/// These nodes are used as the values of a [`StatementBodyNode`]. This means
/// that they are the principal building block of function definitions, as well
/// as any other statements with block bodies, such as
/// [if statements](IfStatementNode), [for](ForStatementNode),
/// [dotimes](DotimesStatementNode), and [while loops](WhileStatementNode),
/// [try statements](TryStatementNode), and many more.
#[derive(Debug)]
pub enum IndependentNode {
    Assert(AssertStatementNode),
    Assign(AssignmentNode),
    AugAssign(AugmentedAssignmentNode),
    Break(BreakStatementNode),
    ClassDef(ClassDefinitionNode),
    Context(ContextDefinitionNode),
    Continue(ContinueStatementNode),
    Declaration(DeclarationNode),
    DeclaredAssign(DeclaredAssignmentNode),
    Defer(DeferStatementNode),
    Delete(DeleteStatementNode),
    Derived(DerivedOperatorNode),
    Do(DoStatementNode),
    Dotimes(DotimesStatementNode),
    Enum(EnumDefinitionNode),
    For(ForStatementNode),
    FunctionDef(FunctionDefinitionNode),
    If(IfStatementNode),
    Import(ImportExportNode),
    IncDec(IncDecNode),
    Interface(InterfaceDefinitionNode),
    Method(MethodDefinitionNode),
    OpDef(OperatorDefinitionNode),
    OpAssign(SpecialOpAssignmentNode),
    Property(PropertyDefinitionNode),
    Return(ReturnStatementNode),
    Sync(SynchronizedStatementNode),
    Test(TestNode),
    Try(TryStatementNode),
    Typedef(TypedefStatementNode),
    Union(UnionDefinitionNode),
    While(WhileStatementNode),
    With(WithStatementNode),
    Yield(YieldStatementNode),
}

impl IndependentNode {
    /// Parses an [`IndependentNode`] from the given list of tokens.
    pub fn parse(tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        tokens.pass_newlines()?;
        match tokens.token_type()? {
            TokenType::Keyword(k) => k.parse_left(tokens),
            TokenType::Descriptor(_) => DescribableNode::parse(tokens).map(From::from),
            TokenType::OpenBrace(_) | TokenType::Name(_) => Self::parse_left_variable(tokens),
            TokenType::CloseBrace(_) => Err(tokens.error("Unmatched close brace")),
            TokenType::AugAssign(_) => Err(tokens.error("Unexpected operator")),
            TokenType::Operator(_)
            | TokenType::String(_)
            | TokenType::Number(_)
            | TokenType::OpFunc(_) => TestNode::parse(tokens).map(IndependentNode::Test),
            TokenType::Assign(_) => Err(tokens.error("Unexpected assignment")),
            TokenType::OperatorSp(_) => {
                if matches!(tokens.token_type_at(1)?, TokenType::Assign(_)) {
                    SpecialOpAssignmentNode::parse(tokens).map(IndependentNode::OpAssign)
                } else {
                    OperatorDefinitionNode::parse(tokens).map(IndependentNode::OpDef)
                }
            }
            TokenType::Ellipsis => VariableNode::parse_ellipsis(tokens)
                .map(NameNode::Variable)
                .map(TestNode::Name)
                .map(IndependentNode::Test),
            TokenType::At => decorator::parse_left_decorator(tokens).map(|x| x.into()),
            TokenType::Dollar => AnnotatableNode::parse_left_annotation(tokens).map(From::from),
            TokenType::Epsilon => Err(tokens.error("Unexpected EOF")),
            TokenType::Arrow
            | TokenType::DoubleArrow
            | TokenType::Increment(_)
            | TokenType::Colon
            | TokenType::Dot(_)
            | TokenType::Comma => Err(tokens.default_error()),
            _ => Err(tokens.error("Nonexistent token found")),
        }
    }

    /// Parses an [`IndependentNode`] from the given list of tokens. The first
    /// element in the list must be a `var` keyword.
    pub fn parse_var(tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        assert!(tokens.first()?.is_kwd(Keyword::Var));
        if line_matches!(tokens, TokenType::Assign(_))? {
            AssignStatementNode::parse(tokens).map(IndependentNode::from)
        } else if line_matches!(tokens, TokenType::AugAssign(_))? {
            Err(tokens.error("var cannot be used in augmented assignment"))
        } else {
            DeclarationNode::parse(tokens).map(From::from)
        }
    }

    fn parse_left_variable(tokens: &mut TokenList) -> ParseResult<IndependentNode> {
        assert!(matches!(
            tokens.token_type()?,
            TokenType::Name(_) | TokenType::OpenBrace(_)
        ));
        let var_size = tokens.size_of_variable()?;
        if is_assignment(tokens)? || is_normal_assignment(tokens)? {
            return AssignStatementNode::parse(tokens).map(Into::into);
        }
        let after_var = tokens.get_token(var_size)?;
        if matches!(after_var.token_type(), TokenType::AugAssign(_)) {
            AugmentedAssignmentNode::parse(tokens).map(IndependentNode::AugAssign)
        } else if matches!(after_var.token_type(), TokenType::Increment(_)) {
            IncDecNode::parse(tokens).map(IndependentNode::IncDec)
        } else if is_declaration(tokens)? {
            DeclarationNode::parse(tokens).map(From::from)
        } else {
            TestNode::parse(tokens).map(IndependentNode::Test)
        }
    }
}

fn is_assignment(tokens: &mut TokenList) -> ParseResult<bool> {
    let mut from = 0;
    loop {
        let var_size = TypeLikeNode::size_of_type(tokens, from)?;
        if var_size == 0 {
            let var_size = tokens.size_of_variable_at(from)?;
            return Ok(matches!(
                tokens.token_type_at(var_size)?,
                &TokenType::Assign(_)
            ));
        } else if let TokenType::Assign(_) = tokens.token_type_at(var_size)? {
            return Ok(true);
        } else if !matches!(tokens.token_type_at(var_size)?, TokenType::Name(_)) {
            return Ok(false);
        }
        let new_var_size = tokens.size_of_variable_at(var_size)?;
        if let TokenType::Comma = tokens.token_type_at(new_var_size)? {
            from = new_var_size + 1;
        } else {
            return Ok(matches!(
                tokens.token_type_at(new_var_size)?,
                TokenType::Assign(_)
            ));
        }
    }
}

fn is_normal_assignment(tokens: &mut TokenList) -> ParseResult<bool> {
    let mut from = 0;
    while TestNode::next_is_test(tokens)? {
        let var_size = tokens.size_of_variable_at(from)?;
        if let TokenType::Assign(_) = tokens.token_type_at(var_size)? {
            return Ok(true);
        } else if let TokenType::Comma = tokens.token_type_at(var_size)? {
            from = var_size + 1;
        } else {
            return Ok(false);
        }
    }
    Ok(false)
}

fn is_declaration(tokens: &mut TokenList) -> ParseResult<bool> {
    let var_size = TypeLikeNode::size_of_type(tokens, 0)?;
    Ok(var_size > 0
        && matches!(tokens.token_type_at(var_size)?, TokenType::Name(_))
        && matches!(tokens.token_type_at(var_size + 1)?, TokenType::Newline))
}

impl Lined for IndependentNode {
    fn line_info(&self) -> &LineInfo {
        match self {
            IndependentNode::Assert(a) => a.line_info(),
            IndependentNode::Assign(a) => a.line_info(),
            IndependentNode::AugAssign(a) => a.line_info(),
            IndependentNode::Break(b) => b.line_info(),
            IndependentNode::ClassDef(c) => c.line_info(),
            IndependentNode::Context(c) => c.line_info(),
            IndependentNode::Continue(c) => c.line_info(),
            IndependentNode::Declaration(d) => d.line_info(),
            IndependentNode::DeclaredAssign(d) => d.line_info(),
            IndependentNode::Defer(d) => d.line_info(),
            IndependentNode::Delete(d) => d.line_info(),
            IndependentNode::Derived(d) => d.line_info(),
            IndependentNode::Do(d) => d.line_info(),
            IndependentNode::Dotimes(d) => d.line_info(),
            IndependentNode::Enum(e) => e.line_info(),
            IndependentNode::For(f) => f.line_info(),
            IndependentNode::FunctionDef(f) => f.line_info(),
            IndependentNode::If(i) => i.line_info(),
            IndependentNode::Import(i) => i.line_info(),
            IndependentNode::IncDec(i) => i.line_info(),
            IndependentNode::Interface(i) => i.line_info(),
            IndependentNode::Method(m) => m.line_info(),
            IndependentNode::OpDef(o) => o.line_info(),
            IndependentNode::OpAssign(o) => o.line_info(),
            IndependentNode::Property(p) => p.line_info(),
            IndependentNode::Return(r) => r.line_info(),
            IndependentNode::Sync(s) => s.line_info(),
            IndependentNode::Test(t) => t.line_info(),
            IndependentNode::Try(t) => t.line_info(),
            IndependentNode::Typedef(t) => t.line_info(),
            IndependentNode::Union(u) => u.line_info(),
            IndependentNode::While(w) => w.line_info(),
            IndependentNode::With(w) => w.line_info(),
            IndependentNode::Yield(y) => y.line_info(),
        }
    }
}
