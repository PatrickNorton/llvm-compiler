use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::stmt_body::StatementBodyNode;
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;
use crate::parser::type_node::TypeNode;
use crate::parser::variable::VariableNode;

#[derive(Debug)]
pub struct TryStatementNode {
    line_info: LineInfo,
    body: StatementBodyNode,
    except: StatementBodyNode,
    excepted: Vec<TypeNode>,
    as_var: VariableNode,
    else_stmt: StatementBodyNode,
    finally_stmt: StatementBodyNode,
}

impl TryStatementNode {
    pub fn new(
        line_info: LineInfo,
        body: StatementBodyNode,
        except: StatementBodyNode,
        excepted: Vec<TypeNode>,
        as_var: VariableNode,
        else_stmt: StatementBodyNode,
        finally_stmt: StatementBodyNode,
    ) -> Self {
        Self {
            line_info,
            body,
            except,
            excepted,
            as_var,
            else_stmt,
            finally_stmt,
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<TryStatementNode> {
        let (info, tok) = tokens.next_token()?.deconstruct();
        assert!(matches!(tok, TokenType::Keyword(Keyword::Try)));
        let body = StatementBodyNode::parse(tokens)?;
        let mut except = StatementBodyNode::empty();
        let mut excepted = Vec::new();
        let mut as_stmt = VariableNode::empty();
        let mut else_stmt = StatementBodyNode::empty();
        if let TokenType::Keyword(Keyword::Except) = tokens.token_type()? {
            tokens.next_token()?;
            excepted = TypeNode::parse_list(tokens, false)?;
            as_stmt = VariableNode::parse_on_keyword(tokens, Keyword::As)?;
            except = StatementBodyNode::parse(tokens)?;
            else_stmt = StatementBodyNode::parse_on_keyword(tokens, Keyword::Else)?;
        }
        let finally_stmt = StatementBodyNode::parse_on_keyword(tokens, Keyword::Finally)?;
        if except.is_empty() && excepted.is_empty() && finally_stmt.is_empty() {
            Err(tokens.error("Try statement must have either an except or finally clause"))
        } else {
            Ok(TryStatementNode::new(
                info,
                body,
                except,
                excepted,
                as_stmt,
                else_stmt,
                finally_stmt,
            ))
        }
    }
}

impl Lined for TryStatementNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
