use std::fmt::Display;

use crate::parser::base::IndependentNode;
use crate::parser::dotted::{DotPrefix, DottedVariableNode};
use crate::parser::error::ParseResult;
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::token::TokenType;
use crate::parser::token_list::TokenList;

#[derive(Debug)]
pub struct ImportExportNode {
    import_type: ImportExportType,
    line_info: LineInfo,
    values: IEValue,
    from: DottedVariableNode,
    pre_dots: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportExportType {
    Import,
    Export,
    Typeget,
}

#[derive(Debug)]
pub enum IEValue {
    Standard(StandardNames),
    Wildcard,
}

#[derive(Debug)]
pub struct StandardNames {
    ports: Vec<DottedVariableNode>,
    as_stmt: Option<Vec<DottedVariableNode>>,
}

impl ImportExportNode {
    pub fn new(
        import_type: ImportExportType,
        line_info: LineInfo,
        ports: Vec<DottedVariableNode>,
        from: DottedVariableNode,
        as_stmt: Option<Vec<DottedVariableNode>>,
        pre_dots: usize,
    ) -> Self {
        Self {
            import_type,
            line_info,
            values: IEValue::Standard(StandardNames { ports, as_stmt }),
            from,
            pre_dots,
        }
    }

    pub fn new_wildcard(
        import_type: ImportExportType,
        line_info: LineInfo,
        from: DottedVariableNode,
        pre_dots: usize,
    ) -> Self {
        Self {
            import_type,
            line_info,
            values: IEValue::Wildcard,
            from,
            pre_dots,
        }
    }

    pub fn get_type(&self) -> ImportExportType {
        self.import_type
    }

    pub fn get_from(&self) -> &DottedVariableNode {
        &self.from
    }

    pub fn get_values(&self) -> &[DottedVariableNode] {
        match &self.values {
            IEValue::Standard(s) => &s.ports,
            IEValue::Wildcard => panic!("Should not be getting values from wildcard"),
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.values, IEValue::Wildcard)
    }

    pub fn get_as(&self) -> Option<&[DottedVariableNode]> {
        match &self.values {
            IEValue::Standard(s) => s.as_stmt.as_deref(),
            IEValue::Wildcard => panic!("Should not be getting values from wildcard"),
        }
    }

    pub fn get_pre_dots(&self) -> usize {
        self.pre_dots
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ImportExportNode> {
        let (info, pre_dots, from) =
            if let TokenType::Keyword(Keyword::From) = tokens.token_type()? {
                let (info, _) = tokens.next_token()?.deconstruct();
                (
                    info,
                    Self::parse_pre_dots(tokens)?,
                    DottedVariableNode::parse_on_name(tokens)?,
                )
            } else {
                (tokens.line_info()?.clone(), 0, DottedVariableNode::empty())
            };
        let import_type = ImportExportType::parse(tokens)?;
        if let TokenType::Newline | TokenType::Epsilon = tokens.token_type()? {
            return Err(tokens.error(format!(
                "Empty {} statements are illegal",
                import_type.name()
            )));
        }
        if tokens.token_equals("*")? {
            tokens.next_token()?;
            if let TokenType::Keyword(Keyword::As) = tokens.token_type()? {
                Err(tokens.error("Cannot use 'as' with wildcard"))
            } else {
                Ok(ImportExportNode::new_wildcard(
                    import_type,
                    info,
                    from,
                    pre_dots,
                ))
            }
        } else {
            let imports = DottedVariableNode::parse_names_only_list(tokens)?;
            if let TokenType::Keyword(Keyword::As) = tokens.token_type()? {
                tokens.next_token()?;
                let as_stmt = DottedVariableNode::parse_names_only_list(tokens)?;
                Ok(ImportExportNode::new(
                    import_type,
                    info,
                    imports,
                    from,
                    Some(as_stmt),
                    pre_dots,
                ))
            } else {
                Ok(ImportExportNode::new(
                    import_type,
                    info,
                    imports,
                    from,
                    None,
                    pre_dots,
                ))
            }
        }
    }

    fn parse_pre_dots(tokens: &mut TokenList) -> ParseResult<usize> {
        let mut dot_count = 0;
        loop {
            match tokens.token_type()? {
                TokenType::Ellipsis => {
                    dot_count += 3;
                    tokens.next_token()?;
                }
                TokenType::Dot(DotPrefix::None) => {
                    dot_count += 1;
                    tokens.next_token()?;
                }
                _ => break,
            }
        }
        Ok(dot_count)
    }
}

impl ImportExportType {
    pub const fn name(&self) -> &'static str {
        match self {
            ImportExportType::Import => "import",
            ImportExportType::Export => "export",
            ImportExportType::Typeget => "typeget",
        }
    }

    pub fn parse(tokens: &mut TokenList) -> ParseResult<ImportExportType> {
        let token = match tokens.token_type()? {
            TokenType::Keyword(Keyword::Import) => ImportExportType::Import,
            TokenType::Keyword(Keyword::Export) => ImportExportType::Export,
            TokenType::Keyword(Keyword::Typeget) => ImportExportType::Typeget,
            _ => panic!(
                "Expected import keyword\n{}",
                tokens.line_info()?.info_string()
            ),
        };
        tokens.next_token()?;
        Ok(token)
    }
}

impl Display for ImportExportType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl Lined for ImportExportNode {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl<'a> TryFrom<&'a IndependentNode> for &'a ImportExportNode {
    type Error = ();

    fn try_from(value: &'a IndependentNode) -> Result<Self, Self::Error> {
        match value {
            IndependentNode::Import(i) => Ok(i),
            _ => Err(()),
        }
    }
}
