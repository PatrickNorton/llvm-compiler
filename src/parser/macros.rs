/// Parses the first token from the given [`TokenList`] if the token type it
/// matches the given pattern.
///
/// The optional `$ignore` expression is whether or not newlines should be
/// ignored.
///
/// # Assumed types
/// This assumes that `$tokens` is an expression of type [`TokenList`].
/// Additionally, the matched expression is a [`TokenType`]. `$ignore` should
/// evaluate to a `bool`.
///
/// # Examples
/// ```
/// let mut tokens = Tokenizer::parse_str(
///     "123 while \n for", PathBuf::from("/"), 0
/// ).unwrap();
///
/// // If there is no match, the first token is not parsed.
/// assert_eq!(parse_if_matches!(tokens, TokenType::Newline).unwrap(), None);
///
/// // If there is a match, the first token is parsed.
/// assert!(matches!(
///     parse_if_matches!(tokens, TokenType::Number(_))
///         .unwrap()
///         .map(|x| x.token_type()),
///     Some(TokenType::Number(_))
/// ));
///
/// // An `if` clause, if given, must also be true in order to parse the next
/// // token.
/// assert_eq!(
///     parse_if_matches!(tokens, TokenType::Keyword(k) if k == Keyword::If)
///         .unwrap()
///         .map(|x| x.token_type()),
///     None
/// );
///
/// // If `$ignore` is given and true, any following newlines will also be
/// // parsed.
/// assert_eq!(
///     parse_if_matches!(tokens, true, TokenType::Keyword(k))
///         .unwrap()
///         .map(|x| x.token_type()),
///     Some(TokenType::Keyword(Keyword::While))
/// );
///
/// // Since `$ignore` was given in the last parse, the next token is not a
/// // newline.
/// assert_eq!(
///     tokens.next_token().unwrap().token_type(),
///     TokenType::Keyword(Keyword)
/// );
/// ```
macro_rules! parse_if_matches {
    ($tokens:expr, $pattern:pat $( if $guard:expr )?) => {
        match $tokens.token_type() {
            Result::Ok(x) => match x {
                $pattern $( if $guard )? => {
                    $tokens.next_token().map(Option::Some)
                },
                _ => Result::Ok(Option::None)
            }
            Result::Err(x) => Result::Err(x),
        }
    };

    ($tokens:expr, $ignore:expr, $pattern:pat $( if $guard:expr )?) => {
        match $tokens.token_type() {
            Result::Ok(x) => match x {
                $pattern $( if $guard )? => {
                    $tokens.next_tok($ignore).map(Option::Some)
                },
                _ => Result::Ok(Option::None)
            }
            Result::Err(x) => Result::Err(x),
        }
    };
}

/// Returns whether or not the current line contains a token whose token type
/// matches the given pattern.
///
/// For more information, and for a more general method, see
/// [`TokenList::line_contains`].
///
/// # Examples
/// ```
/// let mut tokens = Tokenizer::parse_str(
///     "foo 123 baz \n while", PathBuf::from("/"), 0
/// ).unwrap();
///
/// assert!(line_matches!(tokens, TokenType::Number(_)));
/// assert!(!line_matches!(tokens, TokenType::OpenBrace(_)));
/// // The first keyword in the list is after the newline, so it is not checked.
/// assert!(!line_matches!(tokens, TokenType::Keyword(_)));
/// ```
macro_rules! line_matches {
    ($tokens:expr, $pattern:pat) => {
        $tokens.line_contains(|x| matches!(x.token_type(), $pattern))
    };
}

pub(crate) use {line_matches, parse_if_matches};

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use num::ToPrimitive;

    use crate::parser::token::TokenType;
    use crate::parser::token_list::TokenList;
    use crate::parser::tokenizer::Tokenizer;

    fn create_token_list(text: &str) -> TokenList {
        Tokenizer::parse_str(text, PathBuf::from(file!()), 0).expect("Tokenizer should not fail")
    }

    #[test]
    fn parse_if_success() {
        let mut tokens = create_token_list("123");
        assert!(parse_if_matches!(tokens, TokenType::Number(_))
            .unwrap()
            .is_some());
    }

    #[test]
    fn parse_if_no_match() {
        let mut tokens = create_token_list("abc");
        assert!(parse_if_matches!(tokens, TokenType::Number(_))
            .unwrap()
            .is_none());
    }

    #[test]
    fn parse_if_conditional() {
        let mut tokens = create_token_list("123");
        assert!(
            parse_if_matches!(tokens, TokenType::Number(x) if x.to_u32() == Some(50))
                .unwrap()
                .is_none()
        );
        assert!(
            parse_if_matches!(tokens, TokenType::Number(x) if x.to_u32() == Some(123))
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn parse_if_with_ignore() {
        let mut tokens = create_token_list("123 \n\n 456 \n 7");
        assert!(parse_if_matches!(tokens, true, TokenType::Keyword(_))
            .unwrap()
            .is_none());
        assert!(parse_if_matches!(tokens, true, TokenType::Number(_))
            .unwrap()
            .is_some());
        assert!(parse_if_matches!(tokens, false, TokenType::Number(_))
            .unwrap()
            .is_some());
        assert_eq!(tokens.token_type().unwrap(), &TokenType::Newline);
    }
}
