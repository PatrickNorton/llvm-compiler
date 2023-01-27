use crate::parser::error::{ParseResult, ParserError, ParserException, ParserInternalError};
use crate::parser::keyword::Keyword;
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::macros::parse_if_matches;
use crate::parser::token::{Token, TokenType};
use crate::parser::tokenizer::Tokenizer;
use std::cmp::Ordering;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenList {
    buffer: VecDeque<Token>,
    tokenizer: Tokenizer,
}

impl TokenList {
    /// Creates a new [`TokenList`].
    ///
    /// # Examples
    /// ```
    /// let tokenizer = Tokenizer::new("test", PathBuf::from("/"), 0);
    /// let token_list = TokenList::new(tokenizer);
    /// ```
    pub fn new(tokenizer: Tokenizer) -> TokenList {
        TokenList {
            buffer: VecDeque::new(),
            tokenizer,
        }
    }

    /// Removes and returns the next [token](Token) in the file.
    ///
    /// If trailing newlines should be ignored (i.e. within parentheses), use
    /// [`Self::next_tok`] instead.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// let next = token_list.next_token().unwrap();
    /// assert_eq!(next.token_type(), TokenType::Number(Number::Integer(123.into())));
    /// ```
    pub fn next_token(&mut self) -> ParseResult<Token> {
        match self.buffer.pop_front() {
            Option::Some(x) => Result::Ok(x),
            Option::None => self.tokenizer.tokenize_next(),
        }
    }

    /// Removes and returns the next [token](Token) in the file, possibly
    /// ignoring newlines.
    ///
    /// If you never want to ignore newlines, use [`Self::next_token`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123\n456\n\n789", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// // next_tok(false) is equivalent to next_token()
    /// let next = token_list.next_tok(false).unwrap();
    /// assert_eq!(next.token_type(), TokenType::Number(Number::Integer(123.into())));
    ///
    /// // Note that the newline still needs to be parsed here
    /// let next = token_list.next_tok(false).unwrap();
    /// assert_eq!(next.token_type(), TokenType::Newline);
    ///
    /// // next_tok(true) still returns the next token in the list
    /// let next = token_list.next_tok(true).unwrap();
    /// assert_eq!(next.token_type(), TokenType::Number(Number::Integer(456.into())));
    ///
    /// // However, trailing newlines are now skipped over
    /// let next = token_list.next_tok(true).unwrap();
    /// assert_eq!(next.token_type(), TokenType::Number(Number::Integer(789.into())));
    /// ```
    pub fn next_tok(&mut self, ignore_newlines: bool) -> ParseResult<Token> {
        let token = self.next_token()?;
        if ignore_newlines {
            self.pass_newlines()?;
        }
        Ok(token)
    }

    /// Parses the next token if the predicate is true, otherwise returns
    /// [`None`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// let next = token_list.next_if(|x| matches!(x.token_type(), TokenType::Newline)).unwrap();
    /// assert_eq!(next.as_ref().map(|x| x.token_type()), None);
    ///
    /// let next = token_list.next_if(|x| matches!(x.token_type(), TokenType::Number(_))).unwrap();
    /// assert_eq!(
    ///     next.as_ref().map(|x| x.token_type()),
    ///     Some(&TokenType::Number(Number::Integer(123.into()))),
    /// );
    /// ```
    pub fn next_if(
        &mut self,
        predicate: impl FnOnce(&Token) -> bool,
    ) -> ParseResult<Option<Token>> {
        self.ensure_length(1)?;
        if predicate(&self.buffer[0]) {
            self.next_token().map(Option::Some)
        } else {
            ParseResult::Ok(Option::None)
        }
    }

    /// If the predicate is true, parses the next token (possibly ignoring
    /// newlines), otherwise returns [`None`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123 456", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// // If the predicate is false, nothing is parsed (regardless of
    /// // ignore_newlines)
    /// let next = token_list.next_if_ignoring(
    ///     true,
    ///     |x| matches!(x.token_type(), TokenType::Newline)
    /// ).unwrap();
    /// assert_eq!(next.as_ref().map(|x| x.token_type()), None);
    ///
    /// // A true predicate results in the next token getting parsed
    /// let next = token_list.next_if_ignoring(
    ///     true,
    ///     |x| matches!(x.token_type(), TokenType::Number(_))
    /// ).unwrap();
    /// assert_eq!(
    ///     next.as_ref().map(|x| x.token_type()),
    ///     Some(&TokenType::Number(Number::Integer(123.into()))),
    /// );
    ///
    /// // If ignore_newlines is true, trailing newlines are parsed along with
    /// // the predicate token
    /// let next = token_list.next().unwrap();
    /// assert_eq!(next, TokenType::Number(Number::Integer(456.into())));
    /// ```
    pub fn next_if_ignoring(
        &mut self,
        ignore_newlines: bool,
        predicate: impl FnOnce(&Token) -> bool,
    ) -> ParseResult<Option<Token>> {
        self.ensure_length(1)?;
        if predicate(self.buffer.get(0).unwrap()) {
            self.next_tok(ignore_newlines).map(Option::Some)
        } else {
            ParseResult::Ok(Option::None)
        }
    }

    /// Parses & returns the next token if the text is equal to the given
    /// string.
    ///
    /// # String literals
    ///
    /// Note that this does not mean that the token is a
    /// [string](TokenType::String), but that the actual text that was written
    /// in the origin file is equal to the given string. Therefore, this will
    /// not match a string literal to its textual content (and probably
    /// shouldn't be used on string literals at all, because of the fact that
    /// escapes and newline concatenations have not been processed at this
    /// point, as well as the fact that there are two possible delimeters for
    /// string literals).
    ///
    /// # Alternatives
    ///
    /// - To ignore newlines, use [`Self::next_if_eq_ignore`].
    /// - To match the token type as opposed to the contents, use
    ///   [`parse_if_matches!`].
    /// - To return a simple error if the token does not match, use
    ///   [`Self::expect`].
    /// - To check without removing the token, use [`Self::token_equals`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     r#"123 "abc""#, PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// // If the text is not equal, the token list is left unmodified
    /// let next = token_list.next_if_equals("foo");
    /// assert_eq!(next, Ok(None))
    ///
    /// // If the text is equal to the token, it is parsed and returned
    /// let next = token_list.next_if_equals("123").unwrap();
    /// assert_eq!(
    ///     next.as_ref().map(|x| x.token_type()),
    ///     Some(&TokenType::Number(Number::Integer(123.into())))
    /// );
    ///
    /// // Note that this does *not* match the contents of string literals
    /// let next = token_list.next_if_equals("abc");
    /// assert_eq!(next, Ok(None));
    ///
    /// // If you want to match the contents of a string (probably a bad idea),
    /// // include the delimeters
    /// let next = token_list.next_if_equals(r#""abc""#).unwrap();
    /// assert_eq!(
    ///     next.as_ref().map(|x| x.token_type()),
    ///     Some(&TokenType::String("abc".into()))
    /// );
    /// ```
    pub fn next_if_equals(&mut self, text: impl AsRef<str>) -> ParseResult<Option<Token>> {
        self.next_if(|x| x.equals(text.as_ref()))
    }

    /// Parses & returns the next token if the text is equal to the given
    /// string, potentially also removing trailing newlines.
    ///
    /// # String literals
    ///
    /// This method does not compare the *contents* of string literal tokens,
    /// but instead uses the entire text of the token. For more information, see
    /// the documentation of [`Self::next_if_equals`].
    ///
    /// # Alternatives
    ///
    /// - To not ignore newlines, use [`Self::next_if_eq`].
    /// - To match the token type as opposed to the contents, use
    ///   [`parse_if_matches!`].
    /// - To return a simple error if the token does not match, use
    ///   [`Self::expect`].
    /// - To check without removing the token, use [`Self::token_equals`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     r#"123\n"abc""#, PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// // If the text is not equal, the token list is left unmodified
    /// let next = token_list.next_if_eq_ignore("foo", true);
    /// assert_eq!(next, Ok(None))
    ///
    /// // If the text is equal to the token, it is parsed and returned
    /// // When ignore_newlines is true, trailing newlines are parsed as well
    /// let next = token_list.next_if_eq_ignore("123", true).unwrap();
    /// assert_eq!(
    ///     next.as_ref().map(|x| x.token_type()),
    ///     Some(&TokenType::Number(Number::Integer(123.into())))
    /// );
    ///
    /// // Note that this does *not* match the contents of string literals
    /// let next = token_list.next_if_eq_ignore("abc", true);
    /// assert_eq!(next, Ok(None));
    ///
    /// // If you want to match the contents of a string (probably a bad idea),
    /// // include the delimeters
    /// let next = token_list.next_if_eq_ignore(r#""abc""#, true).unwrap();
    /// assert_eq!(
    ///     next.as_ref().map(|x| x.token_type()),
    ///     Some(&TokenType::String("abc".into()))
    /// );
    /// ```
    pub fn next_if_eq_ignore(
        &mut self,
        text: impl AsRef<str>,
        ignore_newlines: bool,
    ) -> ParseResult<Option<Token>> {
        self.next_if_ignoring(ignore_newlines, |x| x.equals(text.as_ref()))
    }

    /// Provides a reference to the first token in the list.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// let first = token_list.first().unwrap();
    /// assert_eq!(
    ///     first.token_type(),
    ///     TokenType::Number(Number::Integer(123.into()))
    /// );
    ///
    /// // Note that first() returns the same thing as the next call of
    /// // next_token()
    /// let next = token_list.next_token().unwrap();
    /// assert_eq!(
    ///     next.token_type(),
    ///     TokenType::Number(Number::Integer(123.into()))
    /// );
    /// ```
    #[inline]
    pub fn first(&mut self) -> ParseResult<&Token> {
        self.ensure_length(1)?;
        ParseResult::Ok(&self.buffer[0])
    }

    /// Provides a reference to the nth token in the list.
    ///
    /// Note that this does not ignore newlines (and there is no equivalent
    /// method that does at the moment).
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123\n456", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// let first = token_list.get_token(2).unwrap();
    /// assert_eq!(
    ///     first.token_type(),
    ///     TokenType::Number(Number::Integer(456.into()))
    /// );
    /// ```
    #[inline]
    pub fn get_token(&mut self, at: usize) -> ParseResult<&Token> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(&self.buffer[at])
    }

    /// Provides a reference to the [`TokenType`] of the first token in the
    /// list.
    ///
    /// This method is roughly equivalent to `self.first().token_type()`.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123\n456", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// let first_ty = token_list.token_type().unwrap();
    /// assert_eq!(first_ty, TokenType::Number(Number::Integer(123.into())));
    ///
    /// let first = token_list.first().unwrap().token_type();
    /// assert_eq!(first_ty, first);
    /// ```
    #[inline]
    pub fn token_type(&mut self) -> ParseResult<&TokenType> {
        ParseResult::Ok(self.first()?.token_type())
    }

    /// Provides a reference to the [`TokenType`] of the nth token in the list.
    ///
    /// This method is roughly equivalent to `self.get_token(at).token_type()`.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123\n456", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// let token_ty = token_list.token_type_at(2).unwrap();
    /// assert_eq!(first_ty, TokenType::Number(Number::Integer(456.into())));
    ///
    /// let token = token_list.get_token(2).unwrap().token_type();
    /// assert_eq!(token_ty, token);
    /// ```
    #[inline]
    pub fn token_type_at(&mut self, at: usize) -> ParseResult<&TokenType> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(self.buffer[at].token_type())
    }

    /// Returns `true` if the first token in the list has the same text as the
    /// given string.
    ///
    /// # Alternatives
    ///
    /// - To check a token at a specific location, use [`Self::token_eq_at`].
    /// - To check if a token is one of two options, use
    ///   [`Self::token_eq_either`].
    /// - To check if a token matches a pattern, use
    ///   `matches!(self.token_type()?, pattern)`
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     r#"123 "abc""#, PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.token_equals("123").unwrap());
    ///
    /// token_list.next_token().unwrap();
    ///
    /// // Note that this matches the entirety of a string literal, not just the
    /// // contents
    /// assert!(!token_list.token_equals("abc").unwrap());
    /// assert!(token_list.token_equals(r#""abc""#).unwrap());
    /// ```
    #[inline]
    pub fn token_equals(&mut self, text: impl AsRef<str>) -> ParseResult<bool> {
        ParseResult::Ok(self.first()?.equals(text.as_ref()))
    }

    /// Returns `true` if the nth token at the list has the same text as the
    /// given string.
    ///
    /// # Alternatives
    ///
    /// - To check the first token in the list, use [`Self::token_eq`].
    /// - To check if a token matches a pattern, use
    ///   `matches!(self.token_type_at(at)?, pattern)`
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     r#"123 456 "abc""#, PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.token_eq_at(1, "456").unwrap());
    ///
    /// // Passing 0 as the first argument makes this equivalent to
    /// // `token_equals`
    /// assert!(token_list.token_eq_at(0, "123").unwrap());
    ///
    /// // Note that this matches the entirety of a string literal, not just the
    /// // contents
    /// assert!(!token_list.token_eq_at(2, "abc").unwrap());
    /// assert!(token_list.token_eq_at(2, r#""abc""#).unwrap());
    /// ```
    #[inline]
    pub fn token_eq_at(&mut self, at: usize, text: impl AsRef<str>) -> ParseResult<bool> {
        self.ensure_length(at + 1)?;
        ParseResult::Ok(self.buffer[at].equals(text.as_ref()))
    }

    /// Returns `true` if the first token in the list has the same text as
    /// either argument.
    ///
    /// This is a two-argument version of [`Self::token_eq`], making it roughly
    /// equivalent to `self.token_eq(first) || self.token_eq(second)`.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     r#"123 "abc""#, PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.token_eq_either("123", "456").unwrap());
    /// assert!(token_list.token_eq_either("456", "123").unwrap());
    ///
    /// token_list.next_token().unwrap();
    ///
    /// // Note that this matches the entirety of a string literal, not just the
    /// // contents
    /// assert!(!token_list.token_eq_either("abc", "def").unwrap());
    /// assert!(token_list.token_eq_either(r#""abc""#, "def").unwrap());
    /// ```
    pub fn token_eq_either(
        &mut self,
        first: impl AsRef<str>,
        second: impl AsRef<str>,
    ) -> ParseResult<bool> {
        let value = self.first()?;
        ParseResult::Ok(value.equals(first.as_ref()) || value.equals(second.as_ref()))
    }

    /// Returns a [`ParserException`] with the given message and line info
    /// corresponding to the first element in the list.
    pub fn error(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(message.as_ref(), first))
    }

    /// Returns a [`ParserInternalError`] with the given message and line info
    /// corresponding to the first element in the list.
    pub fn internal_error(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Internal(ParserInternalError::of(message.as_ref(), first))
    }

    /// Returns a [`ParserException`] with a generic default message.
    ///
    /// The current message is "Unexpected [token]", but that may change. When
    /// possible, a more specific message will provide more meaning.
    pub fn default_error(&mut self) -> ParserError {
        self.error_with_first("Unexpected")
    }

    /// Returns a [`ParserException`] stating that the given string was
    /// excepted.
    pub fn error_expected(&mut self, expected: &str) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(
            format!("Expected {expected}, got {first:?}"),
            first,
        ))
    }

    /// Returns a [`ParserException`] with the debug representation of the
    /// offending token appended to the end.
    pub fn error_with_first(&mut self, message: impl AsRef<str>) -> ParserError {
        let first = match self.first() {
            Ok(x) => x,
            Err(e) => return e,
        };
        ParserError::Normal(ParserException::of(
            format!("{} {:?}", message.as_ref(), first),
            first,
        ))
    }

    /// If the first token has the same text as the given string, parses it and
    /// returns `()`, otherwise creates and returns an error.
    ///
    /// The `ignore_newlines` parameter allows trailing newlines to be parsed on
    /// a success.
    ///
    /// # Alternatives
    /// - For a better way to expect keywords, use [`Self::expect_keyword`].
    /// - To get the expected token, or to not construct an error on failure,
    ///   use [`Self::next_if_eq`] or [`Self::next_if_eq_ignore`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123 456", PathBuf::from("/"), 0
    /// ).unwrap();
    /// let tokenizer = Tokenizer::new("123\n456", PathBuf::from("/"), 0);
    /// let mut token_list = TokenList::new(tokenizer);
    ///
    /// assert!(token_list.expect("foo", false).is_err());
    ///
    /// assert!(token_list.expect("123", true).is_some());
    ///
    /// // `expect` parses the first token
    /// let first = token_list.first().unwrap();
    /// assert_eq!(first.token_type(), TokenType::Number(Number::Integer(123.into())));
    /// ```
    pub fn expect(&mut self, expected: &str, ignore_newlines: bool) -> ParseResult<()> {
        if !self.token_equals(expected)? {
            Err(self.error_expected(expected))
        } else {
            self.next_tok(ignore_newlines)?;
            ParseResult::Ok(())
        }
    }

    /// If the first token is a keyword, and that keyword is equal to
    /// `expected`, parses it and returns `()`, otherwise creates and
    /// returns an error.
    ///
    /// The `ignore_newlines` parameter allows trailing newlines to be parsed on
    /// a success.
    ///
    /// # Alternatives
    /// - For a better way to expect keywords, use [`Self::expect_keyword`].
    /// - To get the expected token, or to not construct an error on failure,
    ///   use [`Self::next_if_eq`] or [`Self::next_if_eq_ignore`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "if\nfor", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.expect_keyword(Keyword::While, false).is_err());
    ///
    /// assert!(token_list.expect(Keyword::If, true).is_some());
    ///
    /// // `expect` parses the first token
    /// let first = token_list.first().unwrap();
    /// assert_eq!(first.token_type(), TokenType::Keyword(Keyword::For));
    /// ```
    pub fn expect_keyword(&mut self, expected: Keyword, ignore_newlines: bool) -> ParseResult<()> {
        if !self.token_equals(expected.name())? {
            Err(self.error_expected(expected.name()))
        } else {
            self.next_tok(ignore_newlines)?;
            ParseResult::Ok(())
        }
    }

    /// Provides a reference to the [`LineInfo`] of the first token in the list.
    pub fn line_info(&mut self) -> ParseResult<&LineInfo> {
        self.first().map(Token::line_info)
    }

    /// Removes all leading newlines from the list.
    ///
    /// This is a component of several other methods, mostly those that take
    /// `ignore_newlines` parameters. In general, passing `true` to this
    /// parameter is the same as calling `pass_newlines()` immediately after.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "\n\n\n123", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// token_list.pass_newlines().unwrap();
    /// let next = token_list.next_token().unwrap();
    /// assert_eq!(next.token_type, TokenType::Number(Number::Integer(123.into())))
    /// ```
    pub fn pass_newlines(&mut self) -> ParseResult<()> {
        while matches!(self.token_type()?, TokenType::Newline) {
            self.next_token()?;
        }
        ParseResult::Ok(())
    }

    /// Returns `true` if the text given occurs as any one of the tokens within
    /// the braced clause at the begininng of this list.
    ///
    /// For more information on which tokens are part of a braced clause, see
    /// [`Self::brace_contains`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "(1 2 3 4)", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.brace_contains_str("2").unwrap());
    /// assert!(!token_list.brace_contains_str("5").unwrap());
    /// ```
    pub fn brace_contains_str(&mut self, text: impl AsRef<str>) -> ParseResult<bool> {
        let text = text.as_ref();
        self.brace_contains(|x| x.equals(text))
    }

    /// Returns `true` if the keyword given occurs as any one of the tokens
    /// within the braced clause at the beginning of this list.
    ///
    /// For more information on which tokens are part of a braced clause, see
    /// [`Self::brace_contains`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "(for 1 2 3)", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.brace_contains_kwd(Keyword::For).unwrap());
    /// assert!(!token_list.brace_contains_kwd(Keyword::If).unwrap());
    /// ```
    pub fn brace_contains_kwd(&mut self, kwd: Keyword) -> ParseResult<bool> {
        self.brace_contains(|x| x.is_kwd(kwd))
    }

    /// Returns `true` if any of the keywords given occur as any one of the
    /// tokens within the braced clause at the beginning of this list.
    ///
    /// For more information on which tokens are part of a braced clause, see
    /// [`Self::brace_contains`].
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "(for 1 2 3)", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// assert!(token_list.brace_contains_kwds([Keyword::For, Keyword::In]).unwrap());
    /// assert!(!token_list.brace_contains_kwds([Keyword::If, Keyword::Is]).unwrap());
    /// ```
    pub fn brace_contains_kwds<const N: usize>(&mut self, kwd: [Keyword; N]) -> ParseResult<bool> {
        self.brace_contains(|x| kwd.iter().any(|y| x.is_kwd(*y)))
    }

    /// Returns `true` if the predicate is `true` for any token within the
    /// brace.
    ///
    /// # Details
    ///
    /// If the first token in the list is not an opening brace, then `false` is
    /// returned. Any tokens within nested braces are *not* passed to the
    /// predicate. This continues until an equal number of opening and closing
    /// braces have been seen. If the end of the file is reached before all
    /// braces are closed, an error is returned. This does not check that the
    /// *types* of braces match, i.e. an open-paren can be closed by a square
    /// bracket without error.
    ///
    /// # Diagram
    ///
    /// Any `y` token in the below diagram is checked, while `n` tokens are not.
    ///
    /// ```text
    /// (y y (n [n n]) y) n
    /// ```
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "(foo bar [baz] quux) corge", PathBuf::from("/"), 0
    /// ).unwrap();
    ///
    /// for name in ["foo", "bar", "quux"] {
    ///     assert!(token_list.brace_contains(|x| x.equals(name)).unwrap());
    /// }
    ///
    /// // Nested braces are not checked
    /// assert!(!token_list.brace_contains(|x| x.equals("baz")).unwrap());
    ///
    /// // Things after the matching close-brace are not checked
    /// assert!(!token_list.brace_contains(|x| x.equals("corge").unwrap());
    /// ```
    pub fn brace_contains(&mut self, mut pred: impl FnMut(&Token) -> bool) -> ParseResult<bool> {
        let mut net_braces = 0;
        let mut index = 0;
        let first = self.first()?;
        if let TokenType::OpenBrace(_) = first.token_type() {
            index += 1;
            net_braces += 1;
        } else {
            return Ok(false);
        }
        while net_braces > 0 {
            let token = self.get_token(index)?;
            if net_braces == 1 && pred(token) {
                return Ok(true);
            } else {
                match token.token_type() {
                    TokenType::Epsilon => {
                        if net_braces > 0 {
                            return Err(self.error("Unmatched brace"));
                        } else {
                            return Ok(false);
                        }
                    }
                    TokenType::OpenBrace(o) => {
                        if net_braces == 0
                            && *o == '{'
                            && self.token_type_at(index - 1)?.precedes_literal_brace()
                        {
                            return Ok(false);
                        } else {
                            net_braces += 1;
                        }
                    }
                    TokenType::CloseBrace(_) => {
                        net_braces -= 1;
                        if net_braces == 0 {
                            return Ok(false);
                        }
                    }
                    _ => {}
                }
            }
            index += 1;
        }
        Ok(false)
    }

    /// Returns `true` if the brace at the beginning of the list is empty, or
    /// contains only newlines.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "()", PathBuf::from("/"), 0
    /// ).unwrap();
    /// assert!(token_list.brace_is_empty().unwrap());
    ///
    /// let mut token_list = Tokenizer::parse_str(
    ///     "(a)", PathBuf::from("/"), 0
    /// ).unwrap();
    /// assert!(!token_list.brace_is_empty().unwrap());
    ///
    /// let mut token_list = Tokenizer::parse_str(
    ///     "(\n\n\n)", PathBuf::from("/"), 0
    /// ).unwrap();
    /// assert!(token_list.brace_is_empty().unwrap());
    /// ```
    pub fn brace_is_empty(&mut self) -> ParseResult<bool> {
        assert!(matches!(self.token_type()?, &TokenType::OpenBrace(_)));
        let mut next = 1;
        while matches!(self.token_type_at(next)?, &TokenType::Newline) {
            next += 1;
        }
        ParseResult::Ok(matches!(
            self.token_type_at(next)?,
            &TokenType::CloseBrace(_)
        ))
    }

    /// Returns `true` if the predicate is true for any token in the current
    /// line.
    ///
    /// Like [`Self::brace_contains`], this does not check tokens in nested
    /// braces.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123 456 (789)\n012", PathBuf::from("/"), 0
    /// );
    ///
    /// assert!(token_list.line_contains(|x| x.equals("123")).unwrap());
    ///
    /// assert!(token_list.line_contains(|x| x.equals("456")).unwrap());
    ///
    /// assert!(!token_list.line_contains(|x| x.equals("789")).unwrap());
    ///
    /// assert!(!token_list.line_contains(|x| x.equals("012")).unwrap());
    /// ```
    pub fn line_contains(&mut self, mut pred: impl FnMut(&Token) -> bool) -> ParseResult<bool> {
        let mut net_braces = 0;
        let mut index = 0;
        let first = self.first()?;
        if pred(first) {
            return Ok(true);
        }
        match first.token_type() {
            TokenType::OpenBrace(_) => {
                index += 1;
                net_braces += 1;
            }
            TokenType::Newline | TokenType::Epsilon => return Ok(false),
            _ => {}
        }
        loop {
            let token = self.get_token(index)?;
            if net_braces == 0 && pred(token) {
                return Ok(true);
            } else {
                match token.token_type() {
                    TokenType::Epsilon => {
                        if net_braces > 0 {
                            return Err(self.error("Unmatched brace"));
                        } else {
                            return Ok(false);
                        }
                    }
                    TokenType::OpenBrace(o) => {
                        if net_braces == 0
                            && *o == '{'
                            && self.token_type_at(index - 1)?.precedes_literal_brace()
                        {
                            return Ok(false);
                        } else {
                            net_braces += 1;
                        }
                    }
                    TokenType::CloseBrace(_) => {
                        net_braces -= 1;
                    }
                    TokenType::Newline => {
                        if net_braces == 0 {
                            return Ok(false);
                        }
                    }
                    _ => {}
                }
            }
            index += 1;
        }
    }

    /// Returns the number of tokens in the variable at the beginning of the
    /// list.
    ///
    /// A "variable", in this context, is a dotted series of identifiers,
    /// function calls, and subscripts.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "foo.bar[baz]", PathBuf::from("/"), 0
    /// );
    ///
    /// assert_eq!(token_list.size_of_variable().unwrap(), 6);
    /// ```
    pub fn size_of_variable(&mut self) -> ParseResult<usize> {
        self.size_of_variable_at(0)
    }

    /// Returns the number of tokens in the variable beginning ta the nth token
    /// in the list.
    ///
    /// A "variable", in this context, is a dotted series of identifiers,
    /// function calls, and subscripts.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "123 foo.bar[baz]", PathBuf::from("/"), 0
    /// );
    ///
    /// assert_eq!(token_list.size_of_variable_at(1).unwrap(), 6);
    /// ```
    pub fn size_of_variable_at(&mut self, offset: usize) -> ParseResult<usize> {
        assert!(matches!(
            self.token_type_at(offset)?,
            TokenType::Name(_) | TokenType::OpenBrace(_)
        ));
        let mut net_braces = 0;
        let mut was_var = false;
        for size in offset.. {
            let token = self.get_token(size)?;
            match token.token_type() {
                TokenType::OpenBrace(_) => net_braces += 1,
                TokenType::CloseBrace(_) => {
                    if net_braces == 0 {
                        return Ok(size);
                    } else {
                        net_braces -= 1;
                    }
                }
                TokenType::Name(_) => {
                    if was_var && net_braces == 0 {
                        return Ok(size);
                    } else {
                        was_var = true;
                    }
                }
                TokenType::Dot(_) => {
                    was_var = false;
                }
                TokenType::Epsilon => match net_braces.cmp(&0) {
                    Ordering::Greater => {
                        return Err(ParserError::Normal(ParserException::of(
                            "Unmatched brace",
                            token,
                        )))
                    }
                    Ordering::Equal => return Ok(size),
                    Ordering::Less => {}
                },
                _ => {
                    if net_braces == 0 {
                        return Ok(size);
                    }
                }
            }
        }
        unreachable!("Infinite loop")
    }

    /// Parses a newline if it is the next token in the list, otherwise returns
    /// an error.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "\nfoo", PathBuf::from("/"), 0
    /// );
    ///
    /// // First token is a newline
    /// assert!(token_list.expect_newline().is_ok());
    ///
    /// // First token is now `foo`
    /// assert!(token_list.expect_newline().is_err());
    /// ```
    pub fn expect_newline(&mut self) -> ParseResult<()> {
        match parse_if_matches!(self, TokenType::Newline)? {
            Option::Some(_) => self.pass_newlines(),
            Option::None => Err(self.error("Expected newline")),
        }
    }

    /// The matching close brace to the given open brace.
    ///
    /// # Examples
    /// ```
    /// assert_eq!(TokenList::matching_brace('('), ')');
    /// assert_eq!(TokenList::matching_brace('['), ']');
    /// assert_eq!(TokenList::matching_brace('{'), '}');
    /// ```
    pub fn matching_brace(brace: char) -> char {
        match brace {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => panic!("Unknown brace {brace}"),
        }
    }

    /// The number of consecutive newlines beginning at `start`.
    ///
    /// # Examples
    /// ```
    /// let mut token_list = Tokenizer::parse_str(
    ///     "foo\n\nbar\n", PathBuf::from("/"), 0
    /// );
    ///
    /// assert_eq!(token_list.number_of_newlines(1).unwrap(), 2);
    /// assert_eq!(token_list.number_of_newlines(3).unwrap(), 0);
    /// assert_eq!(token_list.number_of_newlines(4).unwrap(), 1);
    /// ```
    pub fn number_of_newlines(&mut self, start: usize) -> ParseResult<usize> {
        let mut count = 0;
        while let TokenType::Newline = self.token_type_at(start + count)? {
            count += 1;
        }
        Ok(count)
    }

    fn ensure_length(&mut self, len: usize) -> ParseResult<()> {
        while self.buffer.len() < len {
            let token = self.tokenizer.tokenize_next()?;
            self.buffer.push_back(token)
        }
        ParseResult::Ok(())
    }
}
