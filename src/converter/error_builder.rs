use std::fmt::{Display, Write};

use crate::parser::line_info::{LineInfo, Lined};

/// A struct for building detailed error messages.
///
/// This is meant to be used in conjunction with the `from_builder` methods on
/// various error types, as well as [`warning::warn_builder`]. It is a more
/// flexible and more detailed version of the original error constructor
/// methods, which required a different overload for each set of parameters.
/// This allows effort to be shared between different error types, as well as
/// ensuring they stay consistent.
///
/// # Examples
/// ```
/// let builder = ErrorBuilder::new(&LineInfo::empty())
///     .with_message("Test error")
///     .with_note("This is an example error")
///
/// let error = CompilerException::from_builder(builder);
///
/// let double_def = ErrorBuilder::double_def("foo", &LineInfo::empty(), &LineInfo::empty());
///
/// let double_def_err = CompilerInternalError::from_builder(builder);
/// ```
#[must_use]
pub struct ErrorBuilder<'a> {
    line_info: ErrorLineInfo<'a>,
    message: Option<Box<dyn Display + 'a>>,
    notes: Vec<Box<dyn Display + 'a>>,
    value_def: Option<ValueDef<'a>>,
}

/// The type of error; used in building error messages.
///
/// Each error type corresponds to a class of error. Its only use is in the
/// [`get_message`](ErrorBuilder::get_message) function.
///
/// # Error correspondence
/// - [`Self::Standard`]: [`CompilerException`]
/// - [`Self::Internal`]: [`CompilerInternalError`]
/// - [`Self::Todo`]: [`CompilerTodoError`]
/// - [`Self::Warning`]: [`warning::warn`]
///
/// # Examples
/// ```
/// // Error message will start with "Error: "
/// let standard = ErrorBuilder::new(LineInfo::empty())
///     .get_message(ErrorType::Standard);
///
/// // Error message will start with "Warning: "
/// let warning = ErrorBuilder::new(LineInfo::empty())
///     .get_message(ErrorType::Warning);
/// ```
#[derive(Debug, Copy, Clone)]
pub enum ErrorType {
    Standard,
    Internal,
    Todo,
    Warning,
}

#[derive(Debug)]
enum ErrorLineInfo<'a> {
    Standard(&'a LineInfo),
    DoubleDef(&'a LineInfo, &'a LineInfo),
}

struct ValueDef<'a> {
    name: Box<dyn Display + 'a>,
    line_info: &'a LineInfo,
}

impl<'a> ErrorBuilder<'a> {
    /// Create a new `ErrorBuilder` with the same `LineInfo` as the given value.
    ///
    /// # Examples
    /// ```
    /// let builder = ErrorBuilder::new(&LineInfo::empty());
    /// ```
    pub fn new(line_info: &'a impl Lined) -> Self {
        Self {
            line_info: ErrorLineInfo::Standard(line_info.line_info()),
            message: None,
            notes: Vec::new(),
            value_def: None,
        }
    }

    /// Create a new ErrorBuilder for double-definition errors.
    ///
    /// This method is different from `ErrorBuilder::new` because a
    /// double-definition error has two `LineInfo`s of roughly equal prominence
    /// (those of the two definitions), whereas most errors only have one.
    /// Additionally, this method has an implicit `with_message` call in it; the
    /// message is set to a standard double-definition message based on the
    /// given name.
    ///
    /// # Examples
    /// ```
    /// let builder = ErrorBuilder::double_def("foo", LineInfo::empty_ref(), LineInfo::empty_ref());
    /// ```
    pub fn double_def(name: impl Display, info_1: &'a impl Lined, info_2: &'a impl Lined) -> Self {
        Self {
            line_info: ErrorLineInfo::DoubleDef(info_1.line_info(), info_2.line_info()),
            message: Some(Box::new(format!("Name '{}' defined twice", name))),
            notes: Vec::new(),
            value_def: None,
        }
    }

    /// Get the error message from the builder as a string.
    ///
    /// The actual error message is not guaranteed to be stable, as it may be
    /// improved as time goes on. However, it is guaranteed to contain the given
    /// message, as well as any notes that may have been passed. The `ty` parameter
    /// on this method refers to the type of error that is constructing this; see
    /// [`ErrorType`] for more details.
    ///
    /// # Examples
    /// ```
    /// let message = ErrorBuilder::new(LineInfo::empty_ref())
    ///     .with_message("Test error message")
    ///     .with_note("Test note")
    ///     .get_message(ErrorType::Standard);
    ///
    /// assert!(message.contains("Test error message"));
    /// assert!(message.contains("Test note"));
    /// ```
    pub fn get_message(&self, ty: ErrorType) -> String {
        let mut msg = format!("{}: {}", ty.message(), self.true_message());
        for note in &self.notes {
            write!(msg, "\nNote: {}", note).unwrap();
        }
        match &self.line_info {
            ErrorLineInfo::Standard(s) => write!(
                msg,
                "\n{}: File {} Line {}\n{}",
                ty.secondary_msg(),
                s.get_path().display(),
                s.get_line_number(),
                s.info_string(),
            )
            .unwrap(),
            ErrorLineInfo::DoubleDef(l1, l2) => {
                write!(
                    msg,
                    "\nDefinition 1: File {} Line {}\n{}",
                    l1.get_path().display(),
                    l1.get_line_number(),
                    l1.info_string(),
                )
                .unwrap();
                write!(
                    msg,
                    "\nDefinition 2: File {} Line {}\n{}",
                    l2.get_path().display(),
                    l2.get_line_number(),
                    l2.info_string(),
                )
                .unwrap();
            }
        }
        if let Option::Some(value_def) = &self.value_def {
            write!(msg, "\nNote: Value {} defined here:", value_def.name).unwrap();
            write!(
                msg,
                "\nFile {} Line {}\n{}",
                value_def.line_info.get_path().display(),
                value_def.line_info.get_line_number(),
                value_def.line_info.info_string(),
            )
            .unwrap();
        }
        msg
    }

    /// Sets the message on the given ErrorBuilder.
    ///
    /// This takes `self` by value because it is intended to be used in
    /// method-chaining style.
    ///
    /// If `with_message` is called multiple times on the same object, it will
    /// replace the previous message with the new one.
    ///
    /// # Examples
    /// ```
    /// let builder = ErrorBuilder::new(LineInfo::empty_ref())
    ///     .with_message("This error now has a message");
    /// ```
    #[inline]
    pub fn with_message(mut self, message: impl Display + 'a) -> Self {
        self.message = Some(Box::new(message));
        self
    }

    /// Adds a note to the given ErrorBuilder.
    ///
    /// This takes `self` by value because it is intended to be used in
    /// method-chaining style.
    ///
    /// This method can be called multiple times on the same object without
    /// issue; all notes are guaranteed to be printed on final display of the
    /// error.
    ///
    /// # Examples
    /// ```
    /// let builder = ErrorBuilder::new(LineInfo::empty_ref())
    ///     .with_note("This error now has a note")
    ///     .with_note("Errors can have multiple notes at the same time")
    /// ```
    #[inline]
    pub fn with_note(mut self, note: impl Display + 'a) -> Self {
        self.notes.push(Box::new(note));
        self
    }

    /// Adds a value definition to the given ErrorBuilder.
    ///
    /// This takes `self` by value because it is intended to be used in
    /// method-chaining style.
    ///
    /// This method is used when an error whishes to refer to the definition of
    /// some variable (or similar) to provide more context to the error. It is
    /// similar to a note, but has an attached `LineInfo`, and the message is
    /// automatically generated from the given name.
    ///
    /// # Examples
    /// ```
    /// let builder = ErrorBuilder::new(LineInfo::empty_ref())
    ///     .with_value_def("foo", LineInfo::empty_ref());
    /// ```
    #[inline]
    pub fn with_value_def(mut self, name: impl Display + 'a, line_info: &'a impl Lined) -> Self {
        self.value_def = Some(ValueDef {
            name: Box::new(name),
            line_info: line_info.line_info(),
        });
        self
    }

    /// Adds a value definition to the given ErrorBuilder if the given
    /// [`LineInfo`] is not empty.
    ///
    /// This takes self by value because it is intended to be used in
    /// method-chaining style.
    ///
    /// # See also
    ///
    /// [`Self::with_value_def()`], which always adds the value definition, even
    /// if the given [`LineInfo`] is empty.
    ///
    /// # Examples
    /// ```
    /// let builder = ErrorBuilder::new(LineInfo::empty_ref())
    ///     .try_value_def("foo", LineInfo::empty_ref());
    /// ```
    #[inline]
    pub fn try_value_def(self, name: impl Display + 'a, line_info: &'a impl Lined) -> Self {
        let line_info = line_info.line_info();
        if line_info.is_empty() {
            self
        } else {
            self.with_value_def(name, line_info)
        }
    }

    fn true_message(&self) -> impl Display + '_ {
        static DEFAULT_MESSAGE: &str = "Unknown error";
        self.message.as_deref().unwrap_or(&DEFAULT_MESSAGE)
    }
}

impl ErrorType {
    const fn message(&self) -> &'static str {
        match self {
            ErrorType::Standard => "Error",
            ErrorType::Internal => "Internal error",
            ErrorType::Todo => "Operation not yet implemented",
            ErrorType::Warning => "Warning",
        }
    }

    const fn secondary_msg(&self) -> &'static str {
        match self {
            ErrorType::Standard | ErrorType::Internal | ErrorType::Todo => "Error",
            ErrorType::Warning => "Warning",
        }
    }
}
