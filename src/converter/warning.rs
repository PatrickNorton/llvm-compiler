use std::collections::HashSet;
use std::fmt::Display;
use std::str::FromStr;
use std::sync::Arc;

use crate::converter::error_builder::ErrorType;
use crate::parser::line_info::Lined;
use crate::util::error_counter::ErrorCounter;

use super::compiler_info::CompilerInfo;
use super::error::CompilerException;
use super::error_builder::ErrorBuilder;
use super::CompileResult;

/// The class representing the different types of warning.
// TODO: Longer write-up
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WarningType {
    NoType,
    Deprecated,
    Unused,
    TrivialValue,
    Unreachable,
    InfiniteLoop,
    ZeroDivision,
    IncompleteSwitch,
    Todo,
}

/// A struct to hold the current warning state.
///
/// The warning state consists of two distinct pieces: the set of forbidden
/// warnings and the error count, the latter of which is shared across all
/// threads.
///
/// The set of warnings is implemented as a stack, where each item in the stack
/// has a list of all the changes to the what warnings are
/// allowed/forbidden/etc. That way, nested `$allow` declarations can be
/// supported.
#[derive(Debug)]
pub struct WarningHolder {
    levels: Vec<WarningFrame>,
    counter: Arc<ErrorCounter>,
}

#[derive(Debug)]
struct WarningFrame {
    level: FrameType,
    values: HashSet<WarningType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FrameType {
    Allow,
    Warn,
    Deny,
    Forbid,
}

/// The different levels a lint can take.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WarningLevel {
    /// An allowed lint.
    ///
    /// A lint that is listed as `Allow` provides no feedback when triggered.
    /// Lints can be allowed by the `$allow` annotation. Note that the `$allow`
    /// annotation is itself not allowed inside code governed by a `$forbid`
    /// annotation.
    Allow,

    /// A warned lint.
    ///
    /// Most lints are `Warn` by default. A lint that is `Warn` prints out a
    /// message when triggered, but does not stop compilation or prevent it from
    /// succeeding. Lints can be explicitly warned by the `$warn` annotation.
    /// Note that the `$warn` annotation is itself not allowed inside code
    /// governed by a `$forbid` annotation.
    Warn,

    /// A denied lint.
    ///
    /// A lint that is `Deny` turns it into a hard error, specifically a
    /// `CompilerException`. This does stop compilation fully. Lints can be
    /// specifically denied by the `$deny` annotation. In addition, the
    /// `$forbid` annotation is similar to `$deny`, but prevents it from being
    /// overridden by an `$allow` or `$warn` nested more deeply in the code.
    Deny,
}

/// The simplest function to output a warning.
///
/// This function takes a message, a warning type, a `CompilerInfo`, and the
/// line info of the warned value. From there, it determines the correct action,
/// builds the warning object, and does the correct action.
///
/// # See also
/// [`warn_note`]
/// [`warn_if`]
/// [`warn_builder`]
/// [`warn_counter`]
pub fn warn(
    message: impl Display,
    warn: WarningType,
    info: &CompilerInfo,
    line_info: impl Lined,
) -> CompileResult<()> {
    warn_if(message, warn, info.warning_holder(), line_info)
}

/// Prints a warning with a note.
///
/// This is similar in function to [`warn`], but it also displays a note along
/// with the rest of the warning.
///
/// # See also
/// [`warn`]
/// [`warn_if`]
/// [`warn_builder`]
/// [`warn_counter`]
pub fn warn_note(
    message: impl Display,
    note: impl Display,
    warn: WarningType,
    info: &CompilerInfo,
    line_info: impl Lined,
) -> CompileResult<()> {
    warn_builder(
        ErrorBuilder::new(&line_info)
            .with_message(message)
            .with_note(note),
        warn,
        info.warning_holder(),
    )
}

/// Displays a warning, but does not require a `CompilerInfo`.
///
/// This function does the same thing as [`warn`], but takes slightly different
/// arguments. It exists for convienence, since the vast majority of the time
/// that [`warn`] is called, there is only a [`CompilerInfo`] in scope and the
/// code would have to use `info.warning_holder()` instead, which is a little
/// unwieldy.
///
/// # See also
/// [`warn`]
/// [`warn_note`]
/// [`warn_builder`]
/// [`warn_counter`]
pub fn warn_if(
    message: impl Display,
    warn: WarningType,
    holder: &WarningHolder,
    line_info: impl Lined,
) -> CompileResult<()> {
    warn_builder(
        ErrorBuilder::new(&line_info).with_message(message),
        warn,
        holder,
    )
}

/// Displays a warning from a builder.
///
/// This is the function from the `warn*` family that implements the
/// [`ErrorBuilder`] API.
///
/// # See also
/// [`warn`]
/// [`warn_note`]
/// [`warn_if`]
/// [`warn_counter`]
pub fn warn_builder(
    builder: ErrorBuilder<'_>,
    warn: WarningType,
    holder: &WarningHolder,
) -> CompileResult<()> {
    warn_level(builder, warn, holder.warning_level(warn), &holder.counter)
}

/// Displays a warning only using an [`ErrorCounter`].
///
/// This is intended for use in the very few places where lexical error changes
/// (such as `$allow`) don't make sense, e.g. loading distinct files. Because of
/// this, this function doesn't take a [`WarningHolder`] as an argument, only an
/// [`ErrorCounter`], which is accessible from a [`GlobalCompilerInfo`] instance
/// and therefore is accessible even outside of a single file. Because of these
/// restrictions, it will always warn on its input, and never throw an error.
/// Note that this function is quite rarely warranted; one of the other
/// functions in its family should be used instead.
pub fn warn_counter(builder: ErrorBuilder<'_>, counter: &ErrorCounter) -> CompileResult<()> {
    warn_level(builder, WarningType::NoType, WarningLevel::Warn, counter)
}

fn warn_level(
    builder: ErrorBuilder<'_>,
    warn: WarningType,
    level: WarningLevel,
    counter: &ErrorCounter,
) -> CompileResult<()> {
    match level {
        WarningLevel::Allow => Ok(()),
        WarningLevel::Warn => {
            counter.add_warning();
            eprintln!("{}", builder.get_message(ErrorType::Warning));
            Ok(())
        }
        WarningLevel::Deny => {
            counter.add_error();
            if let Option::Some(warn_name) = warn.annotation_name() {
                Err(CompilerException::from_builder(builder.with_note(format!(
                    "Error because of $deny({}) or $deny(all)",
                    warn_name
                )))
                .into())
            } else {
                Err(CompilerException::from_builder(
                    builder.with_note("Error because of $deny(all)"),
                )
                .into())
            }
        }
    }
}

impl WarningHolder {
    /// Creates a new [`WarningHolder`].
    pub fn new(counter: Arc<ErrorCounter>) -> Self {
        Self {
            levels: Vec::new(),
            counter,
        }
    }

    /// Gets the warning level of the given [`WarningType`].
    ///
    /// # Examples
    ///
    /// ```
    /// let mut holder = WarningHolder::new(Arc::new(ErrorCounter::new()));
    /// holder.allow(HashSet::from([WarningType::Deprecated]));
    /// holder.warn(HashSet::from([WarningType::TrivialValue]));
    /// holder.deny(HashSet::from([WarningType::InfiniteLoop]));
    /// holder.forbid(HashSet::from([WarningType::IncompleteSwitch]));
    ///
    /// assert_eq!(holder.warning_level(WarningType::Deprecated), WarningLevel::Allow);
    /// assert_eq!(holder.warning_level(WarningType::TrivialValue), WarningLevel::Warn);
    /// assert_eq!(holder.warning_level(WarningType::InfiniteLoop), WarningLevel::Deny);
    /// assert_eq!(holder.warning_level(WarningType::IncompleteSwitch), WarningLevel::Deny);
    ///
    /// // Further calls override previous ones
    /// holder.deny(HashSet::from([WarningType::Deprecated]));
    /// assert_eq!(holder.warning_level(WarningType::Deprecated), WarningLevel::Deny);
    ///
    /// // Calling pop_warnings undoes the most recent call
    /// holder.pop_warnings();
    /// assert_eq!(holder.warning_level(WarningType::Deprecated), WarningLevel::Allow);
    /// ```
    pub fn warning_level(&self, warning: WarningType) -> WarningLevel {
        if self.levels.is_empty() {
            return warning.default_level();
        }
        for frame in self.levels.iter().rev() {
            if frame.values.contains(&warning) {
                return match frame.level {
                    FrameType::Allow => WarningLevel::Allow,
                    FrameType::Warn => WarningLevel::Warn,
                    FrameType::Deny | FrameType::Forbid => WarningLevel::Deny,
                };
            }
        }
        warning.default_level()
    }

    /// Pops one level of warning from the warning stack.
    pub fn pop_warnings(&mut self) {
        self.levels.pop();
    }

    pub fn allow(&mut self, allowed: HashSet<WarningType>) {
        debug_assert!(!allowed.iter().any(|&x| self.is_forbidden(x)));
        self.levels.push(WarningFrame {
            level: FrameType::Allow,
            values: allowed,
        })
    }

    pub fn warn(&mut self, warned: HashSet<WarningType>) {
        debug_assert!(!warned.iter().any(|&x| self.is_forbidden(x)));
        self.levels.push(WarningFrame {
            level: FrameType::Warn,
            values: warned,
        })
    }

    pub fn deny(&mut self, denied: HashSet<WarningType>) {
        self.levels.push(WarningFrame {
            level: FrameType::Deny,
            values: denied,
        })
    }

    pub fn forbid(&mut self, forbidden: HashSet<WarningType>) {
        self.levels.push(WarningFrame {
            level: FrameType::Forbid,
            values: forbidden,
        })
    }

    pub fn allow_all(&mut self) {
        self.allow(WARNING_TYPES.iter().cloned().collect())
    }

    pub fn warn_all(&mut self) {
        self.warn(WARNING_TYPES.iter().cloned().collect())
    }

    pub fn deny_all(&mut self) {
        self.deny(WARNING_TYPES.iter().cloned().collect())
    }

    pub fn forbid_all(&mut self) {
        self.forbid(WARNING_TYPES.iter().cloned().collect())
    }

    pub fn is_forbidden(&self, forbidden: WarningType) -> bool {
        self.levels
            .iter()
            .any(|lvl| lvl.level == FrameType::Forbid && lvl.values.contains(&forbidden))
    }
}

pub const WARNING_TYPES: &[WarningType] = &[
    WarningType::NoType,
    WarningType::Deprecated,
    WarningType::Unused,
    WarningType::TrivialValue,
    WarningType::Unreachable,
    WarningType::InfiniteLoop,
    WarningType::ZeroDivision,
    WarningType::IncompleteSwitch,
    WarningType::Todo,
];

impl WarningType {
    pub const fn annotation_name(&self) -> Option<&'static str> {
        match self {
            WarningType::NoType => None,
            WarningType::Deprecated => Some("deprecated"),
            WarningType::Unused => Some("unused"),
            WarningType::TrivialValue => Some("trivial"),
            WarningType::Unreachable => Some("unreachable"),
            WarningType::InfiniteLoop => Some("infinite"),
            WarningType::ZeroDivision => Some("zero"),
            WarningType::IncompleteSwitch => Some("incompleteSwitch"),
            WarningType::Todo => Some("todo"),
        }
    }

    pub const fn default_level(&self) -> WarningLevel {
        match self {
            WarningType::NoType
            | WarningType::Deprecated
            | WarningType::Unused
            | WarningType::TrivialValue
            | WarningType::Unreachable
            | WarningType::InfiniteLoop
            | WarningType::ZeroDivision => WarningLevel::Warn,
            WarningType::IncompleteSwitch => WarningLevel::Allow,
            WarningType::Todo => WarningLevel::Warn,
        }
    }
}

impl FromStr for WarningType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "deprecated" => Ok(WarningType::Deprecated),
            "unused" => Ok(WarningType::Unused),
            "trivial" => Ok(WarningType::TrivialValue),
            "unreachable" => Ok(WarningType::Unreachable),
            "infinite" => Ok(WarningType::InfiniteLoop),
            "zero" => Ok(WarningType::ZeroDivision),
            "incompleteSwitch" => Ok(WarningType::IncompleteSwitch),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{WarningLevel, WarningType};

    #[test]
    fn annotation_name() {
        assert_eq!(WarningType::NoType.annotation_name(), None);
        assert_eq!(
            WarningType::Deprecated.annotation_name(),
            Some("deprecated")
        );
        assert_eq!(WarningType::Unused.annotation_name(), Some("unused"));
        assert_eq!(WarningType::TrivialValue.annotation_name(), Some("trivial"));
        assert_eq!(
            WarningType::Unreachable.annotation_name(),
            Some("unreachable")
        );
        assert_eq!(
            WarningType::InfiniteLoop.annotation_name(),
            Some("infinite")
        );
        assert_eq!(WarningType::ZeroDivision.annotation_name(), Some("zero"));
        assert_eq!(
            WarningType::IncompleteSwitch.annotation_name(),
            Some("incompleteSwitch")
        );
        assert_eq!(WarningType::Todo.annotation_name(), Some("todo"));
    }

    #[test]
    fn default_level() {
        assert_eq!(WarningType::NoType.default_level(), WarningLevel::Warn);
        assert_eq!(WarningType::Deprecated.default_level(), WarningLevel::Warn);
        assert_eq!(WarningType::Unused.default_level(), WarningLevel::Warn);
        assert_eq!(
            WarningType::TrivialValue.default_level(),
            WarningLevel::Warn
        );
        assert_eq!(WarningType::Unreachable.default_level(), WarningLevel::Warn);
        assert_eq!(
            WarningType::InfiniteLoop.default_level(),
            WarningLevel::Warn
        );
        assert_eq!(
            WarningType::ZeroDivision.default_level(),
            WarningLevel::Warn
        );
        assert_eq!(
            WarningType::IncompleteSwitch.default_level(),
            WarningLevel::Allow
        );
        assert_eq!(WarningType::Todo.default_level(), WarningLevel::Warn);
    }
}
