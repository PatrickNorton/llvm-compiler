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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WarningLevel {
    Allow,
    Warn,
    Deny,
}

pub fn warn(
    message: impl Display,
    warn: WarningType,
    info: &CompilerInfo,
    line_info: impl Lined,
) -> CompileResult<()> {
    warn_if(message, warn, info.warning_holder(), line_info)
}

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

pub fn warn_builder(
    builder: ErrorBuilder<'_>,
    warn: WarningType,
    holder: &WarningHolder,
) -> CompileResult<()> {
    warn_level(builder, warn, holder.warning_level(warn), &holder.counter)
}

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
    pub fn new(counter: Arc<ErrorCounter>) -> Self {
        Self {
            levels: Vec::new(),
            counter,
        }
    }

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

    pub fn pop_warnings(&mut self) {
        self.levels.pop();
    }

    pub fn allow(&mut self, allowed: HashSet<WarningType>) {
        self.levels.push(WarningFrame {
            level: FrameType::Allow,
            values: allowed,
        })
    }

    pub fn warn(&mut self, warned: HashSet<WarningType>) {
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
