use derive_new::new;

use crate::converter::access_handler::AccessLevel;
use crate::error::{CompilerError, CompilerException, ErrorBuilder, LineInfo};
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::levenshtein;

use super::TypeObject;

#[derive(Debug, new)]
pub struct AccessError {
    ty: AccessErrorType,
    value: AttrValue,
    parent: TypeObject,
    line_info: LineInfo,
}

#[derive(Debug)]
pub enum AccessErrorType {
    NotFound,
    WeakAccess(AccessTooStrict),
    NeedsMut,
}

#[derive(Debug)]
pub struct AccessTooStrict {
    pub level_gotten: AccessLevel,
    pub level_expected: AccessLevel,
}

#[derive(Debug)]
pub enum AttrValue {
    Operator(OpSpTypeNode),
    Attribute(String),
    StaticAttr(String),
}

impl AccessError {
    pub fn into_compiler_err(self) -> CompilerError {
        CompilerException::from(self).into()
    }

    fn name_str(&self, capitalize: bool) -> String {
        match &self.value {
            AttrValue::Operator(o) => format!("'{o}'"),
            AttrValue::Attribute(a) => {
                if capitalize {
                    format!("Attribute '{a}'")
                } else {
                    format!("attribute '{a}'")
                }
            }
            AttrValue::StaticAttr(s) => {
                if capitalize {
                    format!("Static attribute '{s}'")
                } else {
                    format!("static attribute '{s}'")
                }
            }
        }
    }
}

impl AttrValue {
    fn as_name(&self) -> Option<&str> {
        match self {
            AttrValue::Operator(_) => None,
            AttrValue::Attribute(a) => Some(a.as_str()),
            AttrValue::StaticAttr(s) => Some(s.as_str()),
        }
    }
}

impl From<AccessError> for CompilerException {
    fn from(err: AccessError) -> Self {
        match err.ty {
            AccessErrorType::WeakAccess(access) => CompilerException::with_note(
                format!(
                    "Cannot get {} from type '{}'",
                    err.name_str(false),
                    err.parent.name()
                ),
                // TODO: Make more user-friendly
                format!(
                    "Operator has too strict of an access level (expected {:?}, got {:?})",
                    access.level_expected, access.level_gotten,
                ),
                err.line_info,
            ),
            AccessErrorType::NeedsMut => CompilerException::of(
                format!(
                    "{} needs a mut variable for type '{}'",
                    err.name_str(true),
                    err.parent.name()
                ),
                err.line_info,
            ),
            AccessErrorType::NotFound => {
                let closest = err.value.as_name().and_then(|name| {
                    err.parent
                        .get_defined()
                        .and_then(|x| levenshtein::closest_name(name, x))
                });
                CompilerException::from_builder(
                    ErrorBuilder::new(&err.line_info)
                        .with_message(format!(
                            "{} does not exist in type '{}'",
                            err.name_str(true),
                            err.parent.name()
                        ))
                        .when_some(closest, |builder, closest| {
                            builder.with_help(format!("Did you mean '{closest}'?"))
                        }),
                )
            }
        }
    }
}
