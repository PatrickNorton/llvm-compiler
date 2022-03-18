use std::borrow::Cow;
use std::fmt::Display;

use crate::converter::builtins::Builtins;
use crate::converter::type_obj::TypeObject;
use crate::util::{string_escape, U32_BYTES};

use super::{ConstantBytes, LangConstant};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharConstant {
    pub(super) value: char,
}

impl CharConstant {
    pub const fn new(value: char) -> Self {
        Self { value }
    }

    pub fn get_value(&self) -> char {
        self.value
    }

    pub fn str_value(&self) -> String {
        self.value.into()
    }

    pub fn repr_value(&self) -> String {
        Self::name(self.value).into()
    }

    pub fn get_type<'a>(&self, builtins: &'a Builtins) -> &'a TypeObject {
        builtins.char_type()
    }

    pub fn name(c: char) -> Cow<'static, str> {
        match c {
            '\'' => r#"c"'""#.into(),
            '"' => r#"c'"'"#.into(),
            _ => format!("c'{}'", string_escape::escaped(c)).into(),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(1 + U32_BYTES);
        bytes.push(ConstantBytes::Char as u8);
        bytes.extend((self.value as u32).to_be_bytes());
        bytes
    }
}

impl From<char> for CharConstant {
    fn from(x: char) -> Self {
        Self::new(x)
    }
}

impl From<char> for LangConstant {
    fn from(x: char) -> Self {
        LangConstant::Char(x.into())
    }
}

impl From<CharConstant> for LangConstant {
    fn from(x: CharConstant) -> Self {
        LangConstant::Char(x)
    }
}

impl Display for CharConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            '\'' => r#"c"'""#.fmt(f),
            '"' => r#"c'"'"#.fmt(f),
            c => write!(f, "c'{}'", string_escape::escaped(c)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::constant::{CharConstant, ConstantBytes};

    #[test]
    fn char_bytes() {
        const CHAR_BYTE: u8 = ConstantBytes::Char as u8;
        assert_eq!(
            CharConstant::new('\0').to_bytes(),
            vec![CHAR_BYTE, 0, 0, 0, 0]
        );
        assert_eq!(
            CharConstant::new('a').to_bytes(),
            vec![CHAR_BYTE, 0, 0, 0, 0x61]
        );
        assert_eq!(
            CharConstant::new('\u{1000}').to_bytes(),
            vec![CHAR_BYTE, 0, 0, 0x10, 0]
        );
        assert_eq!(
            CharConstant::new('\u{10000}').to_bytes(),
            vec![CHAR_BYTE, 0, 0x1, 0, 0]
        );
        assert_eq!(
            CharConstant::new('\u{12345}').to_bytes(),
            vec![CHAR_BYTE, 0, 0x1, 0x23, 0x45]
        );
        assert_eq!(
            CharConstant::new('\u{102345}').to_bytes(),
            vec![CHAR_BYTE, 0, 0x10, 0x23, 0x45]
        );
    }

    #[test]
    fn char_str() {
        for x in '\0'..char::MAX {
            assert_eq!(CharConstant::new(x).str_value(), x.to_string());
        }
    }

    #[test]
    fn char_repr() {
        for x in '\0'..char::MAX {
            assert_eq!(
                CharConstant::new(x).repr_value(),
                format!("{}", CharConstant::new(x))
            );
        }
    }

    #[test]
    fn char_display() {
        assert_eq!(format!("{}", CharConstant::new('\'')), r#"c"'""#);
        assert_eq!(format!("{}", CharConstant::new('"')), r#"c'"'"#);
        assert_eq!(format!("{}", CharConstant::new('\0')), r"c'\0'");
        assert_eq!(format!("{}", CharConstant::new('a')), r"c'a'");
        assert_eq!(format!("{}", CharConstant::new('\u{80}')), r"c'\x80'");
        assert_eq!(format!("{}", CharConstant::new('\u{e000}')), r"c'\uE000'");
        assert_eq!(
            format!("{}", CharConstant::new('\u{f0000}')),
            r"c'\U000F0000'"
        );
    }

    #[test]
    fn char_name_equal() {
        for x in '\0'..char::MAX {
            assert_eq!(CharConstant::name(x), format!("{}", CharConstant::new(x)));
        }
    }
}
