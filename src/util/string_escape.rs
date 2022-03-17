use std::borrow::Cow;

/// Adds escape codes to the given string.
///
/// # Examples
/// assert_eq!(escape("abc"), "abc");  // Simple strings return themselves
/// assert_eq!(escape("\0\x07\x12"), r"\0\a\x12");
/// assert_eq!(escape("\u{E000}\u{F0000}", r"\uE000\U000F0000"));
///
pub fn escape(value: &str) -> String {
    value.chars().map(escaped).collect()
}

/// Returns the escaped version of the given character.
///
/// # Examples
/// ```
/// assert_eq!(escaped('a'), "a");
/// assert_eq!(escaped('\\'), r"\\");
/// assert_eq!(escaped('\x0F'), r"\x0F");
/// assert_eq!(escaped('\u{E000}'), r"\uE000");
/// assert_eq!(escaped('\u{F0000}'), r"\U000F0000");
/// ```
pub fn escaped(value: char) -> Cow<'static, str> {
    match value {
        '\\' => r"\\".into(),
        '"' => r#"\""#.into(),
        '\0' => r"\0".into(),
        '\x07' => r"\a".into(),
        '\x08' => r"\b".into(),
        '\x0C' => r"\f".into(),
        '\n' => r"\n".into(),
        '\r' => r"\r".into(),
        '\t' => r"\t".into(),
        '\x0B' => r"\v".into(),
        x @ ' '..='~' => x.to_string().into(), // graphic ASCII characters
        x if x.is_ascii() => format!(r"\x{:02X}", x as u32).into(),
        x => {
            let escaped = value.escape_debug().to_string();
            if !escaped.starts_with(r"\u") {
                escaped.into()
            } else {
                match x as u32 {
                    x @ 0..=0x7F => unreachable!(
                        "ASCII characters should already be filtered out (got {:#x})",
                        x
                    ),
                    x @ 0x80..=0xFF => format!(r"\x{:02X}", x).into(),
                    x @ 0x100..=0xFFFF => format!(r"\u{:04X}", x).into(),
                    x => format!(r"\U{:08X}", x).into(),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::util::string_escape::{escape, escaped};

    #[test]
    fn simple_string() {
        assert_eq!(escape("abcd"), "abcd");
        assert_eq!(escape("test string"), "test string");
    }

    #[test]
    fn string_ascii_escape() {
        assert_eq!(escape("\\\"\0\n\x0C"), r#"\\\"\0\n\f"#);
        assert_eq!(escape("\x01\x0F"), r"\x01\x0F");
    }

    #[test]
    fn string_unicode() {
        assert_eq!(escape("\u{E000}\u{F0000}"), r"\uE000\U000F0000")
    }

    #[test]
    fn printable_ascii() {
        for ch in ' '..='~' {
            if !['"', '\\'].contains(&ch) {
                assert_eq!(escaped(ch), ch.to_string().as_str())
            }
        }
    }

    #[test]
    fn ascii_escape() {
        assert_eq!(escaped('\\'), r"\\");
        assert_eq!(escaped('"'), r#"\""#);
        assert_eq!(escaped('\0'), r"\0");
        assert_eq!(escaped('\x07'), r"\a");
        assert_eq!(escaped('\x08'), r"\b");
        assert_eq!(escaped('\x0C'), r"\f");
        assert_eq!(escaped('\n'), r"\n");
        assert_eq!(escaped('\r'), r"\r");
        assert_eq!(escaped('\t'), r"\t");
        assert_eq!(escaped('\x0B'), r"\v");
    }

    #[test]
    fn unicode() {
        assert_eq!(escaped('\u{80}'), r"\x80");
        assert_eq!(escaped('\u{E000}'), r"\uE000");
        assert_eq!(escaped('\u{F0000}'), r"\U000F0000");
    }
}
