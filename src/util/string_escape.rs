use std::fmt::{Display, Write};

#[derive(Debug)]
pub struct StringEscape<'a> {
    value: &'a str,
}

#[derive(Debug)]
pub struct CharEscape {
    value: char,
}

/// Adds escape codes to the given string.
///
/// # Examples
/// assert_eq!(escape("abc").to_string(), "abc");  // Simple strings return themselves
/// assert_eq!(escape("\0\x07\x12").to_string(), r"\0\a\x12");
/// assert_eq!(escape("\u{E000}\u{F0000}".to_string(), r"\uE000\U000F0000"));
///
pub fn escape(value: &str) -> StringEscape<'_> {
    StringEscape { value }
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
pub fn escaped(value: char) -> CharEscape {
    CharEscape { value }
}

impl<'a> Display for StringEscape<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for chr in self.value.chars() {
            CharEscape { value: chr }.fmt(f)?;
        }
        Ok(())
    }
}

impl Display for CharEscape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            '\\' => f.write_str(r"\\"),
            '"' => f.write_str(r#"\""#),
            '\0' => f.write_str(r"\0"),
            '\x07' => f.write_str(r"\a"),
            '\x08' => f.write_str(r"\b"),
            '\x0C' => f.write_str(r"\f"),
            '\n' => f.write_str(r"\n"),
            '\r' => f.write_str(r"\r"),
            '\t' => f.write_str(r"\t"),
            '\x0B' => f.write_str(r"\v"),
            x @ ' '..='~' => f.write_char(x),
            x if x.is_ascii() => write!(f, r"\x{:02X}", x as u32),
            x => {
                // TODO? Get rid of to_string here
                let escaped = x.escape_debug().to_string();
                if !escaped.starts_with(r"\u") {
                    f.write_str(&escaped)
                } else {
                    match x as u32 {
                        x @ 0..=0x7F => unreachable!(
                            "ASCII characters should already be filtered out (got {:#x})",
                            x
                        ),
                        x @ 0x80..=0xFF => write!(f, r"\x{:02X}", x),
                        x @ 0x100..=0xFFFF => write!(f, r"\u{:04X}", x),
                        x => write!(f, r"\U{:08X}", x),
                    }
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
        assert_eq!(escape("abcd").to_string(), "abcd");
        assert_eq!(escape("test string").to_string(), "test string");
    }

    #[test]
    fn string_ascii_escape() {
        assert_eq!(escape("\\\"\0\n\x0C").to_string(), r#"\\\"\0\n\f"#);
        assert_eq!(escape("\x01\x0F").to_string(), r"\x01\x0F");
    }

    #[test]
    fn string_unicode() {
        assert_eq!(escape("\u{E000}\u{F0000}").to_string(), r"\uE000\U000F0000")
    }

    #[test]
    fn printable_ascii() {
        for ch in ' '..='~' {
            if !['"', '\\'].contains(&ch) {
                assert_eq!(escaped(ch).to_string(), ch.to_string().as_str())
            }
        }
    }

    #[test]
    fn ascii_escape() {
        assert_eq!(escaped('\\').to_string(), r"\\");
        assert_eq!(escaped('"').to_string(), r#"\""#);
        assert_eq!(escaped('\0').to_string(), r"\0");
        assert_eq!(escaped('\x07').to_string(), r"\a");
        assert_eq!(escaped('\x08').to_string(), r"\b");
        assert_eq!(escaped('\x0C').to_string(), r"\f");
        assert_eq!(escaped('\n').to_string(), r"\n");
        assert_eq!(escaped('\r').to_string(), r"\r");
        assert_eq!(escaped('\t').to_string(), r"\t");
        assert_eq!(escaped('\x0B').to_string(), r"\v");
    }

    #[test]
    fn unicode() {
        assert_eq!(escaped('\u{80}').to_string(), r"\x80");
        assert_eq!(escaped('\u{E000}').to_string(), r"\uE000");
        assert_eq!(escaped('\u{F0000}').to_string(), r"\U000F0000");
    }
}
