use std::fmt::{self, Display, Formatter};

/// A struct that implements `Display` with the custom formatter given.
///
/// This struct is created by the [`format_with`] function, see its
/// documentation for more.
#[derive(Debug)]
pub struct FormatWith<'a, T, F> {
    value: &'a T,
    fmt: F,
}

/// Formats the given value with the given function.
///
/// This is a generalization of `format` to use a custom formatting function,
/// instead of the one given by the `Display` trait. This allows printing of
/// values that do not implement `Display`, or printing a value with a different
/// representation than would be typical for that type.
///
/// # Examples
/// ```
/// assert_eq!(format_with(&10, |x, f| write!(f, "{:#x}")).to_string(), "0xa");
/// ```
pub fn format_with<T, F>(value: &T, fmt: F) -> FormatWith<'_, T, F>
where
    F: Fn(&T, &mut Formatter<'_>) -> fmt::Result,
{
    FormatWith { value, fmt }
}

impl<'a, T, F> Display for FormatWith<'a, T, F>
where
    F: Fn(&T, &mut Formatter<'_>) -> fmt::Result,
{
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        (self.fmt)(self.value, f)
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use super::format_with;

    #[test]
    fn default_format() {
        let values: &[Box<dyn Display>] = &[
            Box::new(10i32),
            Box::new("foo"),
            Box::new('x'),
            Box::new(true),
            Box::new(7.5f64),
        ];
        for value in values {
            assert_eq!(
                format_with(value, |x, f| x.fmt(f)).to_string(),
                value.to_string()
            )
        }
    }

    #[test]
    fn constant_format() {
        let values: &[Box<dyn Display>] = &[
            Box::new(10i32),
            Box::new("foo"),
            Box::new('x'),
            Box::new(true),
            Box::new(7.5f64),
        ];
        for value in values {
            assert_eq!(
                format_with(value, |_, f| f.write_str("test")).to_string(),
                "test".to_string()
            )
        }
    }
}
