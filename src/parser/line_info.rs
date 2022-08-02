use std::path::Path;
use std::sync::Arc;

/// A location within a source file and the information required to display that
/// to the user.
///
/// Most `LineInfo`s are created by [`TokenList`] as part of the tokenization
/// process. These are therefore associated with a [`Token`], and from there
/// enter the token tree.
///
/// There are two methods that create a `LineInfo` that don't correspond to any
/// position in a source file: [`LineInfo::empty`] and [`LineInfo::empty_ref`].
/// [`LineInfo::empty`] is a `const fn` that returns a `LineInfo` by value,
/// whereas [`LineInfo::empty_ref`] returns a reference to a static, identical
/// `LineInfo`.
///
/// The user-facing representation of a `LineInfo` can be obtained through the
/// [`info_string`](Self::info_string) method. Additionally, both the file and
/// line number can be obtained through getter methods.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct LineInfo {
    inner: Option<Arc<InfoInner>>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct InfoInner {
    path: Arc<Path>,
    line_no: usize,
    line: Arc<str>,
    start: usize,
}

/// A trait for an object that has an associated [`LineInfo`].
pub trait Lined {
    /// Returns a reference to the [`LineInfo`] associated with `self`.
    fn line_info(&self) -> &LineInfo;
}

impl LineInfo {
    /// Creates a new [`LineInfo`].
    ///
    /// # Examples
    /// ```
    /// let line_info = LineInfo::new(
    ///     Arc::from(Path::new("/dev/null")), 0, "test_line", 0
    /// );
    /// ```
    pub fn new(path: Arc<Path>, line_no: usize, line: Arc<str>, start: usize) -> LineInfo {
        LineInfo {
            inner: Some(Arc::new(InfoInner {
                path,
                line_no,
                line,
                start,
            })),
        }
    }

    /// Creates an empty [`LineInfo`].
    ///
    /// This does not correspond to any particular position in a source file.
    /// Instead, it is mostly used as a placeholder in methods that usually
    /// expect a [`LineInfo`] (usually for debugging purposes), but cannot
    /// provide an actual location for whatever reason. In particular, this
    /// applies to methods generated through `$derive` annotations or the
    /// "magic" builtins such as `object`.
    ///
    /// If a reference is wanted, use [`LineInfo::empty_ref`] instead.
    ///
    /// # Examples
    /// ```
    /// let empty = LineInfo::empty();
    /// ```
    pub const fn empty() -> LineInfo {
        Self { inner: None }
    }

    /// Returns a reference to an empty [`LineInfo`].
    ///
    /// The value referenced by this function is equal to
    /// [`LineInfo::empty()`](LineInfo::empty). If you want to get this by
    /// value, use that function instead.
    ///
    /// # Examples
    /// ```
    /// let empty = LineInfo::empty();
    /// let empty_ref = LineInfo::empty_ref();
    /// assert_eq!(&empty, empty_ref);
    /// ```
    pub fn empty_ref<'a>() -> &'a LineInfo {
        static EMPTY: LineInfo = LineInfo::empty();
        &EMPTY
    }

    /// The path to the file that this [`LineInfo`] references.
    ///
    /// # Examples
    /// ```
    /// let line_info = LineInfo::new(
    ///     Arc::from(Path::new("/dev/null")), 0, "test_line", 0
    /// );
    /// assert_eq!(line_info.get_path(), "/dev/null".as_ref());
    ///
    /// // An empty LineInfo returns an empty path.
    /// let empty = LineInfo::empty();
    /// assert_eq!(empty.get_path(), Path::new(""));
    /// ```
    pub fn get_path(&self) -> &Path {
        self.inner
            .as_ref()
            .map_or_else(|| Path::new(""), |x| &x.path)
    }

    /// The line number within the file that this [`LineInfo`] references.
    ///
    /// # Examples
    /// ```
    /// let line_info = LineInfo::new(
    ///     Arc::from(Path::new("/dev/null")), 5, "test_line", 0
    /// );
    /// assert_eq!(line_info.get_line_number(), 5);
    ///
    /// // An empty LineInfo has a maximum line number.
    /// let empty = LineInfo::empty();
    /// assert_eq!(empty.get_path(), usize::MAX);
    /// ```
    pub fn get_line_number(&self) -> usize {
        self.inner.as_ref().map_or(usize::MAX, |x| x.line_no)
    }

    /// The user-facing display of the region associated with the [`LineInfo`].
    ///
    /// This does not include the current file; that is usually printed
    /// elsewhere. It does include the line number, text of the line and a
    /// carat pointing to the location within the line.
    ///
    /// # Examples
    /// ```
    /// let info = let line_info = LineInfo::new(
    ///     Arc::from(Path::new("/dev/null")), 0, "test_line", 5
    /// );
    /// let text = "\
    /// 0: test_line
    ///         ^";
    /// assert_eq!(info.info_string(), text);
    /// ```
    pub fn info_string(&self) -> String {
        match &self.inner {
            Option::Some(inner) => {
                let num_spaces = inner.start + inner.line_no.to_string().len() + 2;
                format!(
                    "{}: {}\n{:spaces$}^",
                    inner.line_no,
                    inner.line,
                    "",
                    spaces = num_spaces
                )
            }
            Option::None => "Line info not found".to_string(),
        }
    }

    /// Creates a [`LineInfo`] identical to `self`, but pointing to a location
    /// `start` additional characters down the line.
    ///
    /// # Examples
    /// ```
    /// let line_info = LineInfo::new(
    ///     Arc::from(Path::new("/dev/null")), 17, "test_line", 0
    /// );
    /// let substring = line_info.substring(5);
    /// let identical = LineInfo::new(
    ///     Arc::from(Path::new("/dev/null")), 17, "test_line", 5
    /// );
    /// assert_eq!(substring, identical);
    ///
    /// // Calling `substring` on an empty LineInfo returns an empty LineInfo
    /// let empty = LineInfo::empty();
    /// assert_eq!(empty.substring(100), LineInfo::empty());
    /// ```
    pub fn substring(&self, start: usize) -> LineInfo {
        match &self.inner {
            Option::Some(inner) => LineInfo {
                inner: Some(Arc::new(InfoInner {
                    path: inner.path.clone(),
                    line_no: inner.line_no,
                    line: inner.line.clone(),
                    start: inner.start + start,
                })),
            },
            Option::None => LineInfo::empty(),
        }
    }
}

impl Default for LineInfo {
    fn default() -> Self {
        Self::empty()
    }
}

impl Lined for LineInfo {
    fn line_info(&self) -> &LineInfo {
        self
    }
}

impl<T> Lined for &T
where
    T: Lined + ?Sized,
{
    fn line_info(&self) -> &LineInfo {
        (*self).line_info()
    }
}

impl<T> Lined for &mut T
where
    T: Lined + ?Sized,
{
    fn line_info(&self) -> &LineInfo {
        (**self).line_info()
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::sync::Arc;

    use super::LineInfo;

    #[test]
    fn empty_eq() {
        assert_eq!(LineInfo::empty(), LineInfo::empty());
    }

    #[test]
    fn empty_is_empty_ref() {
        assert_eq!(&LineInfo::empty(), LineInfo::empty_ref());
    }

    #[test]
    #[allow(clippy::redundant_clone)]
    fn clone_equal() {
        assert_eq!(LineInfo::empty().clone(), LineInfo::empty());
        let line_info = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            0,
        );
        assert_eq!(line_info.clone(), line_info);
    }

    #[test]
    fn identical_equal() {
        let first = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            0,
        );
        let second = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            0,
        );
        assert_eq!(first, second);
    }

    #[test]
    fn path() {
        let line_info = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            0,
        );
        assert_eq!(line_info.get_path(), Path::new("/dev/null"));
        let empty = LineInfo::empty();
        assert_eq!(empty.get_path(), Path::new(""));
    }

    #[test]
    fn line_number() {
        let line_info = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            0,
        );
        assert_eq!(line_info.get_line_number(), 0);
        let empty = LineInfo::empty();
        assert_eq!(empty.get_line_number(), usize::MAX);
    }

    #[test]
    fn info_string() {
        assert_eq!(LineInfo::empty().info_string(), "Line info not found");
        let line_info = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            5,
        );
        assert_eq!(line_info.info_string(), "0: test_string\n        ^");
    }

    #[test]
    fn empty_substring() {
        assert_eq!(LineInfo::empty().substring(10), LineInfo::empty());
    }

    #[test]
    fn substring() {
        let line_info = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            0,
        );
        let substring = LineInfo::new(
            Arc::from(Path::new("/dev/null")),
            0,
            Arc::from("test_string"),
            5,
        );
        assert_eq!(line_info.substring(5), substring);
    }
}
