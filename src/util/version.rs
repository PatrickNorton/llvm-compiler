use std::fmt::Display;
use std::num::ParseIntError;
use std::str::FromStr;

/// A compiler version.
///
/// At the moment, this can only represent versions in `x.y.z` semantic form.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version {
    major: usize,
    minor: usize,
    bugfix: usize,
}

/// The version of the currently running compiler.
pub const CURRENT_VERSION: Version = Version::new(0, 0, 1);

impl Version {
    /// Creates a new version.
    ///
    /// # Examples
    /// ```
    /// println!(Version::new(0, 0, 1)); // 0.0.1
    /// println!(Version::new(1, 2, 3)); // 1.2.3
    /// ```
    pub const fn new(major: usize, minor: usize, bugfix: usize) -> Self {
        Self {
            major,
            minor,
            bugfix,
        }
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.bugfix)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VersionParseError {
    NumParse(ParseIntError),
    IncorrectDots(),
}

impl FromStr for Version {
    type Err = VersionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let text = s
            .split('.')
            .map(|x| x.parse().map_err(VersionParseError::NumParse))
            .collect::<Result<Vec<_>, _>>()?;
        if let [major, minor, bugfix] = *text {
            Ok(Version::new(major, minor, bugfix))
        } else {
            Err(VersionParseError::IncorrectDots())
        }
    }
}

impl std::error::Error for VersionParseError {}

impl Display for VersionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionParseError::NumParse(e) => e.fmt(f),
            VersionParseError::IncorrectDots() => f.write_str("Incorrect number of dots given"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::util::version::Version;

    #[test]
    fn display() {
        assert_eq!(format!("{}", Version::new(0, 0, 0)), "0.0.0");
        assert_eq!(format!("{}", Version::new(1, 2, 3)), "1.2.3");
    }

    #[test]
    fn ordering() {
        assert_eq!(Version::new(0, 1, 2), Version::new(0, 1, 2));
        assert!(Version::new(0, 1, 2) < Version::new(0, 1, 3));
        assert!(Version::new(0, 1, 2) < Version::new(0, 2, 0));
        assert!(Version::new(0, 1, 2) < Version::new(1, 0, 0));
    }

    #[test]
    fn parse() {
        assert_eq!(Version::from_str("0.1.2"), Ok(Version::new(0, 1, 2)));
        assert_eq!(
            Version::from_str("100.100.100"),
            Ok(Version::new(100, 100, 100))
        );
    }

    #[test]
    fn parse_err() {
        assert!(Version::from_str("hi").is_err());
        assert!(Version::from_str("0.1.2.3").is_err());
        assert!(Version::from_str("0.1").is_err());
        assert!(Version::from_str("-1.2.3").is_err());
        assert!(Version::from_str("1..3").is_err());
    }
}
