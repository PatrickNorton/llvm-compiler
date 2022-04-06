use std::fmt::Display;

/// Permission levels for the compiled code.
///
/// Levels:
/// * [`Normal`](PermissionLevel::Normal): All user-written code
/// * [`Stdlib`](PermissionLevel::Stdlib): Code in the stdlib; has access to
/// `$native(sys)`
/// * [`Builtin`](PermissionLevel::Builtin): Only for `__builtins__.newlang`;
/// can use `$builtin`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PermissionLevel {
    Normal,
    Stdlib,
    Builtin,
}

impl PermissionLevel {
    /// If the permission level is part of the standard library.
    ///
    /// Note that this is `true` for both [`PermissionLevel::Stdlib`] and
    /// [`PermissionLevel::Builtin`], as the builtin file is also part of the
    /// standard library.
    ///
    /// # Examples
    /// assert!(!PermissionLevel::Normal.is_stdlib());
    /// assert!(PermissionLevel::Stdlib.is_stdlib());
    /// assert!(PermissionLevel::Builtin.is_stdlib());
    pub fn is_stdlib(self) -> bool {
        matches!(self, PermissionLevel::Stdlib | PermissionLevel::Builtin)
    }

    /// If the permission level is the level used for builtin files.
    ///
    /// # Examples
    /// assert!(!PermissionLevel::Normal.is_builtin());
    /// assert!(!PermissionLevel::Stdlib.is_builtin());
    /// assert!(PermissionLevel::Builtin.is_builtin());
    pub fn is_builtin(self) -> bool {
        matches!(self, PermissionLevel::Builtin)
    }
}

impl Display for PermissionLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            PermissionLevel::Normal => "normal",
            PermissionLevel::Stdlib => "stdlib",
            PermissionLevel::Builtin => "builtin",
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::converter::permission::PermissionLevel;

    #[test]
    fn stdlib_perm() {
        assert!(!PermissionLevel::Normal.is_stdlib());
        assert!(PermissionLevel::Stdlib.is_stdlib());
        assert!(PermissionLevel::Builtin.is_stdlib());
    }

    #[test]
    fn builtin_perm() {
        assert!(!PermissionLevel::Normal.is_builtin());
        assert!(!PermissionLevel::Stdlib.is_builtin());
        assert!(PermissionLevel::Builtin.is_builtin());
    }

    #[test]
    fn perm_display() {
        assert_eq!(format!("{}", PermissionLevel::Normal), "normal");
        assert_eq!(format!("{}", PermissionLevel::Stdlib), "stdlib");
        assert_eq!(format!("{}", PermissionLevel::Builtin), "builtin");
    }
}
