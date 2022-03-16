use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PermissionLevel {
    Normal,
    Stdlib,
    Builtin,
}

impl PermissionLevel {
    pub fn is_stdlib(self) -> bool {
        matches!(self, PermissionLevel::Stdlib | PermissionLevel::Builtin)
    }

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
