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
