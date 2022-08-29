const SYSCALLS: &[(&str, u16)] = &[
    ("read", 0),
    ("write", 1),
    ("open", 2),
    ("close", 3),
    ("getcwd", 79),
    ("chdir", 80),
    ("mkdir", 83),
];

/// Get the syscall index associated with the given name.
///
/// This trims trailing underscores from the name, so `"read_"` would match the
/// syscall `"read"`.
///
/// # Examples
/// ```
/// assert_eq!(get_syscall("read"), 0);
/// assert_eq!(get_syscall("getcwd_"), 79);
/// ```
pub fn get_syscall(name: &str) -> u16 {
    let name = name.trim_end_matches('_');
    SYSCALLS.iter().find(|x| x.0 == name).unwrap().1
}

/// Get the syscall name associated with the given index.
///
/// # Examples
/// ```
/// assert_eq!(syscall_name(1), "write");
/// assert_eq!(syscall_name(3), "close");
/// ```
pub fn syscall_name(index: u16) -> &'static str {
    SYSCALLS.iter().find(|x| x.1 == index).unwrap().0
}

#[cfg(test)]
mod tests {
    use super::{get_syscall, syscall_name, SYSCALLS};

    #[test]
    fn get_all_syscalls() {
        for &(syscall, index) in SYSCALLS {
            assert_eq!(get_syscall(syscall), index);
        }
    }

    #[test]
    fn syscall_underscore() {
        for &(syscall, index) in SYSCALLS {
            let underscore = format!("{}_", syscall);
            assert_eq!(get_syscall(&underscore), index);
        }
    }

    #[test]
    fn name_all_syscalls() {
        for &(syscall, index) in SYSCALLS {
            assert_eq!(syscall_name(index), syscall);
        }
    }
}
