const SYSCALLS: &[(&str, u16)] = &[
    ("read", 0),
    ("write", 1),
    ("open", 2),
    ("close", 3),
    ("getcwd", 79),
    ("chdir", 80),
    ("mkdir", 83),
];

pub fn get_syscall(name: &str) -> u16 {
    let name = name.trim_end_matches('_');
    SYSCALLS.iter().find(|x| x.0 == name).unwrap().1
}

pub fn syscall_name(index: u16) -> &'static str {
    SYSCALLS.iter().find(|x| x.1 == index).unwrap().0
}
