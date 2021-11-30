pub mod decimal;

pub fn first<T>(args: Vec<T>) -> T {
    debug_assert!(
        !args.is_empty(),
        "Value passed to first must have at least 1 element"
    );
    args.into_iter().next().unwrap()
}

pub fn first_n<T, const N: usize>(args: Vec<T>) -> [T; N] {
    args.try_into()
        .unwrap_or_else(|x: Vec<T>| panic!("Value had length {}, expected length {}", x.len(), N))
}
