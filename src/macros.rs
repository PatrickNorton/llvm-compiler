#[macro_export]
macro_rules! hash_set {
    () => {::std::collections::HashSet::new()};
    ($($x:expr),+ $(,)?) => (
        {
            let mut temp = ::std::collections::HashSet::new();
            $(
                temp.insert($x);
            )+
            temp
        }
    );
}

#[macro_export]
macro_rules! hash_map {
    () => {::std::collections::HashMap::new()};
    ($($x:expr => $y:expr),+ $(,)?) => (
        {
            let mut temp = ::std::collections::HashMap::new();
            $(
                temp.insert($x, $y);
            )+
            temp
        }
    );
}
