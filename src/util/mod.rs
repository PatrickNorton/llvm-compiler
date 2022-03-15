pub mod decimal;
pub mod error_counter;
pub mod int_allocator;
pub mod levenshtein;
pub mod maybe_ref;
pub mod string_escape;
pub mod tee_map;
pub mod version;

pub const MAGIC_NO: u32 = 0x0ABADE66;
pub const MAGIC_NUMBER: &[u8] = &MAGIC_NO.to_be_bytes();
pub const FILE_EXTENSION: &str = ".newlang";
pub const BYTECODE_EXTENSION: &str = ".nbyte";
pub const EXPORTS_FILENAME: &str = "__exports__.newlang";

pub const U32_BYTES: usize = u32::BITS as usize / u8::BITS as usize;
pub const U16_BYTES: usize = u16::BITS as usize / u8::BITS as usize;

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

pub fn usize_to_bytes(val: usize) -> [u8; U32_BYTES] {
    u32::try_from(val).unwrap().to_be_bytes()
}

pub fn usize_to_short_bytes(val: usize) -> [u8; U16_BYTES] {
    u16::try_from(val).unwrap().to_be_bytes()
}

pub fn reborrow_option<'a, 'b, T>(opt: &'a mut Option<&'b mut T>) -> Option<&'a mut T> {
    match opt {
        Some(a) => Some(*a),
        None => None,
    }
}
