use crate::parser::Parser;
use std::env::args;
use std::path::PathBuf;

mod parser;
#[macro_use]
mod macros;
mod util;

fn main() {
    let file = PathBuf::from(args().nth(1).unwrap());
    Parser::parse_file(file).unwrap().unwrap();
    println!("Hello, world!");
}
