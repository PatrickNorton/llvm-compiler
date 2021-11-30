use crate::parser::Parser;
use std::env::args;
use std::path::PathBuf;

mod parser;
#[macro_use]
mod macros;
mod util;

fn main() {
    let file = PathBuf::from(args().nth(1).unwrap());
    match Parser::parse_file(file).unwrap() {
        Result::Ok(_) => {}
        Result::Err(err) => println!("{}", err),
    }
}
