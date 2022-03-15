use std::env::args;
use std::error::Error;
use std::fs::create_dir;
use std::process::exit;

use crate::arguments::CLArgs;
use crate::parser::Parser;
use crate::util::BYTECODE_EXTENSION;

mod arguments;
mod converter;
#[macro_use]
mod macros;
mod parser;
mod util;

fn main() {
    exit(match inner_main() {
        Result::Ok(_) => 0,
        Result::Err(e) => {
            println!("{}", e);
            1
        }
    })
}

fn inner_main() -> Result<(), Box<dyn Error>> {
    let arguments = CLArgs::parse(args())?;
    let file = arguments.target().to_owned();
    let node = Parser::parse_file(file.clone())??;
    let dest_folder = file.parent().unwrap().join("__ncache__");
    if !dest_folder.exists() {
        create_dir(&dest_folder)?;
    }
    let mut dest_file = dest_folder.join(file.file_name().unwrap());
    dest_file.set_extension(BYTECODE_EXTENSION);
    converter::convert_to_file(dest_file, node, arguments)?;
    Ok(())
}
