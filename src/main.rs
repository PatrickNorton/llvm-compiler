use std::env::args;
use std::error::Error;
use std::fs::create_dir;
use std::process::ExitCode;

use crate::arguments::CLArgs;
use crate::parser::Parser;
use crate::util::BYTECODE_EXTENSION;

mod arguments;
mod converter;
#[macro_use]
mod macros;
mod parser;
mod util;

fn main() -> ExitCode {
    match inner_main() {
        Result::Ok(()) => ExitCode::SUCCESS,
        Result::Err(e) => {
            println!("{}", e);
            ExitCode::FAILURE
        }
    }
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
