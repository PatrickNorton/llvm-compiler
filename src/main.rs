use std::env::{self, args};
use std::error::Error;
use std::fs::create_dir;
use std::process::ExitCode;

use crate::arguments::CLArgs;
use crate::util::BYTECODE_EXTENSION;

mod arguments;
mod converter;
mod error;
#[macro_use]
mod macros;
mod parser;
mod util;

fn main() -> ExitCode {
    // This is basically just a wrapper for inner_main: print an error if
    // returned, and then exit with the correct `ExitCode`.
    match inner_main() {
        Result::Ok(()) => ExitCode::SUCCESS,
        Result::Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}

fn inner_main() -> Result<(), Box<dyn Error>> {
    // First, we set RUST_BACKTRACE to be true. This is because we want our
    // errors to have an associated backtrace, at least for now. This may change
    // in the future.
    env::set_var("RUST_BACKTRACE", "1");
    // After having done our debuggability step, we want to parse our command
    // line arguments.
    // TODO: Short-circuiting
    let arguments = CLArgs::parse(args())?;
    // Next, we determine which is the main file to compile. At the moment,
    // compilation is based on a single file (which must either be a library and
    // contain a `main()` function), and the compiler programmatically pulls in
    // the other relevant files. This is unusual for compilers (and is a
    // consequence of this language's origins as a scripting-like language), so
    // it may be changed in the future.
    let file = arguments.target().to_owned();
    // We put all our compiled files in the `__ncache__` directory (another
    // consequence of the Python influence in t this language's early days).
    let dest_folder = file.parent().unwrap().join("__ncache__");
    // If the folder isn't there, we make it be there.
    if !dest_folder.exists() {
        create_dir(&dest_folder)?;
    }
    // Within `__ncache__`, we mirror the source filename to the destination,
    // just with the `.nbyte` extension.
    let mut dest_file = dest_folder.join(file.file_name().unwrap());
    dest_file.set_extension(BYTECODE_EXTENSION);
    // Now we've done all the setup, we actually do the compilation.
    converter::convert_to_file(dest_file, file, arguments)?;
    Ok(())
}
