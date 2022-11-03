use std::path::Path;

use error::Result;
use parsing::parse_to_ast;

pub mod ast;
pub mod error;
mod parsing;

pub fn parse(p: impl AsRef<Path>) -> Result<()> {
    match parse_to_ast(p.as_ref()) {
        Ok(_) => Ok(()),
        Err(error) => {
            eprintln!("{}", error);
            std::process::exit(1);
        }
    }
}

pub fn typecheck(_p: impl AsRef<Path>, _: ()) -> Result<()> {
    unimplemented!()
}

pub fn compile(_p: impl AsRef<Path>, _: ()) -> Result<()> {
    unimplemented!()
}
