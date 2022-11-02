use std::path::Path;

use parsing::parse_to_ast;
use error::Result;

pub mod ast;
pub mod error;
mod parsing;

pub fn parse(p: impl AsRef<Path>) -> Result<()> {
    parse_to_ast(p.as_ref()).unwrap();
    Ok(())
}

pub fn typecheck(_p: impl AsRef<Path>, _: ()) -> Result<()> {
    unimplemented!()
}

pub fn compile(_p: impl AsRef<Path>, _: ()) -> Result<()> {
    unimplemented!()
}
