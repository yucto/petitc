use std::path::Path;

use error::Result;

pub mod ast;
pub mod error;

pub fn parse<'a>(_p: impl AsRef<Path> + 'a) -> Result<'a, ()> {
    unimplemented!()
}

pub fn typecheck<'a>(_p: impl AsRef<Path> + 'a, _: ()) -> Result<'a, ()> {
    unimplemented!()
}

pub fn compile<'a>(_p: impl AsRef<Path> + 'a, _: ()) -> Result<'a, ()> {
    unimplemented!()
}
