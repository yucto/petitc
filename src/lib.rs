use std::path::Path;

use error::{Error, Result};
use parsing::parse_to_ast;
use typechecker::TypedFile;

pub mod ast;
mod color;
mod compile;
pub mod error;
mod parsing;
mod tree;
mod typechecker;
mod typing;

pub fn parse(
    p: impl AsRef<Path>,
) -> Result<(ast::File<parsing::SpanAnnotation>, Vec<String>)> {
    match parse_to_ast(p.as_ref()) {
        Ok((file, string_store)) => Ok((file, string_store)),
        Err(error) => {
            eprintln!("{}", error);
            std::process::exit(1);
        }
    }
}

pub fn typecheck(
    _p: impl AsRef<Path>,
    file: ast::File<parsing::SpanAnnotation>,
    string_store: &mut Vec<String>,
) -> std::result::Result<typechecker::TypedFile, Vec<Error>> {
    typechecker::typecheck(file, string_store)
}

pub fn compile(
    path: impl AsRef<Path>,
    file: TypedFile,
    string_store: &[String],
) {
    compile::compile(path, file, string_store).unwrap()
}
