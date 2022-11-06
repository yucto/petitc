use std::path::Path;

use error::Result;
use parsing::parse_to_ast;

pub mod ast;
pub mod error;
mod parsing;
mod typechecker;

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
    string_store: &[String],
) -> Result<ast::File<typechecker::TypeAnnotation>> {
    typechecker::typecheck(file, string_store)
}

pub fn compile(
    _p: impl AsRef<Path>,
    _: ast::File<typechecker::TypeAnnotation>,
) -> Result<()> {
    unimplemented!()
}
