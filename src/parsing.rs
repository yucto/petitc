use std::path::Path;

use crate::ast::File;

use beans::error::WarningSet;
use beans::include_parser;
use beans::parser::Parser;
use beans::stream::StringStream;
use crate::error::Result;

pub(crate) fn parse_to_ast(source: &Path) -> Result<File> {
    let mut warnings = WarningSet::empty();
    let (lexer, parser) = include_parser!(
    lexer => compiled "../gmrs/petitc.clx",
    parser => compiled "../gmrs/petitc.cgr",
    )
    .unwrap()
    .unpack_into(&mut warnings);
    let mut input = StringStream::from_file(source)
        .unwrap()
        .unpack_into(&mut warnings);
    let _ast = parser
        .parse(&mut lexer.lex(&mut input))
        .unwrap_or_else(|_| std::process::exit(1))
        .unpack_into(&mut warnings);
    std::process::exit(0);
}
