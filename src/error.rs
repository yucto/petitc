use beans::{error::Error as BeansError, location::Span};
use std::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    Beans(#[from] BeansError),
    TypeError(String),
    NoMainFunction,
    IncorrectMainFunctionType {
        ty: crate::ast::Type,
        params: Vec<crate::ast::Type>,
    },
    BreakContinueOutsideLoop,
    // TODO : Should hold the variable name
    NameError,
}

#[derive(Debug, Error)]
pub enum TypeError {}

fn display_span(span: &Span, f: &mut fmt::Formatter) -> fmt::Result {
    let (start_line, start_column) = span.start();
    let (end_line, end_column) = span.end();
    write!(f, "File \"{}\", ", span.file().display())?;
    if start_line == end_line {
        write!(f, "line {}, ", start_line + 1)?;
        if end_column - start_column <= 1 {
            write!(f, "character {}", start_column + 1)?;
        } else {
            write!(f, "characters {}-{},", start_column + 1, end_column)?;
        }
    } else {
        write!(
            f,
            "lines {}-{}, characters {}-{}",
            start_line + 1,
            end_line + 1,
            start_column + 1,
            end_column
        )?;
    }
    write!(f, ":\n")
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Beans(BeansError::SyntaxError { message, location }) => {
                let span = location.get();
                display_span(span, f)?;
                writeln!(f, "Syntax error: {}", message)
            }
            Self::Beans(BeansError::LexingError { message, location }) => {
                let span = location.get();
                display_span(&span, f)?;
                writeln!(f, "Lexing error: {}", message)
            }
            Self::Beans(other) => {
                write!(
                    f,
                    "The parsing engine encountered an internal error:\n{}",
                    other
                )
            }
            Self::TypeError(message) => {
                writeln!(f, "Typing error: {}", message)
            }
            Self::NoMainFunction => {
                writeln!(f, "No main function")
            }
            Self::IncorrectMainFunctionType { ty, params } => {
                writeln!(
                    f,
                    "The signature of the main function should be int(), not {}({})",
                    ty,
                    params.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
                )
            }
            Self::BreakContinueOutsideLoop => {
                writeln!(f, "break or continue statement outside of a loop")
            }
            Self::NameError => {
                writeln!(f, "unknown variable")
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
