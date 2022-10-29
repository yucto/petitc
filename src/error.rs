use std::{error::Error as StdError, fmt, ops::RangeInclusive, path::Path};

#[derive(Debug)]
pub struct Location {
    pub line: usize,
    pub columns: RangeInclusive<usize>,
}

#[derive(Debug)]
pub struct Error<'a> {
    pub path: &'a Path,
    pub loc: Location,
    pub ty: ErrorType,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ErrorType {
    InternalError,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorType::InternalError => write!(f, "internal error"),
        }
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "File {}, line {}, characters {}-{}:\n{}",
            self.path.file_name().unwrap().to_string_lossy(),
            self.loc.line,
            *self.loc.columns.start(),
            *self.loc.columns.end(),
            self.ty,
        )
    }
}

impl<'a> StdError for Error<'a> {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match &self.ty {
            ErrorType::InternalError => None,
        }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
