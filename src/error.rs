use std::{error::Error as StdError, fmt, ops::RangeInclusive, path::PathBuf};

#[derive(Debug)]
pub struct Location {
    pub line: usize,
    pub columns: RangeInclusive<usize>,
}

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub loc: Location,
    pub ty: ErrorType,
}

#[derive(Debug)]
pub enum ErrorType {}

impl fmt::Display for ErrorType {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        match *self {}
    }
}

impl fmt::Display for Error {
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

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self.ty {}
    }
}

pub type Result<T> = std::result::Result<T, Error>;
