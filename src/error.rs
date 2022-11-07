use beans::{error::Error as BeansError, location::Span};
use std::fmt;
use thiserror::Error;

use crate::ast::Type;

#[derive(Debug, Error)]
pub struct Error {
    #[source]
    kind: ErrorKind,
    reason: Option<String>,
    helps: Vec<String>,
}

impl Error {
    pub(crate) fn reason(self, reason: String) -> Self {
        Error {
            reason: Some(reason),
            ..self
        }
    }

    pub(crate) fn add_help(mut self, help: String) -> Self {
        self.helps.push(help);
        self
    }

    pub(crate) fn new(kind: ErrorKind) -> Self {
        Error {
            kind,
            reason: None,
            helps: Vec::new(),
        }
    }
}

impl<T> From<T> for Error
where
    ErrorKind: From<T>,
{
    fn from(error: T) -> Self {
        Self::new(ErrorKind::from(error))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.kind.fmt(f)?;
        if let Some(ref reason) = self.reason {
            writeln!(f, "This error was triggered because {reason}")?;
        }
        for help in self.helps.iter() {
            writeln!(f, "  help: {help}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ErrorKind {
    Beans(#[from] BeansError),
    AddressOfRvalue {
        span: Span,
    },
    DerefNonPointer {
        ty: Type,
        span: Span,
    },
    NoMainFunction,
    IncorrectMainFunctionType {
        ty: crate::ast::Type,
        params: Vec<crate::ast::Type>,
        span: Span,
    },
    BreakContinueOutsideLoop {
        span: Span,
    },
    NameError {
        name: String,
        span: Span,
    },
    SymbolDefinedTwice {
        first_definition: Span,
        second_definition: Span,
        name: String,
    },
    FunctionDefinedTwice {
        first_definition: Option<Span>,
        second_definition: Span,
        name: String,
    },
    ArityMismatch {
        found_arity: usize,
        expected_arity: usize,
        span: Span,
        definition_span: Option<Span>,
        function_name: String,
    },
    VoidVariable {
        span: Span,
        name: String,
    },
    VoidExpression {
        span: Span,
    },
    VariableTypeMismatch {
        span: Span,
        definition_span: Span,
        expected_type: Type,
        found_type: Type,
        variable_name: String,
    },
    SizeofVoid {
        span: Span,
    },
    RvalueAssignment {
        span: Span,
    },
    TypeMismatch {
        span: Span,
        expected_type: Type,
        found_type: Type,
    },
    IncrOrDecrRvalue {
        span: Span,
    },
    BuiltinBinopTypeMismatch {
        left_type: Type,
        right_type: Type,
        span: Span,
        op: &'static str,
    },
}

fn display_locations(
    (start_line, start_column): (usize, usize),
    (end_line, end_column): (usize, usize),
    f: &mut fmt::Formatter,
) -> fmt::Result {
    if start_line == end_line {
        write!(f, "line {}, ", start_line + 1)?;
        if end_column - start_column <= 1 {
            write!(f, "character {}", start_column)
        } else {
            write!(f, "characters {}-{}", start_column, end_column)
        }
    } else {
        write!(
            f,
            "lines {}-{}, characters {}-{}",
            start_line + 1,
            end_line + 1,
            start_column,
            end_column
        )
    }
}

fn display_span(span: &Span, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "File \"{}\", ", span.file().display())?;
    display_locations(span.start(), span.end(), f)?;
    write!(f, ":\n")
}

fn display_relative_span(
    span: &Span,
    parent: &Span,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    if span.file() == parent.file() {
        if span.start().0 == span.end().0
            && parent.start().0 == parent.end().0
            && span.start().0 == parent.start().0
        {
            write!(f, "at the same line, ")?;
        } else {
            write!(f, "in the same file, ")?;
            if span.start().0 == span.end().0 {
                write!(f, "line {}, ", span.start().0 + 1)?;
            } else {
                write!(f, "lines {}-{}", span.start().0 + 1, span.end().0)?;
            }
        }
        if span.start().0 == span.end().0 && span.end().1 - span.start().1 <= 1
        {
            write!(f, "character {}", span.start().1)
        } else {
            write!(f, "characters {}-{}", span.start().1, span.end().1)
        }
    } else {
        write!(f, "in file \"{}\", ", span.file().display())?;
        display_locations(span.start(), span.end(), f)
    }
}

fn display_help_function_definitions(
    first_span: &Option<Span>,
    second_span: &Span,
    function_name: &str,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    if let Some(def_span) = first_span {
        write!(f, "  help: `{}` was defined ", function_name)?;
        display_relative_span(def_span, second_span, f)?;
    } else {
        write!(f, "  help: `{}` is a builtin function", function_name)?;
    }
    writeln!(f)
}

impl fmt::Display for ErrorKind {
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
                writeln!(
                    f,
                    "The parsing engine encountered an internal error:{}",
                    other
                )
            }
            Self::NoMainFunction => {
                writeln!(f, "Expected to find symbol `main` at topleve")
            }
            Self::IncorrectMainFunctionType { ty, params, span } => {
                display_span(span, f)?;
                writeln!(
                    f,
                    "Signature mismatch: `main` should be of type int(), not {}({}).",
                    ty,
                    params.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
                )
            }
            Self::BreakContinueOutsideLoop { span } => {
                display_span(span, f)?;
                writeln!(f, "Break or continue statement outside of a loop.")
            }
            Self::NameError { name, span } => {
                display_span(span, f)?;
                writeln!(f, "Symbol `{name}` is undefined.")
            }
            Self::AddressOfRvalue { span } => {
                display_span(span, f)?;
                writeln!(f, "This expression has no address in memory.")
            }
            Self::DerefNonPointer { ty, span } => {
                display_span(span, f)?;
                writeln!(f, "Cannot cast {ty} as _*")
            }
            Self::SymbolDefinedTwice {
                first_definition,
                second_definition,
                name,
            } => {
                display_span(second_definition, f)?;
                writeln!(
                    f,
                    "The symbol `{name}` is defined twice in the same block."
                )?;
                write!(f, "  help: the first definition was found ")?;
                display_relative_span(first_definition, second_definition, f)?;
                writeln!(f)
            }
            Self::ArityMismatch {
                found_arity,
                expected_arity,
                span,
                definition_span,
                function_name,
            } => {
                display_span(span, f)?;
                writeln!(f, "Arity mismatch between this function call and its definition.")?;
                writeln!(
                    f,
                    "  note: {} arguments were expected, but {} were given.",
                    expected_arity, found_arity
                )?;
                display_help_function_definitions(
                    definition_span,
                    span,
                    &function_name,
                    f,
                )
            }
            Self::FunctionDefinedTwice {
                first_definition,
                second_definition,
                name,
            } => {
                display_span(second_definition, f)?;
                writeln!(
                    f,
                    "The symbol `{name}` is defined twice at the toplevel."
                )?;
                display_help_function_definitions(
                    first_definition,
                    second_definition,
                    &name,
                    f,
                )
            }
            Self::VoidVariable { span, name } => {
                display_span(span, f)?;
                writeln!(
                    f,
                    "The symbol `{name}` has been declared with type `void`."
                )
            }
            Self::VariableTypeMismatch {
                span,
                definition_span,
                expected_type,
                found_type,
                variable_name,
            } => {
                display_span(span, f)?;
                writeln!(f, "The symbol `{variable_name}` has been defined with type `{expected_type}`, but it was assigned `{found_type}`.")?;
                write!(f, "  help: `{variable_name}` was defined ")?;
                display_relative_span(definition_span, span, f)?;
                writeln!(f)
            }
            Self::VoidExpression { span } => {
                display_span(span, f)?;
                writeln!(
                    f,
                    "This expression has type `void`, which is forbidden."
                )
            }
            Self::SizeofVoid { span } => {
                display_span(span, f)?;
                writeln!(f, "`void` does not have a size (this is not gcc).")
            }
            ErrorKind::RvalueAssignment { span } => {
                display_span(span, f)?;
                writeln!(f, "Cannot assign to such expression.")
            }
            ErrorKind::TypeMismatch {
                span,
                expected_type,
                found_type,
            } => {
                display_span(span, f)?;
                writeln!(f, "Expected type {expected_type}, found {found_type} instead.")
            }
            ErrorKind::IncrOrDecrRvalue { span } => {
		display_span(span, f)?;
		writeln!(f, "Cannot mutate such expression.")
	    },
            ErrorKind::BuiltinBinopTypeMismatch {
                left_type,
                right_type,
                span,
                op,
            } => {
		display_span(span, f)?;
		writeln!(f, "Invalid operands to binary `{op}`: {left_type} and {right_type}.")
	    },
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
