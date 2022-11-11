use crate::{ast::Type, cwrite, cwriteln};
use beans::{error::Error as BeansError, span::Span};
use colored::Colorize;
use std::fmt;
use thiserror::Error;

macro_rules! error {
    ($f:expr, $($rest:tt)*) => {{
	let string = $crate::color::cformat!($($rest)*);
	cwriteln!($f, "<s,r!>error</><s,w!>: {}</>", string)
    }};
}

#[derive(Debug, Error)]
pub struct Error {
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
        let helps: &mut dyn Iterator<Item = _> = &mut self.helps.iter();
        match &self.kind {
            ErrorKind::Beans(BeansError::SyntaxError { message, location }) => {
                let span = location.get();
                display::span(span, f)?;
                writeln!(f, "Syntax error: {}", message)?;
            }
            ErrorKind::Beans(BeansError::LexingError { message, location }) => {
                let span = location.get();
                display::span(&span, f)?;
                writeln!(f, "Lexing error: {}", message)?;
            }
            ErrorKind::Beans(other) => {
                writeln!(
                    f,
                    "The parsing engine encountered an internal error:{}",
                    other
                )?;
            }
            ErrorKind::NoMainFunction => {
                error!(f, "expected to find symbol `main` at toplevel")?;
            }
            ErrorKind::IncorrectMainFunctionType { ty, params, span } => {
                display::span(span, f)?;
                error!(
		    f,
		    "signature mismatch, `main` should be of type `int()`, not `{}({})`",
		    ty,
		    params.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "),
		)?;
                display::pretty_span(span, None, None, f)?;
            }
            ErrorKind::BreakContinueOutsideLoop { span } => {
                display::span(span, f)?;
                error!(f, "break or continue statement outside of a loop")?;
                display::pretty_span(
                    span,
                    Some("`break` and `continue` must be within loops"),
                    None,
                    f,
                )?;
            }
            ErrorKind::NameError { name, span } => {
                display::span(span, f)?;
                error!(f, "symbol `{}` is undefined.", name)?;
                display::pretty_span(
                    span,
                    Some("not found in this scope"),
                    None,
                    f,
                )?;
            }
            ErrorKind::AddressOfRvalue {
                span,
                expression_span,
            } => {
                display::span(span, f)?;
                error!(f, "cannot take the address of an rvalue expression")?;
                display::pretty_span(
                    expression_span,
                    Some("this expression has no address in memory"),
                    None,
                    f,
                )?;
            }
            ErrorKind::DerefNonPointer { ty, span } => {
                display::span(span, f)?;
                error!(f, "cannot cast `{}` as `_*`", ty)?;
                display::pretty_span(
                    span,
                    Some("this is not a pointer, it cannot be dereferenced"),
                    None,
                    f,
                )?;
            }
            ErrorKind::SymbolDefinedTwice {
                first_definition,
                second_definition,
                name,
            } => {
                display::span(second_definition, f)?;
                error!(
                    f,
                    "the symbol `{}` is defined twice in the same block", name,
                )?;
                display::pretty_span(
                    second_definition,
                    Some("second definition found here"),
                    None,
                    f,
                )?;
                cwrite!(f, "The first definition was found ")?;
                display::relative_span(first_definition, second_definition, f)?;
                writeln!(f)?;
                display::pretty_span(
                    first_definition,
                    Some("first definition found here"),
                    None,
                    f,
                )?;
            }
            ErrorKind::ArityMismatch {
                found_arity,
                expected_arity,
                span,
                definition_span,
                function_name,
            } => {
                display::span(span, f)?;
                error!(
		    f,
		    "arity mismatch between this function call and its definition.",
		)?;
                display::pretty_span(
                    span,
                    Some("not the right number of arguments"),
                    None,
                    f,
                )?;
                cwriteln!(
                    f,
                    "  <s,b!>=</> <s,w!>note: {} arguments were expected, but {} were given</>",
                    expected_arity, found_arity
                )?;
                display::help_function_definitions(
                    definition_span,
                    span,
                    &function_name,
                    f,
                )?;
            }
            ErrorKind::FunctionDefinedTwice {
                first_definition,
                second_definition,
                name,
            } => {
                display::span(second_definition, f)?;
                error!(
                    f,
                    "the symbol `{}` is defined twice at the toplevel", name
                )?;
                display::pretty_span(
                    second_definition,
                    Some("second definition found here"),
                    None,
                    f,
                )?;
                display::help_function_definitions(
                    first_definition,
                    second_definition,
                    &name,
                    f,
                )?;
                if let Some(first_definition) = first_definition {
                    display::pretty_span(
                        first_definition,
                        Some("first definition found here"),
                        None,
                        f,
                    )?;
                }
            }
            ErrorKind::VoidVariable { span, name } => {
                display::span(span, f)?;
                error!(
                    f,
                    "this symbol `{}` has been declared with type `void`", name,
                )?;
                display::pretty_span(
                    span,
                    Some("`void` is not a valid type for a variable"),
                    None,
                    f,
                )?;
            }
            ErrorKind::VariableTypeMismatch {
                span,
                definition_span,
                expected_type,
                found_type,
                variable_name,
            } => {
                display::span(span, f)?;
                error!(
		    f,
		    "the symbol `{}` has been defined with type `{}`, but it was assigned `{}`",
		    variable_name,
		    expected_type,
		    found_type,
		)?;
                display::pretty_span(span, None, None, f)?;
                write!(f, "  help: `{}` was defined ", variable_name.bold())?;
                display::relative_span(definition_span, span, f)?;
                writeln!(f)?;
                display::pretty_span(definition_span, None, None, f)?;
            }
            ErrorKind::VoidExpression { span } => {
                display::span(span, f)?;
                error!(
                    f,
                    "this expression has type `void`, which is forbidden",
                )?;
                display::pretty_span(span, None, None, f)?;
                cwriteln!(
		    f,
		    "  <s,b!>=</> <s,w!>note:</> expressions with type `void` can only be discarded"
		)?;
            }
            ErrorKind::SizeofVoid { span } => {
                display::span(span, f)?;
                error!(f, "`void` does not have a size",)?;
                display::pretty_span(span, None, None, f)?;
                cwriteln!(f, "  <s,b!>=</> <s,w!>hint: <i>use gcc!</></>")?;
            }
            ErrorKind::RvalueAssignment { span } => {
                display::span(span, f)?;
                error!(
		    f,
		    "only expressions that are <i>lvalues</> can be assigned to",
		)?;
                display::pretty_span(
                    span,
                    Some("cannot assign to this expression"),
		    None,
                    f,
                )?;
            }
            ErrorKind::TypeMismatch {
                span,
                expected_type,
                found_type,
            } => {
                display::span(span, f)?;
                error!(
                    f,
                    "expected type {}, found {} instead",
                    expected_type,
                    found_type,
                )?;
                display::pretty_span(span, Some("wrong type"), None, f)?;
            }
            ErrorKind::IncrOrDecrRvalue {
                span,
                expression_span,
            } => {
                display::span(span, f)?;
                error!(f, "only <i>lvalue</> expressions can be mutated")?;
                display::pretty_span(
                    expression_span,
                    Some("cannot mutate this expression"),
		    None,
                    f,
                )?;
            }
            ErrorKind::BuiltinBinopTypeMismatch {
                left_type,
                right_type,
                span,
                op,
            } => {
                display::span(span, f)?;
                error!(
                    f,
                    "invalid operands to binary `{}` for `{}` and `{}`",
                    op,
                    left_type,
                    right_type,
                )?;
                display::pretty_span(
                    span,
                    Some("invalid arithmetic operation"),
		    None,
                    f,
                )?;
            }
        };
        for help in helps {
            cwriteln!(f, "  <s,b!>=</> <s,w!>help:</> {}", help)?;
        }
        if let Some(ref reason) = self.reason {
            cwriteln!(f, "  <s,b!>=</> <s,w!>note:</> this error was triggered because {}", reason)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Beans(BeansError),
    AddressOfRvalue {
        span: Span,
        expression_span: Span,
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
        expression_span: Span,
    },
    BuiltinBinopTypeMismatch {
        left_type: Type,
        right_type: Type,
        span: Span,
        op: &'static str,
    },
}

impl From<BeansError> for ErrorKind {
    fn from(error: BeansError) -> Self {
        Self::Beans(error)
    }
}

mod display {
    use super::*;

    pub fn span(span: &Span, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "File \"{}\", ", span.file().display())?;
        locations(span.start(), span.end(), f)?;
        write!(f, ":\n")
    }

    pub fn locations(
        (start_line, start_column): (usize, usize),
        (end_line, end_column): (usize, usize),
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        if start_line == end_line {
            write!(f, "line {}, ", start_line + 1)?;
            if start_column == end_column {
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

    pub fn single_line(
        span: &Span,
        left_column_size: usize,
        line_nb: usize,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let (line_begins_at, line_ends_at) = span.line_bytes_of_line(line_nb);
        left_column_with(left_column_size, line_nb + 1, f)?;
        writeln!(
            f,
            "{}",
            span.text()[line_begins_at..line_ends_at].trim_end(),
        )
    }

    pub fn left_column_with(
        left_column_size: usize,
        content: impl fmt::Display,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        cwrite!(
            f,
            "{: <left_column_size$} <s,b!>|</> ",
            content,
            left_column_size = left_column_size,
        )
    }

    pub fn left_column(
        left_column_size: usize,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        cwrite!(f, "{} <s,b!>|</> ", " ".repeat(left_column_size),)
    }

    pub fn left_column_newline(
        left_column_size: usize,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        left_column(left_column_size, f)?;
        writeln!(f)
    }

    pub fn error_report(
        span: &Span,
        left_column_size: usize,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        cwriteln!(
            f,
            "{}<s,b!>--></> {}:{}:{}",
            " ".repeat(left_column_size),
            span.file().file_name().unwrap().to_str().unwrap(),
            span.start().0 + 1,
            span.start().1,
        )
    }

    pub fn comment(
        underline_filler: &'static str,
        offset: usize,
        length: usize,
        left_column_size: usize,
        message: Option<&'static str>,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        left_column(left_column_size, f)?;
        cwriteln!(
            f,
            "{}<s,r!>{} {}</>",
            " ".repeat(offset),
            underline_filler.repeat(length),
            message.unwrap_or_default(),
        )
    }

    pub fn pretty_span(
        span: &Span,
        message: Option<&'static str>,
        _hint: Option<(&Span, &'static str)>,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        if span.start().0 == span.end().0 {
            let left_column_size = (span.start().0 + 1).to_string().len();
            let reported_length = span.end().1 - span.start().1 + 1;
            error_report(span, left_column_size, f)?;
            left_column_newline(left_column_size, f)?;
            single_line(span, left_column_size, span.start().0, f)?;
            comment(
                "^",
                span.start_byte() - span.lines()[span.start().0],
                reported_length,
                left_column_size,
                message,
                f,
            )?;
            left_column_newline(left_column_size, f)?;
        } else if span.end().0 - span.start().0 <= 3 {
            let left_column_size = (span.end().0 + 1).to_string().len();
            error_report(span, left_column_size, f)?;
            left_column_newline(left_column_size, f)?;
            single_line(span, left_column_size, span.start().0, f)?;
            comment(
                "^",
                span.start_byte() - span.lines()[span.start().0],
                1,
                left_column_size,
                None,
                f,
            )?;
            for line in span.start().0 + 1..span.end().0 {
                single_line(span, left_column_size, line, f)?;
            }
            single_line(span, left_column_size, span.end().0, f)?;
            comment(
                "^",
                span.end_byte() - span.lines()[span.end().0],
                1,
                left_column_size,
                message,
                f,
            )?;
            left_column_newline(left_column_size, f)?;
        } else {
            todo!()
        }
        Ok(())
    }

    pub fn relative_span(
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
                    write!(
                        f,
                        "lines {}-{}",
                        span.start().0 + 1,
                        span.end().0 + 1
                    )?;
                }
            }
            if span.start().0 == span.end().0
                && span.end().1 - span.start().1 <= 1
            {
                write!(f, "character {}", span.start().1)
            } else {
                write!(f, "characters {}-{}", span.start().1, span.end().1)
            }
        } else {
            write!(f, "in file \"{}\", ", span.file().display())?;
            locations(span.start(), span.end(), f)
        }
    }

    pub fn help_function_definitions(
        first_span: &Option<Span>,
        second_span: &Span,
        function_name: &str,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        if let Some(def_span) = first_span {
            cwrite!(
                f,
                "  <s,b!>=</> <s,w!>help:</> `{}` was defined ",
                function_name
            )?;
            relative_span(def_span, second_span, f)?;
        } else {
            cwrite!(
                f,
                "  <s,b!>=</> <s,w!>help:</> `{}` is a builtin function",
                function_name
            )?;
        }
        writeln!(f)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
