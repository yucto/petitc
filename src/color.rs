pub use color_print::*;

#[macro_export]
macro_rules! cwrite {
    ($f:expr, $($rest:tt)*) => {{
	let string = $crate::color::cformat!($($rest)*);
	write!($f, "{}", string)
    }};
}

#[macro_export]
macro_rules! cwriteln {
    ($f:expr, $($rest:tt)*) => {{
	let string = $crate::color::cformat!($($rest)*);
	writeln!($f, "{}", string)
    }};
}
