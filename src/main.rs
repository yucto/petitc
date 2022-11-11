use clap::Parser;

use std::{
    borrow::Cow,
    fs,
    path::PathBuf,
    process::{exit, ExitCode},
};

use petitc::error::Error;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    path: PathBuf,
    #[arg(long, default_value_t = false)]
    parse_only: bool,
    #[arg(long, default_value_t = false)]
    type_only: bool,
}

fn report_error(err: Error) -> ! {
    eprint!("{}", err);
    exit(1)
}

fn main() -> ExitCode {
    std::panic::set_hook(Box::new(panic_hook));

    let args = {
        let mut args = Cli::parse();
        args.path = match args.path.canonicalize() {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Fatal error: {}", e);
                exit(1)
            }
        };
        args
    };

    match args.path.extension() {
        Some(ext) if "c" == &ext.to_string_lossy()[..] => (),
        _ => {
            let meta = fs::metadata(&args.path);
            match meta {
                Ok(meta) => {
                    if meta.is_file() {
                        eprintln!(
                            "Fatal error: file {} extension is not \".c\"",
                            args.path.file_name().unwrap().to_string_lossy()
                        );
                    } else {
                        eprintln!(
                            "Fatal error: {} is a directory",
                            args.path.file_name().unwrap().to_string_lossy()
                        );
                    }
                }
                Err(e) => eprintln!("Fatal error: {}", e),
            };
            exit(1)
        }
    }

    let (parsed, string_store) =
        petitc::parse(&args.path).map_err(report_error).unwrap();

    if args.parse_only {
        return ExitCode::from(0);
    }

    let typed = petitc::typecheck(&args.path, parsed, &string_store)
        .map_err(report_error)
        .unwrap();

    if args.type_only {
        return ExitCode::from(0);
    }

    let _ = petitc::compile(&args.path, typed)
        .map_err(report_error)
        .unwrap();

    ExitCode::from(0)
}

fn panic_hook(info: &std::panic::PanicInfo<'_>) {
    let payload = info.payload();
    let msg = if let Some(msg) = payload.downcast_ref::<&str>() {
        msg
    } else if let Some(msg) = payload.downcast_ref::<String>() {
        msg
    } else {
        "unknown cause"
    };

    // Current implementation always returns the Some variant
    let location = if let Some(loc) = info.location() {
        Cow::Owned(format!(
            "File \"{}\", line {}, character {}:",
            loc.file(),
            loc.line(),
            loc.column()
        ))
    } else {
        Cow::Borrowed("Unknown location:")
    };

    eprintln!("{}\nFatal internal error: {}", location, msg);

    exit(2)
}
