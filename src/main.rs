use clap::Parser;

use std::{
    fs,
    path::PathBuf,
    process::{exit, ExitCode},
};

use petitc::error::{Error, ErrorType};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    path: PathBuf,
    #[arg(long, default_value_t = false)]
    parse_only: bool,
    #[arg(long, default_value_t = false)]
    type_only: bool,
}

fn report_error<'a>(err: Error<'a>) -> ! {
    eprintln!("{}", err);
    match err.ty {
        ErrorType::InternalError => exit(2),
        _ => exit(1),
    }
}

fn main() -> ExitCode {
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

    let parsed = petitc::parse(&args.path).map_err(report_error).unwrap();

    if args.parse_only {
        return ExitCode::from(0);
    }

    let typed = petitc::typecheck(&args.path, parsed).map_err(report_error).unwrap();

    if args.type_only {
        return ExitCode::from(0);
    }

    let _ = petitc::compile(&args.path, typed).map_err(report_error).unwrap();

    ExitCode::from(0)
}