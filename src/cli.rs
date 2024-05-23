use std::{io, path::PathBuf};

use crate::{
    checker::{MultiPassChecker, SemanticError, TypeChecker},
    ir::{builder::IrBuilder, opt},
    lexer::Lexer,
    parser::Parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mode {
    Lex,
    Parse,
    Check,
    Ir,

    #[default]
    Opt,
    Asm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    executable:  PathBuf,
    mode:        Mode,
    input_path:  PathBuf,
    input:       String,
    output_path: Option<PathBuf>,
}

#[derive(Debug)]
pub enum Error<'a> {
    InvalidMode(String),
    InvalidInputPath(String),
    InvalidArg(String),
    NoInput,
    Io(io::Error),
    Semantic(SemanticError<'a>),
}

impl<'a> std::fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidMode(mode) => write!(f, "Invalid mode: {}", mode),
            Self::InvalidInputPath(path) => write!(f, "Invalid input path: {}", path),
            Self::InvalidArg(arg) => write!(f, "Invalid argument: {}", arg),
            Self::NoInput => write!(f, "No input file"),
            Self::Io(e) => write!(f, "I/O error: {}", e),
            Self::Semantic(e) => write!(f, "Semantic error: {}", e),
        }
    }
}

impl<'a> std::error::Error for Error<'a> {
}

impl From<io::Error> for Error<'_> {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl<'a, T> From<T> for Error<'a>
where
    T: Into<SemanticError<'a>>,
{
    fn from(e: T) -> Self {
        Self::Semantic(e.into())
    }
}

impl Config {
    fn print_help(mut stream: impl io::Write) -> io::Result<()> {
        writeln!(stream, "Usage: coolc [options] <input>")?;
        writeln!(stream, "Options:")?;
        writeln!(stream, "  -L        Lex only")?;
        writeln!(stream, "  -P        Parse only")?;
        writeln!(stream, "  -C        Check only")?;
        writeln!(stream, "  -I        Generate IR")?;
        writeln!(stream, "  -O        Optimize IR")?;
        writeln!(stream, "  -S        Generate assembly")?;
        writeln!(stream, "  -o <path> Output file path")?;
        writeln!(stream, "  -h        Print this help message")?;
        Ok(())
    }

    pub fn build(mut args: impl Iterator<Item = String>) -> Result<Self, Error<'static>> {
        let executable = args.next().ok_or(Error::NoInput)?.into();

        let mut mode = Mode::default();
        let mut input_path = None;
        let mut output_path = None;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-L" => mode = Mode::Lex,
                "-P" => mode = Mode::Parse,
                "-C" => mode = Mode::Check,
                "-I" => mode = Mode::Ir,
                "-O" => mode = Mode::Opt,
                "-S" => mode = Mode::Asm,
                "-o" => {
                    let path = args.next().ok_or(Error::InvalidInputPath(arg))?;
                    output_path = Some(PathBuf::from(path));
                }
                "-h" => {
                    Self::print_help(&mut io::stdout())?;
                    return Err(Error::InvalidMode(arg));
                }
                _ if arg.starts_with('-') => return Err(Error::InvalidMode(arg)),
                _ if input_path.is_none() => {
                    input_path = Some(PathBuf::from(arg));
                }
                _ => return Err(Error::InvalidArg(arg)),
            }
        }

        let Some(input_path) = input_path else {
            return Err(Error::NoInput);
        };

        let input = std::fs::read_to_string(&input_path)?;

        Ok(Self {
            executable,
            mode,
            input_path,
            input,
            output_path,
        })
    }

    pub fn run(&self) -> Result<(), Error> {
        let output = self.output()?;
        if let Some(output_path) = &self.output_path {
            std::fs::write(output_path, output)?;
        } else {
            println!("{}", output);
        }
        Ok(())
    }

    fn output(&self) -> Result<String, Error> {
        match self.mode {
            Mode::Lex => {
                let lexer = Lexer::new(&self.input);
                let tokens = lexer.collect::<Result<Vec<_>, _>>()?;
                let mut output = String::new();
                for token in tokens {
                    output.push_str(&format!("{:?}\n", token));
                }
                Ok(output)
            }
            Mode::Parse => {
                let parser = Parser::from_input(&self.input);
                let ast = parser.collect::<Result<Vec<_>, _>>()?;
                Ok(format!("{:#?}", ast))
            }
            Mode::Check => {
                let checker = MultiPassChecker::from_input(&self.input)?;
                let typed = checker.collect::<Result<Vec<_>, _>>()?;
                Ok(format!("{:#?}", typed))
            }
            Mode::Ir => {
                let checker = MultiPassChecker::from_input(&self.input)?;
                let (typed, env) = checker.check_all()?;
                let mut ir_builder = IrBuilder::new(env);
                for typed in typed {
                    ir_builder.build_class(typed);
                }
                for method in ir_builder.methods_mut() {
                    method.set_labels();
                }
                let mut output = String::new();
                for ir in ir_builder.instrs().map(|instr| &instr.kind) {
                    output.push_str(&format!("{}\n", ir));
                }
                Ok(output)
            }
            Mode::Opt => {
                let checker = MultiPassChecker::from_input(&self.input)?;
                let (typed, env) = checker.check_all()?;
                let mut ir_builder = IrBuilder::new(env);
                for typed in typed {
                    ir_builder.build_class(typed);
                }
                let default_string = ir_builder.strings().get("").unwrap().clone();
                for method in ir_builder.methods_mut() {
                    opt::optimize(method, &default_string);
                }
                let mut output = String::new();
                for ir in ir_builder.instrs().map(|instr| &instr.kind) {
                    output.push_str(&format!("{}\n", ir));
                }
                Ok(output)
            }
            Mode::Asm => todo!(),
        }
    }

    pub fn report_error(&self, e: Error) -> ! {
        if let Error::Semantic(e) = e {
            let (start, _) = e.span.location(&self.input);
            eprintln!(
                "{}:{}:{}: {}",
                self.input_path.display(),
                start.line,
                start.column,
                e.kind
            );
        } else {
            eprintln!("{}", e);
        }
        std::process::exit(1);
    }
}
