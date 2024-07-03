use std::{io, path::PathBuf, rc::Rc};

use crate::{
    checker::{MultiPassChecker, SemanticError, TypeChecker},
    index_vec::IndexVec,
    ir::{builder::IrBuilder, opt::IrOptmizer, GlobalId, Instr},
    lexer::Lexer,
    llvm::compiler::Compiler,
    parser::Parser,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mode {
    Lex,
    Parse,
    Check,
    Ir,

    Opt,

    #[default]
    Llvm,
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
                "-S" => mode = Mode::Llvm,
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
                for function in ir_builder.functions_mut() {
                    function.set_labels();
                }
                let globals = ir_builder
                    .globals_names()
                    .map(|(id, name)| (id, name.clone()))
                    .collect();
                Ok(ir_output(ir_builder.instrs(), &globals))
            }
            Mode::Opt => {
                let checker = MultiPassChecker::from_input(&self.input)?;
                let (typed, env) = checker.check_all()?;
                let mut ir_builder = IrBuilder::new(env);
                for typed in typed {
                    ir_builder.build_class(typed);
                }
                let mut ir_opt = IrOptmizer::from_builder(ir_builder);
                ir_opt.optimize();
                Ok(ir_output(ir_opt.instrs(), ir_opt.globals()))
            }
            Mode::Llvm => {
                let checker = MultiPassChecker::from_input(&self.input)?;
                let (typed, env) = checker.check_all()?;
                let mut ir_builder = IrBuilder::new(env);
                for typed in typed {
                    ir_builder.build_class(typed);
                }
                let mut ir_opt = IrOptmizer::from_builder(ir_builder);
                ir_opt.optimize();
                let IrOptmizer {
                    functions,
                    vtables,
                    globals,
                    strings,
                } = ir_opt;

                let functions = functions
                    .into_iter()
                    .map(|f| f.function)
                    .collect::<Vec<_>>();

                let mut compiler = Compiler::new(globals, vtables, strings);

                for func in functions {
                    compiler.compile_fn(&func);
                }

                Ok(compiler.finish())
            }
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

fn ir_output<'a, T>(instrs: T, globals: &IndexVec<GlobalId, Rc<str>>) -> String
where
    T: IntoIterator<Item = &'a Instr>,
{
    let mut output = String::new();
    for instr in instrs {
        output.push_str(&instr.to_ir_string(globals));
        output.push('\n');
    }
    output
}
