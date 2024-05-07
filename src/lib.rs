use checker::SemanticResult;

pub mod ast;
pub mod checker;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;
pub mod types;

/// Compile the input string to x86_64 assembly code.
/// return the assembly code as a string.
pub fn compile(input: &str) -> SemanticResult<String> {
    let lexer = lexer::Lexer::new(input);
    let parser = parser::Parser::new(lexer);
    let checker = checker::SemanticChecker::new(parser);

    let (asts, env) = checker.check()?;

    let mut codegen = codegen::CodeGenerator::new(env);
    for ast in asts {
        codegen.gen_class(&ast);
    }
    Ok(codegen.take_output())
}
