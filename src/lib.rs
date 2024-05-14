pub mod ast;
pub mod checker;
pub mod index_vec;
pub mod ir;
pub mod ir_builder;
pub mod lexer;
pub mod opt;
pub mod parser;
pub mod span;
pub mod token;
pub mod types;

use checker::SemanticResult;

/// Compile the input string to x86_64 assembly code.
/// return the assembly code as a string.
pub fn compile(input: &str) -> SemanticResult<'_, String> {
    let _ = input;
    todo!()
}
