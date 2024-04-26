#![deny(unsafe_op_in_unsafe_fn, clippy::absurd_extreme_comparisons)]

pub mod ast;
pub mod checker;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;
pub mod types;
