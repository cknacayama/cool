use cool::{checker::SemanticChecker, lexer::Lexer, parser::Parser};

fn main() {
    let input = std::fs::read_to_string("in/test.cl").unwrap();
    let lexer = Lexer::new(&input);

    match SemanticChecker::new(Parser::new(lexer)).check() {
        Ok((asts, env)) => {}
        Err(e) => {
            let (start, end) = e.span.location(&input);
            eprintln!(
                "Error: {} at {}:{}..{}:{}",
                e.kind, start.line, start.column, end.line, end.column
            );
        }
    }
}
