use cool::checker::SemanticChecker;

fn main() {
    let input = std::fs::read_to_string("in/test.cl").unwrap();

    let checker: SemanticChecker = SemanticChecker::from_input(&input);

    match checker.check() {
        Ok((classes, env)) => {
            for class in classes {
                println!("{:#?}", class);
            }
            println!("{:#?}", env);
        }
        Err(err) => {
            let (start, end) = err.span.location(&input);

            eprintln!(
                "error: {} at {}:{}-{}:{}",
                err.kind, start.line, start.column, end.line, end.column
            );
        }
    }
}
