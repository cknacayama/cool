use std::process;

use cool::cli::Config;

fn main() {
    let now = std::time::Instant::now();
    let config = Config::build(std::env::args()).unwrap_or_else(|e| {
        eprintln!("{}", e);
        process::exit(1)
    });

    if let Err(e) = config.run() {
        config.report_error(e);
    }
    eprintln!("Elapsed time: {:?}", now.elapsed());
}
