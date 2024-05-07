use std::{fs, process};

fn main() {
    let input = fs::read_to_string("in/test.cl").unwrap();

    match cool::compile(&input) {
        Ok(output) => {
            fs::write("out/test.s", output).unwrap();
            let gcc = process::Command::new("gcc-13")
                .args(["-nostdlib", "-static", "-o", "out/test", "out/test.s"])
                .output()
                .unwrap();
            if !gcc.status.success() {
                eprintln!("{}", String::from_utf8_lossy(&gcc.stderr));
                return;
            }

            let objdump = process::Command::new("objdump")
                .args(["-M", "intel", "-d", "out/test"])
                .output()
                .unwrap();
            fs::write("out/test.objdump", objdump.stdout).unwrap();
        }
        Err(e) => {
            let (start, end) = e.span.location(&input);
            eprintln!(
                "Error: {} at {}:{}..{}:{}",
                e.kind, start.line, start.column, end.line, end.column
            );
        }
    }
}
