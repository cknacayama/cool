use std::{fs, process};

use cool::{
    checker::{MultiPassChecker, TypeChecker},
    ir_builder::IrBuilder,
};

pub fn run(input: &str) {
    match cool::compile(input) {
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
            let (start, end) = e.span.location(input);
            eprintln!(
                "Error: {} at {}:{}..{}:{}",
                e.kind, start.line, start.column, end.line, end.column
            );
        }
    }
}

fn main() {
    let input = fs::read_to_string("in/test.cl").unwrap();

    let (classes, env) = MultiPassChecker::from_input(&input)
        .unwrap()
        .check_all()
        .unwrap();

    let mut builder = IrBuilder::new(env);

    for class in classes {
        builder.build_class(class);
    }

    for instr in builder.instrs() {
        println!("{}", instr.kind);
    }

    let default_string = builder.strings().get("").unwrap().clone();
    for method in builder.methods_mut() {
        method.mem2reg();
        method.const_propagation(default_string.clone());
        method.dead_code_elimination();
    }

    for instr in builder.instrs() {
        println!("{}", instr.kind);
    }
}
