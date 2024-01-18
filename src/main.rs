use std::env;
use languria::user_input::{interpret_file, serve_repl};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        0..=1 => { serve_repl() }
        2 => { interpret_file(&args[1], false) }
        3 => { interpret_file(&args[1], true) }
        _ => { print!("too many arguments.") }
    }
}