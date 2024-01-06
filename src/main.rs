use std::env;
use std::fs::File;
use std::io::{Read, stdin, stdout, Write};
use std::path::Path;

use crate::errors::ErrorScribe;
use crate::errors::TerminationPolicy::{PERMISSIVE, STRICT};
use crate::evaluator::Scope;

mod lexer;
mod errors;
mod parser;
mod shared;
mod evaluator;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        0..=1 => { serve_repl() }
        2 => { interpret_file(&args[1]) }
        _ => { print!("too many arguments.") }
    }
}

fn interpret_file(filename: &str) {
    let path = Path::new(filename);
    let mut file = match File::open(path) {
        Ok(f) => { f }
        Err(_) => { return serve_repl(); }
    };
    let mut content = String::new();
    file.read_to_string(&mut content)
        .expect("could not read file contents.");
    let mut es = ErrorScribe::from_termination_policy(STRICT);
    let mut main_scope = Scope::new();
    main_scope.register_entrypoint(path.file_name().unwrap());
    interpret_instructions(&mut es, content, &mut main_scope, false);
}

fn clear_terminal() {
    // if this doesn't work try this, cls for windows
    // std::process::Command::new("clear").status().unwrap();
    print!("{}[2J", 27 as char);
}

fn serve_repl() {
    let mut verbose = false;
    let mut es = ErrorScribe::from_termination_policy(PERMISSIVE);
    let mut main_scope = Scope::new();
    clear_terminal();
    println!("**START OF REPL** - q to quit, dbg to toggle debug mode");
    loop {
        let mut user_input = String::new();
        print!("\n> ");
        stdout().flush().unwrap();
        match stdin().read_line(&mut user_input) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("REPL error: {}", err);
                continue
            }
        };
        if {
            "q\n";
            "q";
            ""
        }.contains(&user_input.to_lowercase()) { break; }
        if user_input.starts_with("dbg") {
            verbose = !verbose;
            println!("debug mode {}", if verbose { "ON" } else { "OFF" });
            continue;
        }
        interpret_instructions(&mut es, user_input, &mut main_scope, verbose);
        main_scope.reset_print();
    }
}

fn interpret_instructions(es: &mut ErrorScribe, instructions: String, ms: &mut Scope, verbose: bool) {
    let mut lexer = lexer::Lexer::from_string(instructions, es);
    let tokens = lexer.produce_tokens();
    if verbose {
        println!("produced following tokens: ");
        tokens.iter().for_each(|tok| println!("{}", tok));
    }
    let mut parser = parser::Parser::from_tokens(tokens.to_owned(), es);
    let exprs = parser.parse();
    if es.has_errors() || exprs.is_empty() {
        es.clear_errors();
        return;
    }
    let expr = exprs.first().unwrap();
    if verbose { println!("\nthis is parsed as:\n{}\n", &expr); }
    let value = evaluator::evaluate_expressions(exprs, ms, es);
    if verbose {
        println!("and evaluated as:\n{:?}", &value)
    } else {
        println!("\n--> {}", &value);
    }
}