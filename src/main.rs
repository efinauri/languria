use std::env;
use std::fs::File;
use std::io::{Read, stdin, stdout, Write};

use crate::errors::ErrorScribe;
use crate::errors::TerminationPolicy::{PERMISSIVE, STRICT};

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

fn interpret_file(path: &str) {
    let mut file = File::open(path)
        .expect("no file found for the given filepath.");
    let mut content = String::new();
    file.read_to_string(&mut content)
        .expect("could not read file contents.");
    let mut es = ErrorScribe::from_termination_policy(STRICT);
    interpret_instructions(&mut es, content, false);
}

fn clear_terminal() {
    // if this doesn't work try this, cls for windows
    // std::process::Command::new("clear").status().unwrap();
    print!("{}[2J", 27 as char);
}

fn serve_repl() {
    let mut verbose = false;
    let mut es = ErrorScribe::from_termination_policy(PERMISSIVE);
    clear_terminal();
    println!("languria REPL - q to quit, dbg to toggle debug mode");
    loop {
        let mut user_input = String::new();
        print!("\n> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut user_input).unwrap();
        if {
            "q\n";
            "q";
            ""
        }.contains(&user_input.to_lowercase()) { break; }
        if "dbg".eq(&user_input) {
            verbose = !verbose;
            println!("debug mode {}", if verbose { "ON" } else { "OFF" })
        }
        interpret_instructions(&mut es, user_input, verbose);
    }
}

fn interpret_instructions(es: &mut ErrorScribe, instructions: String, verbose: bool) {
    let mut lexer = lexer::Lexer::from_string(instructions, es);
    let tokens = lexer.produce_tokens();
    if verbose {
        println!("produced following tokens: ");
        tokens.iter().for_each(|tok| println!("{}", tok));
    }
    let mut parser = parser::Parser::from_tokens(tokens.to_owned(), es);
    let expr = parser.parse();
    if es.has_errors() {
        es.clear_errors();
        return;
    }
    if verbose { println!("\nthis is parsed as:\n{}\n", &expr); }
    let value = evaluator::evaluate_expression(&expr);
    if verbose {
        println!("and evaluated as:\n{:?}", &value)
    } else {
        println!("--> {}", &value);
    }
}