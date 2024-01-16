use crate::evaluator::evaluate_expressions;
use std::env;
use std::fs::File;
use std::io::{Read, stdout, Write};
use std::path::Path;
use std::process::exit;

use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

use environment::Environment;

use crate::errors::ErrorScribe;
use crate::errors::TerminationPolicy::{PERMISSIVE, STRICT};

mod lexer;
mod errors;
mod parser;
mod shared;
mod evaluator;
mod environment;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        0..=1 => { serve_repl() }
        2 => { interpret_file(&args[1], false) }
        3 => { interpret_file(&args[1], true) }
        _ => { print!("too many arguments.") }
    }
}

fn interpret_file(filename: &str, verbose: bool) {
    let path = Path::new(filename);
    let mut file = match File::open(path) {
        Ok(f) => { f }
        Err(_) => { return serve_repl(); }
    };
    let mut content = String::new();
    file.read_to_string(&mut content)
        .expect("could not read file contents.");
    let mut es = ErrorScribe::from_termination_policy(STRICT);
    let mut env = Environment::new();
    interpret_instructions(&mut es, content, &mut env, verbose);
}

fn clear_terminal() {
    // if this doesn't work try this, cls for windows
    // std::process::Command::new("clear").status().unwrap();
    print!("{}[2J", 27 as char);
}

fn serve_repl() {
    let mut verbose = false;
    let mut es = ErrorScribe::from_termination_policy(PERMISSIVE);
    let mut env = Environment::new();
    let mut input_reader = DefaultEditor::new().unwrap();
    clear_terminal();
    println!("**START OF REPL** - q to quit, dbg to toggle debug mode");
    loop {
        let user_input = input_reader.readline(">> ");
        match user_input {
            Ok(line) => {
                input_reader.add_history_entry(line.as_str()).unwrap();
                match line.trim() {
                    "dbg" => {
                        verbose = !verbose;
                        println!("debug mode {}", if verbose { "ON" } else { "OFF" });
                        continue;
                    }
                    "q" => { exit(0); }
                    _ => { interpret_instructions(&mut es, line, &mut env, verbose); }
                }
            }
            Err(err) => {
                match err {
                    ReadlineError::Interrupted | ReadlineError::Eof => {exit(0)}
                    _ => {
                        println!("REPL ERROR");
                        continue;
                    }
                }
            }
        }
    }
}

fn interpret_instructions(es: &mut ErrorScribe, instructions: String, env: &mut Environment, verbose: bool) {
    let mut lexer = lexer::Lexer::from_string(instructions, es);
    let tokens = lexer.produce_tokens();
    if verbose {
        println!("produced following tokens: ");
        tokens.iter().for_each(|tok| println!("{}", tok));
    }
    let mut parser = parser::Parser::from_tokens(tokens.to_owned(), es);
    parser.parse();
    let exprs = parser.into_expressions();
    if es.has_errors() || exprs.is_empty() {
        es.clear_errors();
        return;
    }
    if verbose {
        println!("\nthis is parsed as:\n");
        for ex in &exprs {
            println!("{:#?}", ex);
        }
    }
    let input_exprs = exprs.iter().map(|ex| Box::new(ex.clone())).collect();
    let value = evaluate_expressions(&input_exprs, es, env, false);
    if verbose {
        println!("and evaluated as:\n{:?}", &value)
    } else {
        stdout().flush().unwrap();
        println!("\t{}", &value);
    }
}
