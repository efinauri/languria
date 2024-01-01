use std::env;
use std::fs::File;
use std::io::{Read, stdin};

use crate::errors::ErrorScribe;
use crate::errors::TerminationPolicy::{PERMISSIVE, STRICT};

mod lexer;
mod errors;
mod parser;
mod shared;


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
    interpret(&mut es, content);
}

fn clear_terminal() {
    // if this doesn't work try this, cls for windows
    //std::process::Command::new("clear").status().unwrap();
    print!("{}[2J", 27 as char);
}

fn serve_repl() {
    let mut es = ErrorScribe::from_termination_policy(PERMISSIVE);
    clear_terminal();
    loop {
        let mut user_input = String::new();
        print!("\n> ");
        stdin().read_line(&mut user_input).unwrap();
        if {
            "q\n";
            "q";
            ""
        }.contains(&user_input.to_lowercase()) { break; }
        interpret(&mut es, user_input);
    }
}

fn interpret(es: &mut ErrorScribe, instructions: String) {
    let mut lexer = lexer::Lexer::from_string(instructions, es);
    let tokens = lexer.produce_tokens();
    println!("produced following tokens: ");
    tokens.iter().for_each(|tok| println!("{}", tok));
    let mut parser = parser::Parser::from_tokens(tokens.to_owned());
    let expr = parser.parse();
    println!("this is parsed as: {:?}", expr);
}
