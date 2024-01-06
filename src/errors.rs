use std::fmt::{Display, Formatter};
use std::io::{stdout, Write};
use std::process;
use std::process::exit;

use crate::lexer::TokenType;

#[derive(PartialEq, Debug)]
pub enum TerminationPolicy {
    STRICT,
    PERMISSIVE,
}

#[derive(Debug)]
pub struct ErrorScribe {
    errors: Vec<Error>,
    termination_policy: TerminationPolicy,
}

impl ErrorScribe {

    pub fn new() -> ErrorScribe {
        ErrorScribe {
            errors: vec![],
            termination_policy: TerminationPolicy::STRICT,
        }
    }

    pub fn from_termination_policy(termination_policy: TerminationPolicy) -> ErrorScribe {
        ErrorScribe {
            errors: vec![],
            termination_policy,
        }
    }

    pub fn annotate_error(&mut self, e: Error) {
        println!("{}", &e);
        stdout().flush().unwrap();
        self.errors.push(e);
        self.enact_termination_policy();
    }

    pub fn has_errors(&self) -> bool { !self.errors.is_empty() }

    pub fn clear_errors(&mut self) { self.errors.clear() }

    pub fn enact_termination_policy(&self) {
        if self.has_errors() && self.termination_policy == TerminationPolicy::STRICT {
            exit(1)
        }
    }
}

#[derive(Debug)]
pub enum ErrorType {
    //lexical errors
    UNEXPECTEDCHAR { symbol: char },
    BADSTRFMT,
    NONASCIICHARACTER { symbol: char },
    EXPECTEDLITERAL { found: TokenType },
    EXPECTEDTOKEN { ttype: TokenType },
    EXPECTEDTYPE,
    BADASSIGNMENTLHS,
}

#[derive(Debug)]
pub struct Error {
    etype: ErrorType,
    line: usize,
}

impl Error {
    pub fn on_line(line: usize, etype: ErrorType) -> Error {
        Error {
            etype,
            line,
        }
    }
    fn err_location(&self) -> String { format!("[line #{}] ", self.line) }
}

trait Red { fn red(&self) -> Self; }

impl Red for String {
    fn red(&self) -> Self { format!("{}{}{}", "\x1b[0;31m", self, "\x1b[0m") }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let msg = match &self.etype {
            ErrorType::UNEXPECTEDCHAR { symbol } => format!("unexpected character: {}", symbol),
            ErrorType::BADSTRFMT => String::from("string wasn't closed."),
            ErrorType::NONASCIICHARACTER { symbol } => format!("encountered non-ASCII character: {}", symbol),
            ErrorType::EXPECTEDLITERAL { found } => format!("expected literal, found: {:?}", found),
            ErrorType::EXPECTEDTOKEN { ttype } => format!("expected this token: {:?}", ttype),
            ErrorType::EXPECTEDTYPE => String::from("attempting to change the variable type without using 'into'"),
            ErrorType::BADASSIGNMENTLHS => String::from("only identifiers can be assigned values.")
        };
        f.write_str(&*(self.err_location() + &*msg).red())
    }
}