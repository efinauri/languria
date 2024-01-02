use std::fmt::{Display, Formatter};
use std::process;

use crate::lexer::Lexer;

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
        self.errors.push(e);
    }

    pub fn enact_termination_policy(&self) {
        if !self.errors.is_empty() && self.termination_policy == TerminationPolicy::STRICT {
            process::exit(0)
        }
    }
}
#[derive(Debug)]
pub enum ErrorType {
    //lexical errors
    UNEXPECTEDTOKEN { symbol: char },
    BADSTRFMT,
    NONASCIICHARACTER { symbol: char },
}
#[derive(Debug)]
pub struct Error {
    etype: ErrorType,
    line: usize,
    line_offset: usize,
}

impl Error {
    pub fn from_lexer_fault(l: &Lexer, etype: ErrorType) -> Error {
        Error {
            etype,
            line: l.counter.get(),
            line_offset: l.n_offset,
        }
    }
    fn err_location(&self) -> String { format!("[{},{}] ", self.line, self.line_offset) }
}

trait Red { fn red(&self) -> Self; }

impl Red for String {
    fn red(&self) -> Self { format!("{}{}{}", "\x1b[0;31m", self, "\x1b[0m") }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let msg = match self.etype {
            ErrorType::UNEXPECTEDTOKEN { symbol } => format!("unexpected token: {}", symbol),
            ErrorType::BADSTRFMT => String::from("bad string format."),
            ErrorType::NONASCIICHARACTER { symbol } => format!("rejected non-ASCII token: {}", symbol),
        };
        f.write_str(&*(self.err_location() + &*msg).red())
    }
}