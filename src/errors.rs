use std::fmt::{Display, Formatter};
use std::process;

pub enum TerminationPolicy {
    STRICT,
    PERMISSIVE,
}

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

pub enum Error {
    //lexical errors
    UNEXPECTEDTOKEN { line: usize, line_offset: usize, symbol: char },
    BADSTRFMT { line: usize, line_offset: usize },
    NONASCIICHARACTER { line: usize, line_offset: usize },
}

fn err_location(line: &usize, line_offest: &usize) -> String {
    format!("[{},{}] ", line, line_offest)
}

trait Red { fn red(&self) -> Self; }

impl Red for String {
    fn red(&self) -> Self { format!("{}{}{}", "\x1b[0;31m", self, "\x1b[0m") }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UNEXPECTEDTOKEN { line, line_offset, symbol } =>
                f.write_str(&*format!("{}unexpected token: {}", err_location(line, line_offset), symbol)
                    .red()),
            Error::BADSTRFMT { line, line_offset } =>
                f.write_str(&*format!("{}bad string format.", err_location(line, line_offset))
                    .red()),
            Error::NONASCIICHARACTER { line, line_offset } =>
                f.write_str(&*format!("{}only ASCII characters are supported.", err_location(line, line_offset))
                    .red())
        }.expect("error during production of error message");
        Ok(())
    }
}