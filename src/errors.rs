use std::fmt::{Display, format, Formatter};
use std::io::{stderr, Write};

pub struct ErrorScribe {
    errors: Vec<Error>,
}

impl ErrorScribe {
    pub fn new() -> ErrorScribe {
        ErrorScribe {
            errors: vec![],
        }
    }

    pub fn annotate_error(&mut self, e: Error) {
        println!("{}", &e);
        self.errors.push(e);
    }
}

pub enum Error {
    UNEXPECTEDTOKEN { line: usize, line_offset: usize, symbol: char },
    BADSTRFMT { line: usize, line_offset: usize },
    NONASCIICHARACTER { line: usize, line_offset: usize },
}

fn err_location(line: &usize, line_offest: &usize) -> String {
    format!("[{},{}] ", line, line_offest)
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UNEXPECTEDTOKEN { line, line_offset, symbol } =>
                f.write_str(&*format!("{}unexpected token: {}", err_location(line, line_offset), symbol)),
            Error::BADSTRFMT { line, line_offset } =>
                f.write_str(&*format!("{}bad string format.", err_location(line, line_offset))),
            Error::NONASCIICHARACTER { line, line_offset} =>
                f.write_str(&*format!("{}only ASCII characters are supported.", err_location(line, line_offset)))
        }.expect("TODO: panic message");
        Ok(())
    }
}