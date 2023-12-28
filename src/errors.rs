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
        self.errors.push(e);
    }
}

pub enum Error {
    UNKNOWNSYMBOL {line: usize, line_offset: usize, symbol: char},
    BADSTRFMT {line: usize, line_offset: usize}
}