use std::fmt::{Display, Formatter};
use std::io::{stdout, Write};
use std::process::exit;

use crate::environment::value::Value;
use crate::lexer::{Coord, TokenType};
use crate::user_io::Red;

#[derive(PartialEq, Debug)]
pub enum TerminationPolicy {
    STRICT,
    PERMISSIVE,
}

#[derive(Debug)]
pub struct ErrorScribe {
    pub errors: Vec<Error>,
    pub termination_policy: TerminationPolicy,
}

impl ErrorScribe {
    pub fn from_termination_policy(termination_policy: TerminationPolicy) -> ErrorScribe {
        ErrorScribe {
            errors: vec![],
            termination_policy,
        }
    }

    pub fn annotate_error(&mut self, e: Error) {
        eprintln!("{}", &e);
        stdout().flush().unwrap();
        self.errors.push(e);
        self.enact_termination_policy();
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn clear_errors(&mut self) {
        self.errors.clear()
    }

    pub fn enact_termination_policy(&self) {
        if self.has_errors() && self.termination_policy == TerminationPolicy::STRICT {
            exit(1)
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum ErrorType {
    GENERICERROR,

    LEXER_UNEXPECTED_SYMBOL(char),
    LEXER_BAD_STR_FMT,

    PARSER_EXPECTED_LITERAL(TokenType),
    PARSER_EXPECTED_TOKEN(TokenType),
    PARSER_UNEXPECTED_TOKEN(TokenType),
    PARSER_NOTAVAR,

    EVAL_UNASSIGNED_VAR(String),
    EVAL_ITER_APPL_ON_NONITER(Value),
    EVAL_KEY_NOT_FOUND,
    EVAL_INVALID_EXPR,
    EVAL_INVALID_LITERAL,
    EVAL_INVALID_OP(TokenType, Vec<Value>),
    EVAL_NOT_BOOLEANABLE(Value),
    EVAL_ARGS_TO_NOT_APPLICABLE,
    EVAL_ARGS_TO_ITAPPLICABLE,
    EVAL_VAL_TO_NONIT_APPLICABLE,
    EVAL_INVALID_PUSH,
    EVAL_INVALID_RANGE,
    EVAL_UNEXPECTED_EXPRESSION,
    EVAL_UNEXPECTED_NUMBER_OF_PARAMS { passed: usize, expected: usize },
    EVAL_PROTECTED_VARIABLE(String),
}

#[derive(Debug)]
pub struct Error {
    etype: ErrorType,
    coord: Coord,
}

impl Error {
    pub fn on_coord(coord: &Coord, etype: ErrorType) -> Error {
        Error {
            etype,
            coord: coord.clone(),
        }
    }
    fn err_location(&self) -> String {
        format!("[{}:{}] ", self.coord.row, self.coord.column)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let msg = match &self.etype {
            ErrorType::LEXER_UNEXPECTED_SYMBOL(symbol) => format!("unexpected character: {}", symbol),
            ErrorType::LEXER_BAD_STR_FMT => String::from("string wasn't closed."),
            ErrorType::PARSER_EXPECTED_LITERAL(found) => format!("expected literal, found: {:?}", found),
            ErrorType::PARSER_EXPECTED_TOKEN(ttype) => format!("expected this token: {:?}", ttype),
            ErrorType::PARSER_UNEXPECTED_TOKEN(ttype) => format!("unexpected token: {:?}", ttype),
            ErrorType::PARSER_NOTAVAR => "when declaring an applicable, only literals are allowed in the list of arguments".to_string(),
            ErrorType::EVAL_UNASSIGNED_VAR(varname) => format!("uninitialized variable: {}", varname),
            ErrorType::EVAL_ITER_APPL_ON_NONITER(val) => format!("cannot use @@ to feed `{}`", val),
            ErrorType::EVAL_KEY_NOT_FOUND => "key not found".to_string(),
            ErrorType::EVAL_INVALID_EXPR => "could not parse this line".to_string(),
            ErrorType::EVAL_INVALID_LITERAL => "invalid literal".to_string(),
            ErrorType::EVAL_INVALID_OP(ttype, operands) =>
                format!("cannot apply `{:?}` to operands `{:?}`", ttype, operands),
            ErrorType::EVAL_NOT_BOOLEANABLE(val) => format!("cannot treat `{}` as a boolean value", val),
            ErrorType::EVAL_ARGS_TO_ITAPPLICABLE => "the value fed to an it-applicable cannot be caged between '|'".to_string(),
            ErrorType::EVAL_ARGS_TO_NOT_APPLICABLE => "the right side of the application is not an applicable".to_string(),
            ErrorType::EVAL_VAL_TO_NONIT_APPLICABLE => "the value fed to an explicit applicable must be caged between '|'".to_string(),
            ErrorType::EVAL_INVALID_PUSH => "the correct push syntax is `association << |key, value|`".to_string(),
            ErrorType::EVAL_INVALID_RANGE => "cannot construct a range with the given boundaries".to_string(),
            ErrorType::EVAL_UNEXPECTED_EXPRESSION => "cannot use underscore or argument expression standalone".to_string(),
            ErrorType::EVAL_UNEXPECTED_NUMBER_OF_PARAMS { passed, expected } => format!(
                "expected {} params but got {}. \
                if you're trying to call an applicable with no args, you need to pass _ as the only argument.",
                expected, passed
            ),
            ErrorType::EVAL_PROTECTED_VARIABLE(str) => format!("`{str}` is a standard library value, and cannot be reassigned."),
            ErrorType::GENERICERROR => "generic error".to_string()
        };
        f.write_str(&*(self.err_location() + &*msg).red())
    }
}
