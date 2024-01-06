use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::{Display, Formatter};
use std::ops::Neg;

use crate::evaluator::Value::{BOOLEAN, ERR, FLOAT, INTEGER, NOTAVAL, STRING};
use crate::lexer::TokenType;
use crate::parser::Expression;
use crate::parser::Expression::{LITERAL, VAR_RAW};

pub struct Scope {
    env: HashMap<String, Value>,
    print_line: usize,
    entry_point: String,
}

impl Scope {
    pub fn reset_print(&mut self) { self.print_line = 0; }
}

impl Scope {
    pub fn register_entrypoint(&mut self, entry_point: &OsStr) {
        self.entry_point = String::from(entry_point.to_str().unwrap());
    }
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            env: Default::default(),
            print_line: 0,
            entry_point: String::from("REPL"),
        }
    }

    fn write(&mut self, varname: &String, varval: Value) -> Value {
        self.env.insert(varname.clone(), varval.clone());
        varval.clone()
    }

    fn read(&self, varname: &String) -> Value {
        if self.env.contains_key(varname) {
            self.env.get(varname).unwrap().clone()
        } else { ERR }
    }
}

pub fn evaluate_expressions(exprs: Vec<Expression>, x: &mut Scope) -> Value {
    let mut ret = ERR;
    for expr in exprs {
        let eval = evaluate_expression(&expr, x);
        if eval != NOTAVAL { ret = eval; }
    }
    ret
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum Value {
    INTEGER(i32),
    FLOAT(f64),
    STRING(String),
    BOOLEAN(bool),
    ERR,
    NOTAVAL,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            INTEGER(int) => { f.write_str(&*int.to_string()) }
            FLOAT(flt) => { f.write_str(&*flt.to_string()) }
            STRING(str) => { f.write_str(str) }
            BOOLEAN(boo) => { f.write_str(&*boo.to_string()) }
            NOTAVAL => { f.write_str("no input.") }
            _ => { f.write_str("ERR") }
        }
    }
}

fn num_as_bool(v: &Value) -> Value {
    return match v {
        INTEGER(int) => BOOLEAN(*int > 0),
        FLOAT(flt) => BOOLEAN(flt.is_sign_positive()),
        _ => { ERR }
    };
}

fn print_eol(scope: &mut Scope, line: &usize) -> Value {
    let start_new_line = scope.print_line != *line;
    scope.print_line = *line;
    let to_print = format!("[{}:{}]", scope.entry_point, line);
    if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
    NOTAVAL
}

#[allow(non_snake_case)]
impl Value {
    fn bang_it(&self) -> Value {
        match self {
            INTEGER(_) | FLOAT(_) => num_as_bool(&self).bang_it(),
            BOOLEAN(boo) => { BOOLEAN(!boo) }
            _ => { ERR }
        }
    }

    fn minus_it(&self) -> Value {
        match self {
            INTEGER(int) => { INTEGER(-int) }
            FLOAT(flt) => { FLOAT(flt.neg()) }
            _ => { ERR }
        }
    }

    fn print_it(&self, curr_line: usize, scope: &mut Scope, tag: Option<&Box<Expression>>) -> &Value {
        let tag = match tag {
            Some(boxx)
            => {
                match (**boxx).clone() {
                    LITERAL { value } =>
                        match value.ttype {
                            TokenType::IDENTIFIER(str) => {
                                format!("{}: ", str)
                            }
                            _ => String::new()
                        }
                    _ => String::new()
                }
            }
            _ => String::new()
        };
        let start_new_line = scope.print_line != curr_line;
        scope.print_line = curr_line;
        if start_new_line { print!("\n{}{}", tag, &self); } else { print!(" {}{}", tag, &self); }
        &self
    }

    fn minus_them(&self, other: Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { INTEGER(i - j) }
            (INTEGER(i), FLOAT(J)) => { FLOAT(*i as f64 - J) }
            (FLOAT(I), INTEGER(j)) => { FLOAT(I - j as f64) }
            (FLOAT(I), FLOAT(J)) => { FLOAT(I - J) }
            (_, _) => ERR
        }
    }

    fn plus_them(&self, other: Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { INTEGER(i + j) }
            (INTEGER(i), FLOAT(J)) => { FLOAT(*i as f64 + J) }
            (FLOAT(I), INTEGER(j)) => { FLOAT(I + j as f64) }
            (FLOAT(I), FLOAT(J)) => { FLOAT(I + J) }
            (STRING(s1), STRING(s2)) => { STRING(format!("{}{}", s1, s2)) }
            (_, _) => ERR
        }
    }

    fn mul_them(&self, other: Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { INTEGER(i * j) }
            (INTEGER(i), FLOAT(J)) => { FLOAT(*i as f64 * J) }
            (FLOAT(I), INTEGER(j)) => { FLOAT(I * j as f64) }
            (FLOAT(I), FLOAT(J)) => { FLOAT(I * J) }
            // bool*num
            (INTEGER(i), BOOLEAN(boo)) => { INTEGER(if boo { *i } else { 0 }) }
            (BOOLEAN(boo), INTEGER(j)) => { INTEGER(if *boo { j } else { 0 }) }
            (FLOAT(I), BOOLEAN(boo)) => { FLOAT(if boo { *I } else { 0.0 }) }
            (BOOLEAN(boo), FLOAT(J)) => { FLOAT(if *boo { J } else { 0.0 }) }
            // strings
            (INTEGER(i), STRING(s)) => { STRING(s.repeat(*i as usize)) }
            (STRING(s), INTEGER(j)) => { STRING(s.repeat(j as usize)) }
            (STRING(s1), STRING(s2)) => { STRING(format!("{}{}", s1, s2)) }
            (_, _) => ERR
        }
    }

    fn div_them(&self, other: Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { if j == 0 { ERR } else { INTEGER(i / j) } }
            (INTEGER(i), FLOAT(J)) => { if J == 0.0 { ERR } else { FLOAT(*i as f64 / J) } }
            (FLOAT(I), INTEGER(j)) => { if j == 0 { ERR } else { FLOAT(I / j as f64) } }
            (FLOAT(I), FLOAT(J)) => { if J == 0.0 { ERR } else { FLOAT(I / J) } }
            (_, _) => ERR
        }
    }
}

fn evaluate_expression(expr: &Expression, scope: &mut Scope) -> Value {
    match expr {
        Expression::NOTANEXPR => { NOTAVAL }
        Expression::GROUPING { expr } => { evaluate_expression(expr, scope) }
        Expression::VAR_ASSIGN { varname, varval } => {
            let val = evaluate_expression(varval, scope);
            scope.write(varname, val).clone()
        }
        VAR_RAW { varname } => {
            scope.read(varname).clone()
        }

        LITERAL { value } => {
            match &value.ttype {
                TokenType::EOLPRINT => print_eol(scope, &value.line),
                TokenType::FALSE => BOOLEAN(false),
                TokenType::TRUE => BOOLEAN(true),
                TokenType::STRING(str) => STRING(str.to_owned()),
                TokenType::INTEGER(int) => INTEGER(*int),
                TokenType::FLOAT(flt) => FLOAT(*flt),
                _ => { ERR }
            }
        }

        Expression::UNARY { op, expr } => {
            let expr = evaluate_expression(expr, scope);
            match op.ttype {
                TokenType::BANG => { expr.bang_it() }
                TokenType::MINUS => { expr.minus_it() }
                TokenType::DOLLAR => {
                    expr.print_it(op.line, scope, None).clone()
                }
                _ => { ERR }
            }
        }

        Expression::BINARY { lhs, op, rhs } => {
            let elhs = evaluate_expression(lhs, scope);
            let erhs = evaluate_expression(rhs, scope);

            match op.ttype {
                TokenType::DOLLAR => { elhs.print_it(op.line, scope, Some(rhs)).clone() }
                TokenType::MINUS => { elhs.minus_them(erhs) }
                TokenType::PLUS => { elhs.plus_them(erhs) }
                TokenType::MUL => { elhs.mul_them(erhs) }
                TokenType::DIV => { elhs.div_them(erhs) }
                // TokenType::GT => { lhs > rhs }
                // TokenType::GTE => { lhs >= rhs }
                // TokenType::LT => { lhs < rhs }
                // TokenType::LTE => { lhs <= rhs }
                // TokenType::EQ => { lhs == rhs }
                // TokenType::UNEQ => { lhs != rhs }
                _ => { ERR }
            }
        }
        // _ => { ERR }
    }
}