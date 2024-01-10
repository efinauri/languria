use std::cmp::{max, min};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::{Display, Formatter};
use std::ops::Neg;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::evaluator::Value::{BOOLEAN, ERR, FLOAT, INTEGER, NOTAVAL, STRING};
use crate::lexer::{Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::Expression;
use crate::parser::Expression::{LITERAL, VAR_RAW};

mod tests;

pub struct Scope {
    env: HashMap<String, Value>,
    curr_line: usize,
    last_print_line: usize,
    entry_point: String,
    child_scope: Option<Box<Self>>,
}

impl Scope {
    pub fn attach_child_scope(&mut self) -> &mut Scope {
        self.child_scope = Some(Box::new(Scope::new()));
        self.child_scope.as_mut().unwrap()
    }
    pub fn detach_child_scope(&mut self) {
        self.child_scope = None;
    }

    pub fn reset_print(&mut self) { self.last_print_line = 0; }

    pub fn register_entrypoint(&mut self, entry_point: &OsStr) {
        self.entry_point = String::from(entry_point.to_str().unwrap());
    }
    pub fn new() -> Scope {
        Scope {
            env: Default::default(),
            curr_line: 0,
            last_print_line: 0,
            entry_point: String::from("REPL"),
            child_scope: None,
        }
    }

    fn innermost_variable_scope_mut(&mut self, varname: &String, scribe: &mut ErrorScribe) -> &mut Scope {
        if self.child_scope.is_some() {
            return self.child_scope.as_mut().unwrap().innermost_variable_scope_mut(varname, scribe);
        }
        self
    }

    pub fn read(&self, varname: &String) -> Option<Value> {
        if let Some(val) = self.env.get(varname) { return Some(val.clone()); }
        if let Some(scope) = &self.child_scope { return scope.read(varname); }
        None
    }

    fn write_here(&mut self, varname: &String, varval: Value, op: &Token, scribe: &mut ErrorScribe) -> Value {
        let old_val = &self.read(varname);
        let val_to_write = match old_val {
            None => { varval }
            Some(ov) => {
                match op.ttype {
                    ASSIGN | INTO => { varval }
                    MINASSIGN => { varval.min_them(ov) }
                    MAXASSIGN => { varval.max_them(ov) }
                    PLUSASSIGN => { varval.plus_them(ov) }
                    MINUSASSIGN => { ov.minus_them(&varval) }
                    MULASSIGN => { varval.mul_them(ov) }
                    DIVASSIGN => { ov.div_them(&varval) }
                    _ => { NOTAVAL }
                }
            }
        };
        if !op.type_equals(&INTO) && old_val.as_ref().is_some_and(|val| !val.type_equals(&val_to_write)) {
            scribe.annotate_error(Error::on_line(self.curr_line, ErrorType::EXPECTEDTYPE));
            return ERR;
        }
        self.env.insert(varname.clone(), val_to_write.clone());
        val_to_write.clone()
    }

    fn write(&mut self, varname: &String, varval: Value, op: &Token, scribe: &mut ErrorScribe) -> Value {
        self.innermost_variable_scope_mut(varname, scribe)
            .write_here(varname, varval, op, scribe)
    }
}

pub fn evaluate_expressions(exprs: Vec<Expression>, x: &mut Scope, es: &mut ErrorScribe) -> Value {
    let mut ret = NOTAVAL;
    for expr in exprs {
        let eval = evaluate_expression(&expr, x, es);
        if eval != NOTAVAL { ret = eval; }
    }
    ret
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, PartialOrd)]
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
            FLOAT(flt) => { f.write_str(&*format!("{}{}", flt, if flt.fract()>0.0 {""} else {".0"})) }
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
    let start_new_line = scope.last_print_line != *line;
    scope.last_print_line = *line;
    let to_print = format!("[{}:{}]", scope.entry_point, line);
    if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
    NOTAVAL
}

#[allow(non_snake_case)]
impl Value {
    pub fn type_equals(&self, other: &Value) -> bool {
        match (self, other) {
            (INTEGER(_), INTEGER(_)) |
            (FLOAT(_), FLOAT(_)) |
            (STRING(_), STRING(_)) |
            (BOOLEAN(_), BOOLEAN(_)) |
            (ERR, ERR) |
            (NOTAVAL, NOTAVAL) => { true }
            (_, _) => false
        }
    }
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
                            IDENTIFIER(str) => {
                                format!("{}: ", str)
                            }
                            _ => String::new()
                        }
                    _ => String::new()
                }
            }
            _ => String::new()
        };
        let start_new_line = scope.last_print_line != curr_line;
        scope.last_print_line = curr_line;
        if start_new_line { print!("\n{}{}", tag, &self); } else { print!(" {}{}", tag, &self); }
        &self
    }

    fn minus_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { INTEGER(i - j) }
            (INTEGER(i), FLOAT(J)) => { FLOAT(*i as f64 - J) }
            (FLOAT(I), INTEGER(j)) => { FLOAT(I - *j as f64) }
            (FLOAT(I), FLOAT(J)) => { FLOAT(I - J) }
            (_, _) => ERR
        }
    }

    fn plus_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { INTEGER(i + j) }
            (INTEGER(i), FLOAT(J)) => { FLOAT(*i as f64 + J) }
            (FLOAT(I), INTEGER(j)) => { FLOAT(I + *j as f64) }
            (FLOAT(I), FLOAT(J)) => { FLOAT(I + J) }
            (STRING(s1), STRING(s2)) => { STRING(format!("{}{}", s1, s2)) }
            (_, _) => ERR
        }
    }

    fn mul_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { INTEGER(i * j) }
            (INTEGER(i), FLOAT(J)) => { FLOAT(*i as f64 * J) }
            (FLOAT(I), INTEGER(j)) => { FLOAT(I * *j as f64) }
            (FLOAT(I), FLOAT(J)) => { FLOAT(I * J) }
            // bool*num
            (INTEGER(i), BOOLEAN(boo)) => { INTEGER(if *boo { *i } else { 0 }) }
            (BOOLEAN(boo), INTEGER(j)) => { INTEGER(if *boo { *j } else { 0 }) }
            (FLOAT(I), BOOLEAN(boo)) => { FLOAT(if *boo { *I } else { 0.0 }) }
            (BOOLEAN(boo), FLOAT(J)) => { FLOAT(if *boo { *J } else { 0.0 }) }
            // strings
            (INTEGER(i), STRING(s)) => { STRING(s.repeat(*i as usize)) }
            (STRING(s), INTEGER(j)) => { STRING(s.repeat(*j as usize)) }
            (STRING(s1), STRING(s2)) => { STRING(format!("{}{}", s1, s2)) }
            (_, _) => ERR
        }
    }

    fn div_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { if *j == 0 { ERR } else { INTEGER(*i / *j) } }
            (INTEGER(i), FLOAT(J)) => { if *J == 0.0 { ERR } else { FLOAT(*i as f64 / *J) } }
            (FLOAT(I), INTEGER(j)) => { if *j == 0 { ERR } else { FLOAT(*I / *j as f64) } }
            (FLOAT(I), FLOAT(J)) => { if *J == 0.0 { ERR } else { FLOAT(*I / *J) } }
            (_, _) => ERR
        }
    }

    fn min_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { if *j == 0 { ERR } else { INTEGER(min(*i, *j)) } }
            (_, _) => ERR
        }
    }

    fn max_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => { if *j == 0 { ERR } else { INTEGER(max(*i, *j)) } }
            (_, _) => ERR
        }
    }

    fn cmp_them(&self, other: &Value, cmp: fn(&Value, &Value) -> bool) -> Value { BOOLEAN(cmp(self, other)) }
}


fn evaluate_expression(expr: &Expression, scope: &mut Scope, scribe: &mut ErrorScribe) -> Value {
    match expr {
        Expression::BLOCK { exprs} => {
            scope.attach_child_scope();
            let mut last_val = NOTAVAL;
            for ex in exprs {
                last_val = evaluate_expression(ex, scope, scribe);
            }
            scope.detach_child_scope();
            last_val
        }
        Expression::NOTANEXPR => { NOTAVAL }
        Expression::GROUPING { expr } => { evaluate_expression(expr, scope, scribe) }
        Expression::VAR_ASSIGN { varname, op, varval } => {
            let val = evaluate_expression(varval, scope, scribe);
            scope.write(varname, val, op, scribe).clone()
        }
        VAR_RAW { varname } => {
            match scope.read(varname) {
                None => {
                    scribe.annotate_error(Error::on_line(scope.curr_line,
                                                         ErrorType::UNASSIGNEDVAR { varname: varname.clone() }));
                    ERR
                }
                Some(val) => { val }
            }
        }

        LITERAL { value } => {
            match &value.ttype {
                EOLPRINT => print_eol(scope, &value.line),
                FALSE => BOOLEAN(false),
                TRUE => BOOLEAN(true),
                TokenType::STRING(str) => STRING(str.to_owned()),
                TokenType::INTEGER(int) => INTEGER(*int),
                TokenType::FLOAT(flt) => FLOAT(*flt),
                _ => { ERR }
            }
        }

        Expression::UNARY { op, expr } => {
            let expr = evaluate_expression(expr, scope, scribe);
            match op.ttype {
                BANG => { expr.bang_it() }
                MINUS => { expr.minus_it() }
                DOLLAR => {
                    expr.print_it(op.line, scope, None).clone()
                }
                _ => { ERR }
            }
        }

        Expression::BINARY { lhs, op, rhs } => {
            let elhs = evaluate_expression(lhs, scope, scribe);
            let erhs = evaluate_expression(rhs, scope, scribe);

            match op.ttype {
                DOLLAR => { elhs.print_it(op.line, scope, Some(rhs)).clone() }
                MINUS => { elhs.minus_them(&erhs) }
                PLUS => { elhs.plus_them(&erhs) }
                MUL => { elhs.mul_them(&erhs) }
                DIV => { elhs.div_them(&erhs) }
                GT => { elhs.cmp_them(&erhs, |a, b| a > b) }
                GTE => { elhs.cmp_them(&erhs, |a, b| a >= b) }
                LT => { elhs.cmp_them(&erhs, |a, b| a < b) }
                LTE => { elhs.cmp_them(&erhs, |a, b| a <= b) }
                EQ => { elhs.cmp_them(&erhs, |a, b| a == b) }
                UNEQ => { elhs.cmp_them(&erhs, |a, b| a != b) }
                _ => { ERR }
            }
        }
    }
}