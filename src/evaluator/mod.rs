use std::cmp::{max, min, Ordering};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, Neg};

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::evaluator::Value::{BOOLEAN, ERR, FLOAT, INTEGER, LAMBDA, NOTAVAL, OPTION, RETURNVAL, STRING};
use crate::lexer::{Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::Expression;
use crate::parser::Expression::{LITERAL, VAR_RAW};

mod tests;

pub struct Environment {
    scopes: Vec<Scope>,
    curr_line: usize,
    last_print_line: usize,
}

impl Environment {
    pub fn new() -> Environment { Environment { scopes: vec![], curr_line: 0, last_print_line: 0 } }
    pub fn create_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.push(scope);
    }
    pub fn destroy_scope(&mut self) { self.scopes.pop(); }
    pub fn curr_scope(&mut self) -> &Scope { self.scopes.last().unwrap() }
    pub fn curr_scope_mut(&mut self) -> &mut Scope { self.scopes.iter_mut().last().unwrap() }

    pub fn read(&self, varname: &String) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(varname) { return Some(val); }
        }
        None
    }
    fn write(&mut self, varname: &String, varval: &Value, op: &Token, scribe: &mut ErrorScribe) -> Value {
        let limit = self.scopes.len() - 1;
        for (scope, i) in &mut self.scopes.iter_mut().zip(0..) {
            if scope.variables.contains_key(varname) || i == limit {
                return scope.write(varname, varval, op, scribe).clone();
            }
        }
        NOTAVAL
    }
}


pub struct Scope {
    variables: HashMap<String, Value>,
    entry_point: String,
    is_application: bool,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: Default::default(),
            entry_point: String::from("REPL"),
            is_application: false,
        }
    }

    fn read(&self, varname: &String) -> Option<&Value> { self.variables.get(varname) }

    fn write(&mut self, varname: &String, varval: &Value, op: &Token, scribe: &mut ErrorScribe) -> Value {
        let old_val = &self.read(varname);
        let val_to_write = match old_val {
            None => { varval.clone() }
            Some(ov) => {
                match op.ttype {
                    ASSIGN | INTO => { varval.clone() }
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
            scribe.annotate_error(Error::on_line(42, ErrorType::EXPECTEDTYPE));
            return ERR;
        }
        self.variables.insert(varname.clone(), val_to_write.clone());
        val_to_write.clone()
    }
}

#[derive(Clone)]
pub enum Value {
    INTEGER(i32),
    FLOAT(f64),
    STRING(String),
    BOOLEAN(bool),
    LAMBDA(Vec<Box<Expression>>),
    OPTION(Option<Box<Self>>),
    ASSOCIATION(HashMap<Box<Vec<Self>>, Box<Vec<Self>>>),
    RETURNVAL(Box<Self>),
    ERR,
    NOTAVAL,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => i == j,
            (FLOAT(i), FLOAT(j)) => i == j,
            (STRING(i), STRING(j)) => i == j,
            (BOOLEAN(i), BOOLEAN(j)) => i == j,
            (OPTION(i), OPTION(j)) => {
                if let Some(i) = i {
                    if let Some(j) = j {
                        return i.eq(j);
                    } else { false }
                } else { false }
            }
            (NOTAVAL, NOTAVAL) => true,
            _ => false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (INTEGER(i), INTEGER(j)) => Some(i.cmp(j)),
            (FLOAT(i), FLOAT(j)) => {
                if i > j { return Some(Ordering::Greater); } else if i < j { return Some(Ordering::Less); }
                Some(Ordering::Equal)
            }
            (STRING(i), STRING(j)) => Some(i.cmp(j)),
            _ => None
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.display_and_debug(f)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.display_and_debug(f)
    }
}

fn num_as_bool(v: &Value) -> Value {
    return match v {
        INTEGER(int) => BOOLEAN(*int > 0),
        FLOAT(flt) => BOOLEAN(flt.is_sign_positive()),
        _ => { ERR }
    };
}

fn print_eol(env: &mut Environment, line: &usize) -> Value {
    let start_new_line = env.last_print_line != *line;
    env.last_print_line = *line;
    let to_print = format!("[{}:{}]", env.curr_scope().entry_point, line);
    if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
    NOTAVAL
}

#[allow(non_snake_case)]
impl Value {
    fn display_and_debug(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            INTEGER(int) => { f.write_str(&*int.to_string()) }
            FLOAT(flt) => { f.write_str(&*format!("{}{}", flt, if flt.fract() > 0.0 { "" } else { ".0" })) }
            STRING(str) => { f.write_str(str) }
            BOOLEAN(boo) => { f.write_str(&*boo.to_string()) }
            LAMBDA(_) => { f.write_str("lambda") }
            NOTAVAL => { f.write_str("no input.") }
            _ => { f.write_str("ERR") }
        }
    }

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

    fn print_it(&self, curr_line: usize, env: &mut Environment, tag: Option<&Box<Expression>>) {
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
        let start_new_line = env.last_print_line != curr_line;
        env.last_print_line = curr_line;
        if start_new_line { print!("\n{}{}", tag, &self); } else { print!(" {}{}", tag, &self); }
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


pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, es: &mut ErrorScribe, env: &mut Environment) -> Value {
    env.create_scope();
    let mut result = NOTAVAL;
    for expr in exprs {
        let eval = eval_expr(&expr, env, es);
        match eval {
            NOTAVAL => { continue; }
            RETURNVAL(val) => {
                env.destroy_scope();
                return val.deref().clone();
            }
            _ => { result = eval; }
        }
    }
    env.destroy_scope();
    result
}

fn eval_application(body: &Box<Expression>, env: &mut Environment, es: &mut ErrorScribe, it: Value, ti: Value) -> Value {
    env.create_scope();
    env.curr_scope_mut().is_application = true;
    env.write(&String::from("_it"), &it, &Token::new(INTO, 0), es);
    env.write(&String::from("_ti"), &ti, &Token::new(INTO, 0), es);
    let ret = eval_expr(body, env, es);
    env.destroy_scope();
    ret
}

fn eval_expr(expr: &Expression, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
    match expr {
        Expression::RETURN_EXPR { expr } => {
            RETURNVAL(Box::new(eval_expr(expr, env, scribe)))
        }
        Expression::APPLICATION { arg, body } => {
            let it = eval_expr(arg, env, scribe);
            eval_application(body, env, scribe, it, NOTAVAL)
        }
        Expression::BLOCK { exprs, applicable } => {
            if *applicable && !env.curr_scope().is_application { return LAMBDA(exprs.to_owned()); }
            evaluate_expressions(exprs, scribe, env)
        }
        Expression::NOTANEXPR => { NOTAVAL }
        Expression::GROUPING { expr } => { eval_expr(expr, env, scribe) }
        Expression::VAR_ASSIGN { varname, op, varval } => {
            let val = eval_expr(varval, env, scribe);
            env.write(varname, &val, op, scribe)
        }
        VAR_RAW { varname } => {
            match env.read(varname) {
                None => {
                    scribe.annotate_error(Error::on_line(env.curr_line,
                                                         ErrorType::UNASSIGNEDVAR { varname: varname.clone() }));
                    ERR
                }
                Some(val) => match val {
                    LAMBDA(exprs) => {
                        eval_expr(&Expression::BLOCK { exprs: exprs.clone(), applicable: true }, env, scribe)
                    }
                    _ => { val.clone() }
                }
            }
        }

        LITERAL { value } => {
            match &value.ttype {
                EOLPRINT => print_eol(env, &value.line),
                FALSE => BOOLEAN(false),
                TRUE => BOOLEAN(true),
                TokenType::STRING(str) => STRING(str.to_owned()),
                TokenType::INTEGER(int) => INTEGER(*int),
                TokenType::FLOAT(flt) => FLOAT(*flt),
                _ => { ERR }
            }
        }

        Expression::UNARY { op, expr } => {
            let expr = eval_expr(expr, env, scribe);
            match op.ttype {
                BANG => { expr.bang_it() }
                MINUS => { expr.minus_it() }
                DOLLAR => {
                    expr.print_it(op.line, env, None);
                    expr
                }
                _ => { ERR }
            }
        }

        Expression::BINARY { lhs, op, rhs } => {
            let elhs = eval_expr(lhs, env, scribe);
            let erhs = eval_expr(rhs, env, scribe);

            match op.ttype {
                DOLLAR => {
                    elhs.print_it(op.line, env, Some(rhs));
                    elhs
                }
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

