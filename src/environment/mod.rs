use std::cmp::{max, min, Ordering};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Neg;

use crate::environment::Value::*;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::Expression;
use crate::parser::Expression::LITERAL;

pub struct Environment {
    scopes: Vec<Scope>,
    pub curr_line: usize,
    last_print_line: usize,
}

impl Environment {
    pub fn new() -> Environment { Environment { scopes: vec![Scope::new()], curr_line: 0, last_print_line: 0 } }
    pub fn create_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.push(scope);
    }
    pub fn destroy_scope(&mut self) { if self.scopes.len() > 1 { self.scopes.pop(); } }
    pub fn curr_scope(&mut self) -> &Scope { self.scopes.last().unwrap() }
    pub fn curr_scope_mut(&mut self) -> &mut Scope { self.scopes.iter_mut().last().unwrap() }

    pub fn read(&self, varname: &String) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(varname) { return Some(val); }
        }
        None
    }
    pub fn write(&mut self, varname: &String, varval: &Value, op: &Token, scribe: &mut ErrorScribe) -> Value {
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
    pub is_application: bool,
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
            return ERRVAL;
        }
        self.variables.insert(varname.clone(), val_to_write.clone());
        val_to_write.clone()
    }
}

#[derive(Clone)]
pub enum Value {
    INTEGERVAL(i32),
    FLOATVAL(f64),
    STRINGVAL(String),
    BOOLEANVAL(bool),
    LAMBDAVAL(Vec<Box<Expression>>),
    OPTIONVAL(Option<Box<Self>>),
    ASSOCIATIONVAL(HashMap<Box<Self>, Box<Self>>),
    RETURNVAL(Box<Self>),
    ERRVAL,
    NOTAVAL,
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            INTEGERVAL(int) => { int.hash(state); }
            FLOATVAL(flt) => { flt.to_bits().hash(state); }
            STRINGVAL(str) => { str.hash(state); }
            BOOLEANVAL(bool) => { bool.hash(state); }
            LAMBDAVAL(_lmb) => {}
            OPTIONVAL(opt) => { opt.hash(state); }
            ASSOCIATIONVAL(map) => { map.hasher(); }
            RETURNVAL(ret) => { ret.hash(state); }
            ERRVAL => {}
            NOTAVAL => {}
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => i == j,
            (FLOATVAL(i), FLOATVAL(j)) => i == j,
            (STRINGVAL(i), STRINGVAL(j)) => i == j,
            (BOOLEANVAL(i), BOOLEANVAL(j)) => i == j,
            (OPTIONVAL(i), OPTIONVAL(j)) => {
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
            (INTEGERVAL(i), INTEGERVAL(j)) => Some(i.cmp(j)),
            (FLOATVAL(i), FLOATVAL(j)) => {
                if i > j { return Some(Ordering::Greater); } else if i < j { return Some(Ordering::Less); }
                Some(Ordering::Equal)
            }
            (STRINGVAL(i), STRINGVAL(j)) => Some(i.cmp(j)),
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

pub fn print_eol(env: &mut Environment, line: &usize) -> Value {
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
            INTEGERVAL(int) => { f.write_str(&*int.to_string()) }
            FLOATVAL(flt) => { f.write_str(&*format!("{}{}", flt, if flt.fract() > 0.0 { "" } else { ".0" })) }
            STRINGVAL(str) => { f.write_str(str) }
            BOOLEANVAL(boo) => { f.write_str(&*boo.to_string()) }
            LAMBDAVAL(_) => { f.write_str("lambda") }
            ASSOCIATIONVAL(map) => {
                let mut str = map.iter()
                    .map(|(k, v)|format!("{}: {}, ", k, v))
                    .reduce(|str1, str2| str1 + &*str2)
                    .unwrap();
                str.pop();
                str.pop();
                f.write_str(&*format!("[{}]", str))
            }
            NOTAVAL => { f.write_str("no input.") }
            ERRVAL => { f.write_str("ERR") }
            RETURNVAL(val) => { f.write_str(&*val.to_string()) }
            OPTIONVAL(_val) => {f.write_str("TODO")}
        }
    }

    pub fn type_equals(&self, other: &Value) -> bool {
        match (self, other) {
            (INTEGERVAL(_), INTEGERVAL(_)) |
            (FLOATVAL(_), FLOATVAL(_)) |
            (STRINGVAL(_), STRINGVAL(_)) |
            (BOOLEANVAL(_), BOOLEANVAL(_)) |
            (ERRVAL, ERRVAL) |
            (NOTAVAL, NOTAVAL) => { true }
            (_, _) => false
        }
    }

    pub fn as_bool_val(&self) -> Value {
        return match self {
            BOOLEANVAL(bool) => BOOLEANVAL(*bool),
            INTEGERVAL(int) => BOOLEANVAL(*int > 0),
            FLOATVAL(flt) => BOOLEANVAL(flt.is_sign_positive()),
            _ => { ERRVAL }
        };
    }

    pub fn bang_it(&self) -> Value {
        match self {
            INTEGERVAL(_) | FLOATVAL(_) => self.as_bool_val().bang_it(),
            BOOLEANVAL(boo) => { BOOLEANVAL(!boo) }
            _ => { ERRVAL }
        }
    }

    pub fn minus_it(&self) -> Value {
        match self {
            INTEGERVAL(int) => { INTEGERVAL(-int) }
            FLOATVAL(flt) => { FLOATVAL(flt.neg()) }
            _ => { ERRVAL }
        }
    }

    pub fn print_it(&self, curr_line: usize, env: &mut Environment, tag: Option<&Box<Expression>>) {
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

    pub fn minus_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { INTEGERVAL(i - j) }
            (INTEGERVAL(i), FLOATVAL(J)) => { FLOATVAL(*i as f64 - J) }
            (FLOATVAL(I), INTEGERVAL(j)) => { FLOATVAL(I - *j as f64) }
            (FLOATVAL(I), FLOATVAL(J)) => { FLOATVAL(I - J) }
            (_, _) => ERRVAL
        }
    }

    pub fn plus_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { INTEGERVAL(i + j) }
            (INTEGERVAL(i), FLOATVAL(J)) => { FLOATVAL(*i as f64 + J) }
            (FLOATVAL(I), INTEGERVAL(j)) => { FLOATVAL(I + *j as f64) }
            (FLOATVAL(I), FLOATVAL(J)) => { FLOATVAL(I + J) }
            (STRINGVAL(s1), STRINGVAL(s2)) => { STRINGVAL(format!("{}{}", s1, s2)) }
            (_, _) => ERRVAL
        }
    }

    pub fn mul_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { INTEGERVAL(i * j) }
            (INTEGERVAL(i), FLOATVAL(J)) => { FLOATVAL(*i as f64 * J) }
            (FLOATVAL(I), INTEGERVAL(j)) => { FLOATVAL(I * *j as f64) }
            (FLOATVAL(I), FLOATVAL(J)) => { FLOATVAL(I * J) }
            // bool*num
            (INTEGERVAL(i), BOOLEANVAL(boo)) => { INTEGERVAL(if *boo { *i } else { 0 }) }
            (BOOLEANVAL(boo), INTEGERVAL(j)) => { INTEGERVAL(if *boo { *j } else { 0 }) }
            (FLOATVAL(I), BOOLEANVAL(boo)) => { FLOATVAL(if *boo { *I } else { 0.0 }) }
            (BOOLEANVAL(boo), FLOATVAL(J)) => { FLOATVAL(if *boo { *J } else { 0.0 }) }
            // strings
            (INTEGERVAL(i), STRINGVAL(s)) => { STRINGVAL(s.repeat(*i as usize)) }
            (STRINGVAL(s), INTEGERVAL(j)) => { STRINGVAL(s.repeat(*j as usize)) }
            (STRINGVAL(s1), STRINGVAL(s2)) => { STRINGVAL(format!("{}{}", s1, s2)) }
            (_, _) => ERRVAL
        }
    }

    pub fn div_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { if *j == 0 { ERRVAL } else { INTEGERVAL(*i / *j) } }
            (INTEGERVAL(i), FLOATVAL(J)) => { if *J == 0.0 { ERRVAL } else { FLOATVAL(*i as f64 / *J) } }
            (FLOATVAL(I), INTEGERVAL(j)) => { if *j == 0 { ERRVAL } else { FLOATVAL(*I / *j as f64) } }
            (FLOATVAL(I), FLOATVAL(J)) => { if *J == 0.0 { ERRVAL } else { FLOATVAL(*I / *J) } }
            (_, _) => ERRVAL
        }
    }

    fn min_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { if *j == 0 { ERRVAL } else { INTEGERVAL(min(*i, *j)) } }
            (_, _) => ERRVAL
        }
    }

    fn max_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { if *j == 0 { ERRVAL } else { INTEGERVAL(max(*i, *j)) } }
            (_, _) => ERRVAL
        }
    }

    pub fn cmp_them(&self, other: &Value, cmp: fn(&Value, &Value) -> bool) -> Value { BOOLEANVAL(cmp(self, other)) }
}