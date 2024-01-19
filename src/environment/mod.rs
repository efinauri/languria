use std::cmp::{max, min, Ordering};
use std::collections::{BTreeMap, HashMap};
use std::collections::btree_map::Iter;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, Neg};

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
    pub fn in_application(&self) -> bool { self.scopes.iter().any(|s| s.is_application) }
    pub fn new() -> Environment {
        Environment {
            scopes: vec![Scope::new(false)],
            curr_line: 0,
            last_print_line: 0,
        }
    }
    pub fn create_scope(&mut self, is_application: bool) {
        let scope = Scope::new(is_application);
        self.scopes.push(scope);
    }
    pub fn destroy_scope(&mut self) { if self.scopes.len() > 1 { self.scopes.pop(); } }
    pub fn curr_scope(&mut self) -> &Scope { self.scopes.last().unwrap() }

    pub fn read(&self, varname: &String, scribe: &mut ErrorScribe) -> &Value {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(varname) { return val; }
        }
        scribe.annotate_error(Error::on_line(self.curr_line,
                                             ErrorType::EVAL_UNASSIGNED_VAR(varname.clone())));
        &ERRVAL
    }
    pub fn write(&mut self, varname: &String, varval: &Value, op: &Token) -> Value {
        let limit = self.scopes.len() - 1;
        for (scope, i) in &mut self.scopes.iter_mut().zip(0..) {
            if scope.variables.contains_key(varname) || i == limit {
                return scope.write(varname, varval, op).clone();
            }
        }
        NOTAVAL
    }

    pub fn write_binding(&mut self, varname: &String, varval: &Value) -> Value {
        self.write(varname, varval, &Token::new(ASSIGN, self.curr_line))
    }
}


pub struct Scope {
    variables: HashMap<String, Value>,
    entry_point: String,
    is_application: bool,
}

impl Scope {
    pub fn new(is_application: bool) -> Scope {
        Scope {
            variables: Default::default(),
            entry_point: String::from("REPL"),
            is_application,
        }
    }

    fn read(&self, varname: &String) -> Option<&Value> { self.variables.get(varname) }

    fn write(&mut self, varname: &String, varval: &Value, op: &Token) -> Value {
        let old_val = &self.read(varname);
        let val_to_write = match old_val {
            None => { varval.clone() }
            Some(ov) => {
                match op.ttype {
                    ASSIGN => { varval.clone() }
                    MINASSIGN => { varval.min_them(ov) }
                    MAXASSIGN => { varval.max_them(ov) }
                    PLUSASSIGN => { varval.plus_them(ov) }
                    MINUSASSIGN => { ov.minus_them(&varval) }
                    MULASSIGN => { varval.mul_them(ov) }
                    DIVASSIGN => { ov.div_them(&varval) }
                    POWASSIGN => { ov.pow_them(&varval) }
                    MODULOASSIGN => { ov.modulo_them(&varval) }
                    _ => { ERRVAL }
                }
            }
        };
        if val_to_write.type_equals(&ERRVAL) { return ERRVAL; }
        self.variables.insert(varname.clone(), val_to_write.clone());
        val_to_write.clone()
    }
}

#[derive(Clone)]
pub enum Value {
    INTEGERVAL(i64),
    FLOATVAL(f64),
    STRINGVAL(String),
    BOOLEANVAL(bool),
    LAMBDAVAL(Box<Expression>),
    OPTIONVAL(Option<Box<Self>>),
    ASSOCIATIONVAL(ValueMap),
    RETURNVAL(Box<Self>),
    LAZYVAL(Box<Expression>),
    ERRVAL,
    NOTAVAL,
}

#[derive(Clone)]
pub struct ValueMap {
    map: BTreeMap<Box<Value>, Box<Value>>,
    pub default: Option<Box<Value>>,
}

impl ValueMap {
    pub fn new() -> ValueMap {
        ValueMap {
            map: Default::default(),
            default: None,
        }
    }

    pub fn get(&self, key: &Value) -> Option<Value> {
        if let Some(hit) = self.map.iter()
            .filter(|(k, _)| (*k).deref() == key)
            .map(|(_, v)| v)
            .next() {
            return Some(hit.deref().clone());
        }
        None
    }

    pub fn insert(&mut self, k: Value, v: Value) {
        self.map.insert(Box::new(k), Box::new(v));
    }

    pub fn iter(&self) -> Iter<'_, Box<Value>, Box<Value>> {
        self.map.iter()
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => i == j,
            (INTEGERVAL(i), FLOATVAL(j)) | (FLOATVAL(j), INTEGERVAL(i))
                => { (*i as f64) == *j},
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
            (INTEGERVAL(i), FLOATVAL(j)) | (FLOATVAL(j), INTEGERVAL(i))
            => { j.partial_cmp(&(*i as f64)) }
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

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            None => { Ordering::Less }
            Some(cmp) => { cmp }
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
            LAZYVAL(_) => { f.write_str("(not yet evaluated)") }
            INTEGERVAL(int) => { f.write_str(&*int.to_string()) }
            FLOATVAL(flt) => { f.write_str(&*format!("{}{}", flt, if flt.fract() > 0.0 { "" } else { ".0" })) }
            STRINGVAL(str) => { f.write_str(str) }
            BOOLEANVAL(boo) => { f.write_str(&*boo.to_string()) }
            LAMBDAVAL(_) => { f.write_str("applicable") }
            ASSOCIATIONVAL(map) => {
                let mut str = map.map.iter()
                    .map(|(k, v)| format!("{}: {}, ", k, v))
                    .reduce(|str1, str2| str1 + &*str2)
                    .unwrap_or(String::new());
                str.pop();
                str.pop();
                if let Some(val) = &map.default { str += &*format!(", _: {}", val); }
                f.write_str(&*format!("[{}]", str))
            }
            NOTAVAL => { f.write_str("no input.") }
            ERRVAL => { f.write_str("ERR") }
            RETURNVAL(val) => { f.write_str(&*val.to_string()) }
            OPTIONVAL(val) => {
                match val {
                    None => { f.write_str("<>") }
                    Some(v) => { f.write_str(&*format!("<{}>", v)) }
                }
            }
        }
    }

    pub fn type_equals(&self, other: &Value) -> bool {
        match (self, other) {
            (INTEGERVAL(_), INTEGERVAL(_)) |
            (FLOATVAL(_), FLOATVAL(_)) |
            (STRINGVAL(_), STRINGVAL(_)) |
            (BOOLEANVAL(_), BOOLEANVAL(_)) |
            (ERRVAL, ERRVAL) |
            (LAMBDAVAL(_), LAMBDAVAL(_)) |
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
                    LITERAL(value) =>
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

    pub fn pow_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { if *i + *j == 0 { ERRVAL } else { INTEGERVAL(i.pow(*j as u32)) } }
            (INTEGERVAL(i), FLOATVAL(J)) => { if *i as f64 + *J == 0.0 { ERRVAL } else { FLOATVAL((*i as f64).powf(*J)) } }
            (FLOATVAL(I), INTEGERVAL(j)) => { if *I + *j as f64 == 0.0 { ERRVAL } else { FLOATVAL(I.powi(*j as i32)) } }
            (FLOATVAL(I), FLOATVAL(J)) => { if *I + *J == 0.0 { ERRVAL } else { FLOATVAL(I.powf(*J)) } }
            (_, _) => ERRVAL
        }
    }


    pub fn modulo_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { if *j == 0 { ERRVAL } else { INTEGERVAL(*i % *j) } }
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

    pub fn cmp_them(&self, other: &Value, cmp: fn(&Value, &Value) -> bool) -> Value {
        match self.partial_cmp(other) {
            None => { ERRVAL }
            Some(_) => { BOOLEANVAL(cmp(self, other)) }
        }
    }
}