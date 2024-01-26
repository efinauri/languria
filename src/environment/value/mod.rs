use std::cmp::{max, min, Ordering};
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, Neg};

use crate::environment::Environment;
use crate::environment::Value::{ERRVAL, NOTAVAL};
use crate::environment::value::Value::*;
use crate::lexer::TokenType::IDENTIFIER;
use crate::parser::Expression;
use crate::parser::Expression::LITERAL;
use crate::user_io::Red;

#[derive(Clone)]
pub enum Value {
    INTEGERVAL(i64),
    FLOATVAL(f64),
    STRINGVAL(String),
    BOOLEANVAL(bool),
    LAMBDAVAL { params: Box<Expression>, body: Box<Expression> },
    OPTIONVAL(Option<Box<Self>>),
    ASSOCIATIONVAL(ValueMap),
    RETURNVAL(Box<Self>),
    LAZYVAL(Box<Expression>),
    ERRVAL,
    NOTAVAL,
    UNDERSCOREVAL,
}

impl Value {
    pub fn extract(&self) -> Value {
        match self {
            OPTIONVAL(Some(val)) => { val.deref().clone() }
            _ => { ERRVAL }
        }
    }
}

#[derive(Clone, Debug)]
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

    pub fn remove(&mut self, k: Value) { self.map.remove(&k); }

    pub fn iter(&self) -> Iter<'_, Box<Value>, Box<Value>> {
        self.map.iter()
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (STRINGVAL(i), STRINGVAL(j)) => i == j,
            (BOOLEANVAL(i), BOOLEANVAL(j)) => i == j,
            (INTEGERVAL(i), INTEGERVAL(j)) => i == j,
            (FLOATVAL(i), FLOATVAL(j)) => i == j,

            (INTEGERVAL(i), FLOATVAL(j)) | (FLOATVAL(j), INTEGERVAL(i))
            => { (*i as f64) == *j }

            (OPTIONVAL(i), OPTIONVAL(j)) => {
                match (i, j) {
                    (None, None) => { true }
                    (Some(i), Some(j)) => { i == j }
                    (_, _) => { false }
                }
            }
            _ => { false }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (INTEGERVAL(i), FLOATVAL(j)) => { Some((*i as f64).total_cmp(j)) }
            (FLOATVAL(j), INTEGERVAL(i)) => { Some(j.total_cmp(&(*i as f64))) }
            (INTEGERVAL(i), INTEGERVAL(j)) => Some(i.cmp(j)),
            (FLOATVAL(i), FLOATVAL(j)) => {
                if i > j { return Some(Ordering::Greater); } else if i < j { return Some(Ordering::Less); }
                Some(Ordering::Equal)
            }
            (OPTIONVAL(i), OPTIONVAL(j)) => {
                Some(i.cmp(j))
            }
            (BOOLEANVAL(i), BOOLEANVAL(j)) => Some(i.cmp(j)),
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

#[allow(non_snake_case)]
impl Value {
    fn display_and_debug(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LAZYVAL(_) => { f.write_str("(not yet evaluated)") }
            INTEGERVAL(int) => { f.write_str(&*int.to_string()) }
            FLOATVAL(flt) => { f.write_str(&*format!("{}{}", flt, if flt.fract() > 0.0 { "" } else { ".0" })) }
            STRINGVAL(str) => { f.write_str(str) }
            BOOLEANVAL(boo) => { f.write_str(&*boo.to_string()) }
            LAMBDAVAL { .. } => { f.write_str("applicable") }
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
            ERRVAL => { f.write_str(&*"ERR".to_string().red()) }
            RETURNVAL(val) => { f.write_str(&*val.to_string()) }
            OPTIONVAL(val) => {
                match val {
                    None => { f.write_str("?_") }
                    Some(v) => { f.write_str(&*format!("?{}", v)) }
                }
            }
            UNDERSCOREVAL => f.write_str("_")
        }
    }

    pub fn type_equals(&self, other: &Value) -> bool {
        match (self, other) {
            (INTEGERVAL(_), INTEGERVAL(_)) |
            (FLOATVAL(_), FLOATVAL(_)) |
            (STRINGVAL(_), STRINGVAL(_)) |
            (BOOLEANVAL(_), BOOLEANVAL(_)) |
            (ERRVAL, ERRVAL) |
            (LAMBDAVAL { .. }, LAMBDAVAL { .. }) |
            (UNDERSCOREVAL, UNDERSCOREVAL) |
            (NOTAVAL, NOTAVAL) => { true }
            (_, _) => false
        }
    }

    pub fn as_bool_val(&self) -> Value {
        return match self {
            RETURNVAL(val) => val.as_bool_val(),
            BOOLEANVAL(bool) => BOOLEANVAL(*bool),
            INTEGERVAL(int) => BOOLEANVAL(*int > 0),
            FLOATVAL(flt) => BOOLEANVAL(flt.is_sign_positive()),
            STRINGVAL(str) => BOOLEANVAL(str.len() > 0),
            OPTIONVAL(opt) => BOOLEANVAL(opt.is_some()),
            ASSOCIATIONVAL(map) => BOOLEANVAL(map.iter().len() > 0),
            _ => { ERRVAL }
        };
    }

    pub fn not_it(&self) -> Value {
        match self {
            INTEGERVAL(_) | FLOATVAL(_) => self.as_bool_val().not_it(),
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

    pub(crate) fn min_them(&self, other: &Value) -> Value {
        match (self, other) {
            (INTEGERVAL(i), INTEGERVAL(j)) => { if *j == 0 { ERRVAL } else { INTEGERVAL(min(*i, *j)) } }
            (_, _) => ERRVAL
        }
    }

    pub(crate) fn max_them(&self, other: &Value) -> Value {
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
