use std::fmt::{Display, Formatter};
use std::ops::Neg;

use crate::evaluator::Value::{BOOLEAN, ERR, FLOAT, INTEGER, NOTAVAL, STRING};
use crate::lexer::TokenType;
use crate::parser::Expression;

#[derive(Debug)]
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
            NOTAVAL => {f.write_str("no input.")}
            _ => { f.write_str("ERR") }
        }
    }
}

// fn bool_as_int(v: &Value) -> Value {
//     return match v {
//         BOOLEAN(boo) => INTEGER(if *boo { 1 } else { 0 }),
//         _ => { ERR }
//     };
// }

fn num_as_bool(v: &Value) -> Value {
    return match v {
        INTEGER(int) => BOOLEAN(*int > 0),
        FLOAT(flt) => BOOLEAN(flt.is_sign_positive()),
        _ => { ERR }
    };
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
}

pub fn evaluate_expression(expr: &Expression) -> Value {
    match expr {
        Expression::NOTANEXPR => { NOTAVAL }
        Expression::GROUPING { expr } => { evaluate_expression(expr) }

        Expression::LITERAL { value } => {
            match &value.ttype {
                TokenType::FALSE => BOOLEAN(false),
                TokenType::TRUE => BOOLEAN(true),
                TokenType::STRING(str) => STRING(str.to_owned()),
                TokenType::INTEGER(int) => INTEGER(*int),
                TokenType::FLOAT(flt) => FLOAT(*flt),
                _ => { ERR }
            }
        }

        Expression::UNARY { op, expr } => {
            let expr = evaluate_expression(expr);
            match op.ttype {
                TokenType::BANG => { expr.bang_it() }
                TokenType::MINUS => { expr.minus_it() }
                _ => { ERR }
            }
        }

        Expression::BINARY { lhs, op, rhs } => {
            let lhs = evaluate_expression(lhs);
            let rhs = evaluate_expression(rhs);

            match op.ttype {
                TokenType::MINUS => { lhs.minus_them(rhs) }
                TokenType::PLUS => { lhs.plus_them(rhs) }
                TokenType::MUL => { lhs.mul_them(rhs) }
                // TokenType::DIV => { lhs.div_them(rhs) }
                //         TokenType::GT => { lhs > rhs }
                //         TokenType::GTE => { lhs >= rhs }
                //         TokenType::LT => { lhs < rhs }
                //         TokenType::LTE => { lhs <= rhs }
                //         TokenType::EQ => { lhs == rhs }
                //         TokenType::UNEQ => { lhs != rhs }
                _ => { ERR }
            }
        }
        // _ => { ERR }
    }
}