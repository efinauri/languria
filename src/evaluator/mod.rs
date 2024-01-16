use std::collections::HashMap;
use std::ops::Deref;

use crate::environment::{Environment, print_eol, Value};
use crate::environment::Value::*;
use crate::errors::{Error, ErrorScribe};
use crate::errors::ErrorType::UNASSIGNEDVAR;
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::Expression;
use crate::parser::Expression::{LITERAL, VAR_RAW};

mod tests;

pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, es: &mut ErrorScribe, env: &mut Environment, subscoping: bool) -> Value {
    if subscoping { env.create_scope(); }
    let mut result = NOTAVAL;
    for expr in exprs {
        let eval = eval_expr(&expr, env, es);
        match eval {
            NOTAVAL => { continue; }
            RETURNVAL(val) => {
                if subscoping { env.destroy_scope(); }
                return val.deref().clone();
            }
            _ => { result = eval; }
        }
    }
    if subscoping { env.destroy_scope(); }
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
        Expression::QUERY { source, field } => {
            let source = eval_expr(source, env, scribe);
            return match source {
                ASSOCIATIONVAL(map) => {
                    let field = eval_expr(field, env, scribe);
                    if let Some(val) = map.get(&field) { return val.deref().clone(); } else { NOTAVAL }
                }
                _ => NOTAVAL
            };
        }
        Expression::ASSOCIATION { pairs } => {
            let mut map = HashMap::new();
            for (k, v) in pairs {
                let k = Box::new(eval_expr(k, env, scribe));
                let v = Box::new(eval_expr(v, env, scribe));
                map.insert(k, v);
            }
            ASSOCIATIONVAL(map)
        }
        Expression::RETURN_EXPR { expr } => { RETURNVAL(Box::new(eval_expr(expr, env, scribe))) }
        Expression::APPLICATION { arg, body } => {
            let it = eval_expr(arg, env, scribe);
            eval_application(body, env, scribe, it, NOTAVAL)
        }
        Expression::BLOCK { exprs } => {
            evaluate_expressions(exprs, scribe, env, true)
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
                                                         UNASSIGNEDVAR { varname: varname.clone() }));
                    ERRVAL
                }
                Some(val) => match val {
                    LAMBDAVAL(exprs) => {
                        eval_expr(&Expression::BLOCK { exprs: exprs.clone() }, env, scribe)
                    }
                    _ => { val.clone() }
                }
            }
        }

        LITERAL { value } => {
            match &value.ttype {
                EOLPRINT => print_eol(env, &value.line),
                FALSE => BOOLEANVAL(false),
                TRUE => BOOLEANVAL(true),
                STRING(str) => STRINGVAL(fill_in_string_tokens(str, env, scribe)),
                INTEGER(int) => INTEGERVAL(*int),
                FLOAT(flt) => FLOATVAL(*flt),
                _ => { ERRVAL }
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
                _ => { ERRVAL }
            }
        }

        Expression::LOGIC { lhs, op, rhs } => {
            let elhs = eval_expr(lhs, env, scribe);

            match (&op.ttype, elhs.as_bool_val()) {
                (AND, BOOLEANVAL(bool)) => {
                    if !bool { return elhs; } else { eval_expr(rhs, env, scribe) }
                }
                (OR, BOOLEANVAL(bool)) => {
                    if bool { return elhs; } else { eval_expr(rhs, env, scribe) }
                }
                (XOR, BOOLEANVAL(bool)) => {
                    let ehrs = eval_expr(rhs, env, scribe).as_bool_val();
                    if bool { return ehrs.bang_it(); } else { ehrs }
                }
                _ => { NOTAVAL }
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
                _ => { ERRVAL }
            }
        }
    }
}

fn fill_in_string_tokens(str: &String, env: &mut Environment, es: &mut ErrorScribe) -> String {
    let mut result = String::new();
    let mut varname = String::new();
    for ch in str.chars() {
        match ch {
            '{' => { varname = "_".to_string(); }
            '}' => {
                varname.remove(0);
                if let Some(val) = env.read(&varname) {
                    result += &*val.to_string();
                    varname.clear();
                } else {
                    es.annotate_error(Error::on_line(41, UNASSIGNEDVAR { varname: varname.clone() }));
                    break;
                }
            }
            _ => {
                if varname.len() > 0 { varname.push(ch); } else { result.push(ch); }
            }
        }
    }
    result
}
