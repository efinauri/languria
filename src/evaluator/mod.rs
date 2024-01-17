use std::collections::BTreeMap;
use std::ops::Deref;

use crate::environment::{Environment, print_eol, Value};
use crate::environment::Value::*;
use crate::errors::{Error, ErrorScribe};
use crate::errors::ErrorType::UNASSIGNEDVAR;
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::Expression;

mod tests;

pub fn is_expr_applicable(expr: &Expression, env: &Environment) -> bool {
    match expr {
        Expression::VAR_RAW(varname)
        => { env.read(varname).is_some_and(|val| val.type_equals(&LAMBDAVAL(Box::new(Expression::NOTANEXPR)))) }

        Expression::LITERAL(value) => { [IT, TI, IDX].contains(&value.ttype) }
        Expression::NOTANEXPR => { false }
        Expression::VAR_ASSIGN { .. } => { false }

        Expression::RETURN_EXPR(expr) |
        Expression::APPLICATION { arg: expr, .. } |
        Expression::UNARY { expr, .. } |
        Expression::GROUPING(expr)
        => { is_expr_applicable(expr, env) }

        Expression::BINARY { rhs, lhs, .. } |
        Expression::LOGIC { rhs, lhs, .. } |
        Expression::QUERY { source: rhs, field: lhs }
        => { is_expr_applicable(rhs, env) || is_expr_applicable(lhs, env) }
        Expression::BLOCK(exprs)
        => { exprs.iter().any(|expr| is_expr_applicable(expr, env)) }
        Expression::ASSOCIATION(pairs)
        => { pairs.iter().any(|(k, v)| is_expr_applicable(k, env) || is_expr_applicable(v, env)) }
    }
}

pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, es: &mut ErrorScribe, env: &mut Environment, subscoping: bool) -> Value {
    if subscoping { env.create_scope(false); }
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

fn eval_application(arg: &Box<Expression>,
                    op: Token,
                    body: &Box<Expression>,
                    env: &mut Environment,
                    es: &mut ErrorScribe) -> Value {
    env.create_scope(true);

    let ret = if op.type_equals(&AT) {
        let it = eval_expr(arg, env, es);
        env.write(&String::from("_it"), &it, &Token::new(INTO, 0), es);
        eval_expr(body, env, es)
    }
    else {
        let arg = eval_expr(arg, env, es);
        if let ASSOCIATIONVAL { map, .. } = arg {
            let mut ret = NOTAVAL;
            for ((it, ti), idx) in map.iter().zip(0..) {
                env.write(&String::from("_it"), &it, &Token::new(INTO, 0), es);
                env.write(&String::from("_ti"), &ti, &Token::new(INTO, 0), es);
                env.write(&String::from("_idx"), &INTEGERVAL(idx), &Token::new(INTO, 0), es);
                ret = eval_expr(body, env, es);
            }
        ret
        } else { NOTAVAL }
    };
    env.destroy_scope();
    return ret;
}

fn eval_expr(expr: &Expression, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
    match expr {
        Expression::VAR_RAW(varname) => {
            let read_val = env.read(varname);
            if read_val.is_none() {
                scribe.annotate_error(Error::on_line(env.curr_line,
                                                     UNASSIGNEDVAR { varname: varname.clone() }));
                return ERRVAL;
            }
            let read_val = read_val.unwrap().clone();
            return match read_val {
                LAMBDAVAL(ex) => { eval_expr(&ex, env, scribe) }
                _ => { read_val.clone() }
            };
        }
        _ => {}
    }

    if is_expr_applicable(expr, env) && !env.in_application() { return LAMBDAVAL(Box::new(expr.clone())); }

    match expr {
        Expression::VAR_RAW(_) => NOTAVAL, // unreachable because it's handled separately
        Expression::QUERY { source, field } => {
            let source = eval_expr(source, env, scribe);
            return match source {
                ASSOCIATIONVAL { map, default } => {
                    let field = eval_expr(field, env, scribe);
                    if let Some(val) = map.get(&field) {
                        return val.deref().clone();
                    } else if let Some(val) = default {
                        return val.deref().clone();
                    } else { NOTAVAL }
                }
                _ => NOTAVAL
            };
        }
        Expression::ASSOCIATION(pairs) => {
            let mut map = BTreeMap::new();
            let mut default = None;
            for (k, v) in pairs {
                if let Expression::LITERAL(tok) = k.deref() {
                    if tok.type_equals(&UNDERSCORE) {
                        default = Some(Box::new(eval_expr(v, env, scribe)));
                        continue;
                    }
                }
                let k = Box::new(eval_expr(k, env, scribe));
                let v = Box::new(eval_expr(v, env, scribe));
                map.insert(k, v);
            }
            ASSOCIATIONVAL { map, default }
        }
        Expression::RETURN_EXPR(expr) => { RETURNVAL(Box::new(eval_expr(expr, env, scribe))) }
        Expression::APPLICATION { arg, op, body } => {
            eval_application(arg, op.clone(), body, env, scribe)
        }
        Expression::BLOCK(exprs) => {
            evaluate_expressions(exprs, scribe, env, true)
        }
        Expression::NOTANEXPR => { NOTAVAL }
        Expression::GROUPING(expr) => { eval_expr(expr, env, scribe) }
        Expression::VAR_ASSIGN { varname, op, varval } => {
            let val = eval_expr(varval, env, scribe);
            env.write(varname, &val, op, scribe)
        }
        Expression::LITERAL(value) => {
            match &value.ttype {
                EOLPRINT => print_eol(env, &value.line),
                FALSE => BOOLEANVAL(false),
                TRUE => BOOLEANVAL(true),
                STRING(str) => STRINGVAL(fill_in_string_tokens(str, env, scribe)),
                INTEGER(int) => INTEGERVAL(*int),
                FLOAT(flt) => FLOATVAL(*flt),
                IT => { env.read(&"_it".to_string()).unwrap().clone() }
                TI => { env.read(&"_ti".to_string()).unwrap().clone() }
                IDX => { env.read(&"_idx".to_string()).unwrap().clone() }
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
