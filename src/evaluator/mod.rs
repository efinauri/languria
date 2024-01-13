use std::ops::Deref;
use crate::environment::{Environment, print_eol, Value};
use crate::environment::Value::*;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::Expression;
use crate::parser::Expression::{LITERAL, VAR_RAW};

mod tests;

pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, es: &mut ErrorScribe, env: &mut Environment, subscoping: bool) -> Value {
    if subscoping {env.create_scope();}
    let mut result = NOTAVAL;
    for expr in exprs {
        let eval = eval_expr(&expr, env, es);
        match eval {
            NOTAVAL => { continue; }
            RETURNVAL(val) => {
                if subscoping {env.destroy_scope();}
                return val.deref().clone();
            }
            _ => { result = eval; }
        }
    }
    if subscoping {env.destroy_scope();}
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
        Expression::RETURN_EXPR { expr } => { RETURNVAL(Box::new(eval_expr(expr, env, scribe))) }
        Expression::APPLICATION { arg, body } => {
            let it = eval_expr(arg, env, scribe);
            eval_application(body, env, scribe, it, NOTAVAL)
        }
        Expression::BLOCK { exprs, applicable } => {
            if *applicable && !env.curr_scope().is_application { return LAMBDAVAL(exprs.to_owned()); }
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
                                                         ErrorType::UNASSIGNEDVAR { varname: varname.clone() }));
                    ERRVAL
                }
                Some(val) => match val {
                    LAMBDAVAL(exprs) => {
                        eval_expr(&Expression::BLOCK { exprs: exprs.clone(), applicable: true }, env, scribe)
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
                STRING(str) => STRINGVAL(str.to_owned()),
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
