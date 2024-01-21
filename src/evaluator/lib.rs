use std::ops::Deref;

use crate::environment::{Environment, Value, ValueMap};
use crate::environment::Value::{ASSOCIATIONVAL, ERRVAL, INTEGERVAL, LAMBDAVAL, LAZYVAL, NOTAVAL, OPTIONVAL, STRINGVAL};
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::evaluator::eval_expr;
use crate::lexer::Token;
use crate::lexer::TokenType::{ASSIGN, AT, PULL, PUSH};
use crate::parser::Expression;
use crate::parser::Expression::UNDERSCORE_EXPR;


pub fn eval_association(
    pairs: &Vec<(Box<Expression>, Box<Expression>)>,
    lazy: bool,
    env: &mut Environment,
    scribe: &mut ErrorScribe,
) -> Value {
    let mut map = ValueMap::new();
    // all code branching needs to be lazily evaluated, even the ones that aren't applicables
    // (because of side effects)
    for (k, v) in pairs {
        let v = if lazy { LAZYVAL(v.clone()) } else { eval_expr(v, env, scribe) };
        if k.type_equals(&UNDERSCORE_EXPR) {
            map.default = Some(Box::new(v));
            continue;
        }
        map.insert(eval_expr(k, env, scribe), v);
    }
    ASSOCIATIONVAL(map)
}
pub fn eval_pull(field: Value, source: Value, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
    return match source {
        ASSOCIATIONVAL(map) => {
            let mut query_expr = None;
            let mut query_val = None;
            if let Some(val) = map.get(&field) {
                if let LAZYVAL(ex) = val { query_expr = Some(ex); } else { query_val = Some(val); };
            } else if let Some(val) = map.default {
                if let LAZYVAL(ex) = val.deref() { query_expr = Some(ex.clone()); } else { query_val = Some(*val); };
            }
            return match (query_expr, query_val) {
                (Some(ex), None) => { OPTIONVAL(Some(Box::new(eval_expr(&ex, env, scribe)))) }
                (None, Some(val)) => { OPTIONVAL(Some(Box::new(val.clone()))) }
                (None, None) => { OPTIONVAL(None) }
                (_, _) => { unreachable!() }
            };
        }
        _ => {
            scribe.annotate_error(Error::on_line(env.curr_line,
                                                 ErrorType::EVAL_INVALID_OP(PULL, vec![source])));
            ERRVAL
        }
    };
}

pub fn eval_push(
    obj: &Expression, args: &Expression, env: &mut Environment, scribe: &mut ErrorScribe,
) -> Value {
    // the caller performs desugaring to ensure that obj is not a variable

    let mut obj = eval_expr(obj, env, scribe);
    let else_branch = vec![];
    let exprs = if let Expression::ARGS(exprs) = args { exprs } else { &else_branch };
    if exprs.len() != 2 {
        scribe.annotate_error(Error::on_line(env.curr_line,
                                             ErrorType::EVAL_INVALID_PUSH));
        return ERRVAL;
    }
    return match obj {
        ASSOCIATIONVAL(ref mut map) => {
            match (exprs.get(0).unwrap().deref(), exprs.get(1).unwrap().deref()) {
                (UNDERSCORE_EXPR, UNDERSCORE_EXPR) => { map.default = None; }
                (UNDERSCORE_EXPR, val) => { map.default = Some(Box::new(eval_expr(val, env, scribe))); }
                (key, UNDERSCORE_EXPR) => { map.remove(eval_expr(key, env, scribe)); }
                (key, val) => {
                    map.insert(
                        eval_expr(key, env, scribe),
                        eval_expr(val, env, scribe),
                    );
                }
            };
            obj
        }
        _ => {
            scribe.annotate_error(Error::on_line(env.curr_line,
                                                 ErrorType::EVAL_INVALID_OP(PUSH, vec![obj])));
            return ERRVAL;
        }
    };
}

pub fn eval_application(arg: &Box<Expression>,
                        op: Token,
                        body: &Box<Expression>,
                        env: &mut Environment,
                        scribe: &mut ErrorScribe) -> Value {
    env.create_scope(true);

    let ret = if let Expression::ARGS(args) = arg.deref() {
        eval_parametrized_application(args, body, env, scribe)
    } else if op.type_equals(&AT) {
        // eval anonymous single application: 3 @ it + 1
        let it = eval_expr(arg, env, scribe);
        env.write_binding(&String::from("it"), &it);
        eval_expr(body, env, scribe)
    } else {
        eval_iterated_application(arg, body, env, scribe)
    };
    env.destroy_scope();
    return ret;
}

fn eval_parametrized_application(args: &Vec<Box<Expression>>, body: &Box<Expression>, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
    // | args | @ variable containing applicable, where
    // applicable = | params | expr

    // must be applied to var raw (func name)
    let varval = if let Expression::VAR_RAW(name) = body.deref() { env.read(name, scribe) } else {
        scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_ARGS_TO_NOT_APPLICABLE));
        return ERRVAL;
    };
    // // var must contain lambda
    let lambda = if let LAMBDAVAL(ex) = varval { ex.deref().clone() } else {
        scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_ARGS_TO_NOT_APPLICABLE));
        return ERRVAL;
    };
    // lambda must be wrapping an applicable
    let applicable = if let Expression::APPLICABLE {
        arg, body
    } = lambda { (arg, body) } else {
        scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_ARGS_TO_ITAPPLICABLE));
        return ERRVAL;
    };
    // and the applicable must have params as the arg
    let params = if let Expression::ARGS(params) = *applicable.0 { params } else {
        scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::GENERICERROR));
        return ERRVAL;
    };
    for (arg, param) in args.iter().zip(params) {
        if let Expression::VAR_RAW(name) = param.deref() {
            let val = eval_expr(arg, env, scribe);

            env.write(
                &name,
                &val,
                &Token::new(ASSIGN, 0),
            );
        }
    }
    eval_expr(applicable.1.deref(), env, scribe)
}

fn eval_iterated_application(
    arg: &Box<Expression>,
    body: &Box<Expression>,
    env: &mut Environment,
    scribe: &mut ErrorScribe,
) -> Value {
    let arg = eval_expr(arg, env, scribe);
    let mut ret = NOTAVAL;
    if let ASSOCIATIONVAL(map) = arg {
        for ((it, ti), idx) in map.iter().zip(0..) {
            let unlazy_ti;
            if let LAZYVAL(ex) = ti.deref() { unlazy_ti = eval_expr(ex, env, scribe); } else { unlazy_ti = ti.deref().clone() }
            env.write_binding(&String::from("it"), &it);
            env.write_binding(&String::from("ti"), &unlazy_ti);
            env.write_binding(&String::from("idx"), &INTEGERVAL(idx));
            ret = eval_expr(body, env, scribe);
        }
        ret
    } else if let STRINGVAL(str) = arg {
        for (it, idx) in str.chars().zip(0..) {
            env.write_binding(&String::from("it"), &STRINGVAL(it.to_string()));
            env.write_binding(&String::from("idx"), &INTEGERVAL(idx));
            ret = eval_expr(body, env, scribe);
        }
        ret
    } else {
        scribe.annotate_error(Error::on_line(env.curr_line,
                                             ErrorType::EVAL_ITER_APPL_ON_NONITER(arg)));
        ERRVAL
    }
}

pub fn replace_string_placeholders(str: &String, env: &mut Environment, scribe: &mut ErrorScribe) -> String {
    let mut result = String::new();
    let mut varname = String::new();
    for ch in str.chars() {
        match ch {
            '{' => { varname = "_".to_string(); }
            '}' => {
                varname.remove(0);
                let val = env.read(&varname, scribe);
                result += &*val.to_string();
                varname.clear();
            }
            _ => {
                if varname.len() > 0 { varname.push(ch); } else { result.push(ch); }
            }
        }
    }
    result
}
