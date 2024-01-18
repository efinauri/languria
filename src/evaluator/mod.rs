use std::ops::Deref;

use crate::environment::{Environment, print_eol, Value, ValueMap};
use crate::environment::Value::*;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::errors::ErrorType::EVAL_UNASSIGNED_VAR;
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
        Expression::QUERY { source: rhs, field: lhs, .. }
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
            //exit on first evaluation error. careful about side effects.
            ERRVAL => {
                result = eval;
                break;
            }
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
        env.write_binding(&String::from("it"), &it);
        eval_expr(body, env, es)
    } else {
        let arg = eval_expr(arg, env, es);
        let mut ret = NOTAVAL;
        if let ASSOCIATIONVAL(map) = arg {
            for ((it, ti), idx) in map.iter().zip(0..) {
                let unlazy_ti;
                if let LAZYVAL(ex) = ti.deref() { unlazy_ti = eval_expr(ex, env, es); } else { unlazy_ti = ti.deref().clone() }
                env.write_binding(&String::from("it"), &it);
                env.write_binding(&String::from("ti"), &unlazy_ti);
                env.write_binding(&String::from("idx"), &INTEGERVAL(idx));
                ret = eval_expr(body, env, es);
            }
            ret
        } else if let STRINGVAL(str) = arg {
            for (it, idx) in str.chars().zip(0..) {
                env.write_binding(&String::from("it"), &STRINGVAL(it.to_string()));
                env.write_binding(&String::from("idx"), &INTEGERVAL(idx));
                ret = eval_expr(body, env, es);
            }
            ret
        } else {
            es.annotate_error(Error::on_line(env.curr_line,
                                             ErrorType::EVAL_ITER_APPL_ON_NONITER(arg)));
            ERRVAL
        }
    };
    env.destroy_scope();
    return ret;
}

fn resolve_query(
    expression: Option<Box<Expression>>,
    value: Option<Value>,
    env: &mut Environment,
    scribe: &mut ErrorScribe,
    op: &Token,
) -> Value {
    // if something was successfully queried, it is first evaluated (if it's not a proper value yet) and then returned.
    // keynotfound is thrown otherwise.
    match (&op.ttype, expression, value) {
        (POUND, Some(ex), None) => { OPTIONVAL(Some(Box::new(eval_expr(&ex, env, scribe)))) }
        (POUND, None, Some(val)) => { OPTIONVAL(Some(Box::new(val.clone()))) }
        (POUND, None, None) => { OPTIONVAL(None) }
        (POUNDPOUND, Some(ex), None) => { eval_expr(&ex, env, scribe) }
        (POUNDPOUND, None, Some(val)) => { val.clone() }
        (_, _, _) => {
            scribe.annotate_error(Error::on_line(env.curr_line,
                                                 ErrorType::EVAL_KEY_NOT_FOUND));
            ERRVAL
        }
    }
}


fn eval_expr(expr: &Expression, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
    match expr {
        Expression::VAR_RAW(varname) => {
            let read_val = env.read(varname);
            if read_val.is_none() {
                scribe.annotate_error(Error::on_line(env.curr_line,
                                                     EVAL_UNASSIGNED_VAR(varname.clone())));
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
        Expression::VAR_RAW(_) => ERRVAL, // unreachable because it's handled above
        Expression::QUERY { source, op, field } => {
            let field = eval_expr(field, env, scribe);
            let source = eval_expr(source, env, scribe);
            return match source {
                ASSOCIATIONVAL(map) => {
                    let mut query_expr = None;
                    let mut query_val = None;
                    if let Some(val) = map.get(&field) {
                        if let LAZYVAL(ex) = val { query_expr = Some(ex); } else { query_val = Some(val); };
                    } else if let Some(val) = map.default {
                        if let LAZYVAL(ex) = val.deref() { query_expr = Some(ex.clone()); } else { query_val = Some(*val); };
                    }
                    return resolve_query(query_expr, query_val, env, scribe, op);
                }
                _ => {
                    scribe.annotate_error(Error::on_line(env.curr_line,
                                                         ErrorType::EVAL_UNQUERIABLE(source)));
                    ERRVAL
                }
            };
        }
        Expression::ASSOCIATION(pairs) => { eval_association(pairs, true, env, scribe) }
        Expression::RETURN_EXPR(expr) => { RETURNVAL(Box::new(eval_expr(expr, env, scribe))) }
        Expression::APPLICATION { arg, op, body } => {
            eval_application(arg, op.clone(), body, env, scribe)
        }
        Expression::BLOCK(exprs) => {
            evaluate_expressions(exprs, scribe, env, true)
        }
        Expression::NOTANEXPR => {
            scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_INVALID_EXPR));
            ERRVAL
        }
        Expression::GROUPING(expr) => { eval_expr(expr, env, scribe) }
        Expression::VAR_ASSIGN { varname, op, varval } => {
            let val = eval_expr(varval, env, scribe);
            if val.type_equals(&ERRVAL) { return val; }
            env.write(varname, &val, op)
        }
        Expression::LITERAL(value) => {
            match &value.ttype {
                EOLPRINT => print_eol(env, &value.line),
                FALSE => BOOLEANVAL(false),
                TRUE => BOOLEANVAL(true),
                STRING(str) => STRINGVAL(fill_in_string_tokens(str, env, scribe)),
                INTEGER(int) => INTEGERVAL(*int),
                FLOAT(flt) => FLOATVAL(*flt),
                IT => { env.read(&"it".to_string()).unwrap().clone() }
                TI => { env.read(&"ti".to_string()).unwrap().clone() }
                IDX => { env.read(&"idx".to_string()).unwrap().clone() }
                _ => {
                    scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_INVALID_LITERAL));
                    ERRVAL
                }
            }
        }

        Expression::UNARY { op, expr } => {
            if op.ttype == BANGBANG {
                if let Expression::ASSOCIATION(pairs) = expr.deref() {
                    return eval_association(pairs, false, env, scribe);
                }
            }

            let val = eval_expr(expr, env, scribe);
            let ret = match op.ttype {
                BANG => { val.bang_it() }
                MINUS => { val.minus_it() }
                DOLLAR => {
                    val.print_it(op.line, env, None);
                    val.clone()
                }
                _ => {
                    ERRVAL
                }
            };
            if ret.type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(
                    env.curr_line, ErrorType::EVAL_INVALID_OP(
                        op.clone().ttype, vec![val.clone()])));
            }
            ret
        }

        Expression::LOGIC { lhs, op, rhs } => {
            let lval = eval_expr(lhs, env, scribe);
            if lval.as_bool_val().type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::NOT_BOOLEANABLE(lval)));
                return ERRVAL;
            }
            let ret = match (&op.ttype, lval.as_bool_val()) {
                (AND, BOOLEANVAL(bool)) => {
                    if !bool { return lval; } else { eval_expr(rhs, env, scribe) }
                }
                (OR, BOOLEANVAL(bool)) => {
                    if bool { return lval; } else { eval_expr(rhs, env, scribe) }
                }
                (XOR, BOOLEANVAL(bool)) => {
                    let ehrs = eval_expr(rhs, env, scribe);
                    if bool { ehrs.bang_it() } else { ehrs }
                }
                _ => {
                    ERRVAL
                }
            };
            if ret.as_bool_val().type_equals(&ERRVAL) {
                scribe.annotate_error(
                    Error::on_line(env.curr_line, ErrorType::NOT_BOOLEANABLE(ret.clone())));
                return ERRVAL;
            }
            ret
        }
        Expression::BINARY { lhs, op, rhs } => {
            let lval = eval_expr(lhs, env, scribe);
            if op.ttype == DOLLAR {
                lval.print_it(op.line, env, Some(rhs));
                return lval;
            }
            let rval = eval_expr(rhs, env, scribe);

            let ret = match op.ttype {
                MINUS => { lval.minus_them(&rval) }
                PLUS => { lval.plus_them(&rval) }
                MUL => { lval.mul_them(&rval) }
                DIV => { lval.div_them(&rval) }
                POW => { lval.pow_them(&rval) }
                MODULO => { lval.modulo_them(&rval) }
                GT => { lval.cmp_them(&rval, |a, b| a > b) }
                GTE => { lval.cmp_them(&rval, |a, b| a >= b) }
                LT => { lval.cmp_them(&rval, |a, b| a < b) }
                LTE => { lval.cmp_them(&rval, |a, b| a <= b) }
                EQ => { lval.cmp_them(&rval, |a, b| a == b) }
                UNEQ => { lval.cmp_them(&rval, |a, b| a != b) }
                _ => {
                    ERRVAL
                }
            };
            if ret.type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(
                    env.curr_line, ErrorType::EVAL_INVALID_OP(
                        op.clone().ttype, vec![lval.clone(), rval.clone()])));
            }
            ret
        }
    }
}

fn eval_association(
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
        if let Expression::LITERAL(tok) = k.deref() {
            if tok.type_equals(&UNDERSCORE) {
                map.default = Some(Box::new(v));
                continue;
            }
        }
        map.insert(eval_expr(k, env, scribe), v);
    }
    ASSOCIATIONVAL(map)
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
                    es.annotate_error(Error::on_line(env.curr_line, EVAL_UNASSIGNED_VAR(varname.clone())));
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
