use std::ops::Deref;

use crate::environment::value::Value;
use crate::environment::value::Value::*;
use crate::errors::ErrorType::*;
use crate::evaluator::Evaluator;
use crate::evaluator::operation::Operation;
use crate::evaluator::operation::OperationType::*;
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::Expression;

pub fn binary_op(eval: &mut Evaluator, tok: &Token) -> Value {
    let rval = eval.val_queue.pop_back().unwrap();
    let lval = eval.val_queue.pop_back().unwrap();
    match tok.ttype {
        PLUS => { lval.plus_them(&rval) }
        MINUS => { lval.minus_them(&rval) }
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
        _ => { eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![lval, rval])) }
    }
}

pub fn assoc_pusher_op(eval: &mut Evaluator) -> Value {
    let v = eval.val_queue.pop_back().unwrap();
    let k = eval.val_queue.pop_back().unwrap();
    let obj = eval.val_queue.pop_back().unwrap();
    if let ASSOCIATIONVAL(mut map) = obj {
        // removal if v == _, and change is on default if k == _
        match (k, v) {
            (UNDERSCOREVAL, UNDERSCOREVAL) => { map.default = None }
            (UNDERSCOREVAL, val) => { map.default = Some(Box::from(val.to_owned())); }
            (key, UNDERSCOREVAL) => { map.remove(key); }
            (key, val) => { map.insert(key, val); }
        }
        ASSOCIATIONVAL(map)
    } else { eval.error(EVAL_INVALID_PUSH) }
}

pub fn at_applicable_resolver_op(eval: &mut Evaluator) -> Value {
    let body_val = eval.val_queue.pop_back().unwrap();
    let body = if let LAMBDAVAL { params: _params, body } = body_val {
        body
    } else { return eval.error(EVAL_ARGS_TO_NOT_APPLICABLE); };
    eval.add_scope_closure_lazily(1);
    eval.exp_queue.push_back(*body);
    NOTAVAL
}

pub fn iterative_param_binder_op(eval: &mut Evaluator, past_iterations: &usize, iterand: &Box<Value>, body: &Box<Expression>) -> Result<Value, Value> {
// eat previous iteration. this doesn't eat the last iteration since that is consumed in
    // the closing operation
    if *past_iterations != 0 { eval.val_queue.pop_back().unwrap(); }
    let it;
    let ti;
    let idx;
    let len;
    match iterand.deref() {
        STRINGVAL(str) => {
            it = STRINGVAL(String::from(str.chars().nth(*past_iterations).unwrap()));
            ti = STRINGVAL(String::from(str.chars().nth(*past_iterations).unwrap()));
            idx = INTEGERVAL(*past_iterations as i64);
            len = str.len();
        }
        ASSOCIATIONVAL(map) => {
            it = map.ith_key(past_iterations);
            ti = map.ith_val(past_iterations);
            idx = INTEGERVAL(*past_iterations as i64);
            len = map.len();
        }
        _ => { return Err(eval.error(EVAL_ITER_APPL_ON_NONITER(iterand.deref().clone()))); }
    }
    eval.env.write_binding(&"it".to_string(), &it);
    eval.env.write_binding(&"ti".to_string(), &ti);
    eval.env.write_binding(&"idx".to_string(), &idx);
    if len > past_iterations + 1 {
        eval.op_queue.push_back(Operation::from_type(ITERATIVE_PARAM_BINDER(
            past_iterations + 1,
            iterand.to_owned(), body.to_owned(),
        )));
        eval.exp_queue.push_back(body.deref().clone());
    } else { eval.add_scope_closure_lazily(1) }
    // before all of this, however, we need to unlazy the map's value and put it back into ti.
    if let LAZYVAL(ex) = ti {
        eval.exp_queue.push_back(*ex);
        eval.op_queue.push_back(Operation::from_type(TI_REBINDER_OP));
    }
    Ok(NOTAVAL)
}

pub fn bind_application_args_to_params_op(eval: &mut Evaluator, amount_of_passed_args: &usize, tok: &Token) -> Result<Value, Value> {
    eval.create_scope_lazily();
    if tok.type_equals(&ATAT) {
        // assoc_arg @@ it-expression. another operation takes over, binding one iteration of params at a time.
        let arg = eval.val_queue.pop_back().unwrap();
        return Err(match arg {
            ASSOCIATIONVAL(_) => {
                Operation::from_type(ITERATIVE_PARAM_BINDER(
                    0, Box::from(arg), Box::from(eval.exp_queue.back().unwrap().clone())))
                    .value(eval, &NOTAVAL)
            }
            STRINGVAL(_) => {
                Operation::from_type(ITERATIVE_PARAM_BINDER(
                    0, Box::from(arg), Box::from(eval.exp_queue.back().unwrap().clone())))
                    .value(eval, &NOTAVAL)
            }
            _ => eval.error(EVAL_ITER_APPL_ON_NONITER(arg))
        });
    }
    // | args | @ lambdaval, where lambdaval = | params | body
    let body_expr = eval.exp_queue.back().unwrap().clone();
    let body_expr = body_expr.ok_or_var_with_applicable(eval);
    let lambda_contents = if let Expression::APPLICABLE_EXPR {
        params, body
    } = body_expr.deref() {
        (params.deref().clone(), body.deref().clone())
    } else {
        dbg!(body_expr);
        return Err(eval.error(EVAL_ARGS_TO_NOT_APPLICABLE));
    };

    let params = lambda_contents.0;
    let params = if let Expression::ARGS(params) = params {
        params
    } else { unreachable!() };
    if params.len() != *amount_of_passed_args {
        // only acceptable when calling an argless function with _.
        if *amount_of_passed_args > params.len()
            && *amount_of_passed_args - params.len() == 1
            && eval.val_queue.back().is_some_and(
            |v| v.type_equals(&UNDERSCOREVAL)) { eval.val_queue.pop_back(); } else {
            return Err(eval.error(EVAL_UNEXPECTED_NUMBER_OF_PARAMS {
                passed: *amount_of_passed_args,
                expected: params.len(),
            }));
        }
    }
    for param in params {
        if let Expression::VAR_RAW(_, varname) = *param {
            eval.env.write_binding(&varname, &eval.val_queue.pop_back().unwrap());
        } else { return Err(eval.error(GENERICERROR)); }
    }
    Ok(NOTAVAL)
}

pub fn pull_op(eval: &mut Evaluator, tok: &Token) -> Value {
    let key = eval.val_queue.pop_back().unwrap();
    let source = eval.val_queue.pop_back().unwrap();
    return match source {
        ASSOCIATIONVAL(ref map) => {
            // try to read key, if absent try to grab default
            let val = map.get(&key).or(
                map.default.as_deref().map(|v| v.clone())
            );
            let val = match (&tok.ttype, val.clone()) {
                (PULL, Some(v)) => { v }
                (PULLEXTRACT, Some(v)) => { v }
                (PULL, None) => { OPTIONVAL(None) }
                (PULLEXTRACT, None) => { eval.error(EVAL_KEY_NOT_FOUND) }
                _ => eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(),
                                                vec![source.to_owned(), val.clone().unwrap()]))
            };
            if let LAZYVAL(expr) = val {
                eval.exp_queue.push_back(*expr);
                if &tok.ttype == &PULL { eval.op_queue.push_back(Operation::from_type(OPTIONAL_OP)) }
                NOTAVAL
            } else { val }
        }
        _ => eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![source.to_owned()]))
    };
}

pub fn lazy_logic_op(eval: &mut Evaluator, tok: &Token) -> Result<Value, Value> {
    let lhs = eval.val_queue.back().unwrap();
    if lhs.as_bool_val().type_equals(&ERRVAL) {
        return Err(eval.error(EVAL_NOT_BOOLEANABLE(lhs.to_owned())));
    }
    let can_exit_early = match (&tok.ttype, lhs.as_bool_val()) {
        (AND, BOOLEANVAL(bool)) => { !bool }
        (OR, BOOLEANVAL(bool)) => { bool }
        _ => { false }
    };
    Ok(if can_exit_early {
        // no need to eval next expr. remove rhs from stack, pop value that was read and return.
        // early exit from "or" is because whole expr is true, and the inverse goes for "and"
        eval.exp_queue.pop_back();
        eval.val_queue.pop_back();
        BOOLEANVAL(tok.type_equals(&OR))
    } else {
        eval.op_queue.push_back(Operation::from_type(LOGIC_OP(tok.to_owned())));
        eval.val_queue.pop_back().unwrap() // will be wrapped back as an expr and evaluated by LOGIC OP
    })
}