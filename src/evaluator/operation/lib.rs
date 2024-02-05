use std::ops::Deref;

use crate::environment::value::Value::*;
use crate::environment::value::{Value, ValueMap};
use crate::errors::ErrorType::*;
use crate::evaluator::operation::Operation;
use crate::evaluator::operation::OperationType::*;
use crate::evaluator::Evaluator;
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::Expression;

pub fn binary_op(eval: &mut Evaluator, tok: &Token) -> Value {
    let rval = eval.val_queue.pop_back().unwrap();
    let lval = eval.val_queue.pop_back().unwrap();
    match tok.ttype {
        PLUS => lval.plus_them(&rval),
        MINUS => lval.minus_them(&rval),
        MUL => lval.mul_them(&rval),
        DIV => lval.div_them(&rval),
        POW => lval.pow_them(&rval),
        MODULO => lval.modulo_them(&rval),
        GT => lval.cmp_them(&rval, |a, b| a > b),
        GTE => lval.cmp_them(&rval, |a, b| a >= b),
        LT => lval.cmp_them(&rval, |a, b| a < b),
        LTE => lval.cmp_them(&rval, |a, b| a <= b),
        EQ => lval.cmp_them(&rval, |a, b| a == b),
        UNEQ => lval.cmp_them(&rval, |a, b| a != b),
        _ => eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![lval, rval])),
    }
}

pub fn assoc_pusher_op(eval: &mut Evaluator) -> Value {
    let v = eval.val_queue.pop_back().unwrap();
    let k = eval.val_queue.pop_back().unwrap();
    let obj = eval.val_queue.pop_back().unwrap();
    if let ASSOCIATIONVAL(mut map) = obj {
        // removal if v == _, and change is on default if k == _
        match (k, v) {
            (UNDERSCOREVAL, UNDERSCOREVAL) => map.default = None,
            (UNDERSCOREVAL, val) => {
                map.default = Some(Box::from(val.to_owned()));
            }
            (key, UNDERSCOREVAL) => {
                map.remove(key);
            }
            (key, val) => {
                map.insert(key, val);
            }
        }
        ASSOCIATIONVAL(map)
    } else {
        eval.error(EVAL_INVALID_PUSH)
    }
}

pub fn at_applicable_resolver_op(eval: &mut Evaluator) -> Value {
    let body_val = eval.val_queue.pop_back().unwrap();
    let body = if let LAMBDAVAL {
        params: _params,
        body,
    } = body_val
    {
        body
    } else {
        return eval.error(EVAL_ARGS_TO_NOT_APPLICABLE);
    };
    eval.exp_queue.push_back(*body);
    NOTAVAL
}

pub fn iterative_param_binder_op(
    eval: &mut Evaluator,
    past_iterations: &usize,
    iterand: &Box<Value>,
    body: &Box<Expression>,
) -> Value {
    // eval what was predisposed by the last iteration op, and in turn predispose data for the next iteration.
    if *past_iterations != 0 {
        eval.val_queue.pop_back().unwrap();
    }
    let it;
    let ti;
    let idx;
    let len: isize;
    match iterand.deref() {
        STRINGVAL(str) => {
            it = STRINGVAL(String::from(str.chars().nth(*past_iterations).unwrap()));
            ti = STRINGVAL(String::from(str.chars().nth(*past_iterations).unwrap()));
            idx = INTEGERVAL(*past_iterations as i64);
            len = str.len() as isize;
        }
        ASSOCIATIONVAL(map) => {
            it = map.ith_key(past_iterations);
            ti = map.ith_val(past_iterations);
            idx = INTEGERVAL(*past_iterations as i64);
            len = map.len() as isize;
        }
        UNDERSCOREVAL => {
            it = INTEGERVAL(*past_iterations as i64);
            ti = INTEGERVAL(*past_iterations as i64);
            idx = INTEGERVAL(*past_iterations as i64);
            len = -1;
        }
        _ => {
            return eval.error(EVAL_ITER_APPL_ON_NONITER(iterand.deref().clone()));
        }
    }
    eval.env.write_binding(&"it".to_string(), &it);
    eval.env.write_binding(&"ti".to_string(), &ti);
    eval.env.write_binding(&"idx".to_string(), &idx);
    if len < 0 || len > (past_iterations + 1) as isize {
        eval.op_queue
            .push_back(Operation::from_type(ITERATIVE_PARAM_BINDER(
                past_iterations + 1,
                iterand.to_owned(),
                body.to_owned(),
            )));
        eval.exp_queue.push_back(body.deref().clone());
    }
    // before all of this, however, we need to unlazy the map's value and put it back into ti.
    if let LAZYVAL(ex) = ti {
        eval.exp_queue.push_back(*ex);
        eval.op_queue
            .push_back(Operation::from_type(TI_REBINDER_OP));
    }
    NOTAVAL
}

pub fn bind_application_args_to_params_op(
    eval: &mut Evaluator,
    amount_of_passed_args: &usize,
    tok: &Token,
) -> Value {
    if tok.type_equals(&ATAT) {
        // assoc_arg @@ it-expression. another operation takes over, binding one iteration of params at a time.
        let arg = eval.val_queue.pop_back().unwrap();
        return match arg {
            ASSOCIATIONVAL(_) | STRINGVAL(_) | UNDERSCOREVAL => {
                Operation::from_type(ITERATIVE_PARAM_BINDER(
                    0,
                    Box::from(arg),
                    Box::from(eval.exp_queue.back().unwrap().clone()),
                ))
                .value(eval, &NOTAVAL)
            }
            _ => eval.error(EVAL_ITER_APPL_ON_NONITER(arg)),
        };
    }
    // it_arg @ applicable(contour_args)
    // read the body from the queue to understand the params it's requiring. generic expressions are treated as having no args.
    let body_expr = eval.exp_queue.back().unwrap().clone();
    let body_expr = body_expr.ok_or_var_with_applicable(eval);
    let params = if let Expression::APPLICABLE_EXPR {
        params,
        body: _ignored,
    } = body_expr.deref()
    {
        params.deref().clone()
    } else {
        return eval.error(EVAL_ARGS_TO_NOT_APPLICABLE);
    };

    let params = if let Expression::ARGS(mut params) = params {
        params.reverse();
        params
    } else {
        unreachable!()
    };
    if 1 + params.len() != *amount_of_passed_args {
        return eval.error(EVAL_UNEXPECTED_NUMBER_OF_PARAMS {
            passed: *amount_of_passed_args,
            expected: params.len(),
        });
    }
    eval.env
        .write_binding(&"it".to_string(), &eval.val_queue.pop_back().unwrap());
    for param in params {
        if let Expression::VAR_RAW(_, varname) = *param {
            eval.env
                .write_binding(&varname, &eval.val_queue.pop_back().unwrap());
        } else {
            return eval.error(GENERICERROR);
        }
    }
    NOTAVAL
}

pub fn pull_op(eval: &mut Evaluator, tok: &Token) -> Value {
    let key = eval.val_queue.pop_back().unwrap();
    let source = eval.val_queue.pop_back().unwrap();
    return match source {
        ASSOCIATIONVAL(ref map) => {
            // try to read key, if absent try to grab default
            let val = map.get(&key).or(map.default.as_deref().map(|v| v.clone()));
            let val = match (&tok.ttype, val.clone()) {
                (PULL, Some(v)) => v,
                (PULLEXTRACT, Some(v)) => v,
                (PULL, None) => OPTIONVAL(None),
                (PULLEXTRACT, None) => eval.error(EVAL_KEY_NOT_FOUND),
                _ => eval.error(EVAL_INVALID_OP(
                    tok.ttype.to_owned(),
                    vec![source.to_owned(), val.clone().unwrap()],
                )),
            };
            if let LAZYVAL(expr) = val {
                eval.exp_queue.push_back(*expr);
                if &tok.ttype == &PULL {
                    eval.op_queue.push_back(Operation::from_type(OPTIONAL_OP))
                }
                NOTAVAL
            } else {
                val
            }
        }
        _ => eval.error(EVAL_INVALID_OP(
            tok.ttype.to_owned(),
            vec![source.to_owned()],
        )),
    };
}

pub fn lazy_logic_op(eval: &mut Evaluator, tok: &Token) -> Value {
    let lhs = eval.val_queue.back().unwrap();
    if lhs.as_bool_val().type_equals(&ERRVAL) {
        return eval.error(EVAL_NOT_BOOLEANABLE(lhs.to_owned()));
    }
    let can_exit_early = match (&tok.ttype, lhs.as_bool_val()) {
        (AND, BOOLEANVAL(bool)) => !bool,
        (OR, BOOLEANVAL(bool)) => bool,
        _ => false,
    };
    if can_exit_early {
        // no need to eval next expr. remove rhs from stack, pop value that was read and return.
        // early exit from "or" is because whole expr is true; the inverse goes for "and"
        eval.exp_queue.pop_back();
        eval.val_queue.pop_back();
        BOOLEANVAL(tok.type_equals(&OR))
    } else {
        // full evaluation required. delegated to LOGIC_OP
        eval.op_queue
            .push_back(Operation::from_type(LOGIC_OP(tok.to_owned())));
        eval.val_queue.pop_back().unwrap()
    }
}

pub fn assoc_grower_resolver_op(
    eval: &mut Evaluator,
    map: &ValueMap,
    n: &usize,
    is_lazy: &bool,
) -> Value {
    let mut map = map.to_owned();
    let v = eval.val_queue.pop_back().unwrap();
    let k = eval.val_queue.pop_back().unwrap();
    if k.type_equals(&UNDERSCOREVAL) {
        map.default = Some(Box::from(v.to_owned()));
    } else {
        map.insert(k, v);
    }
    if *n <= 1 {
        return ASSOCIATIONVAL(map);
    }
    eval.op_queue
        .push_back(Operation::from_type(ASSOC_GROWER_SETUPPER_OP(
            map,
            n - 1,
            *is_lazy,
        )));
    NOTAVAL
}

pub fn assoc_grower_setupper_op(
    eval: &mut Evaluator,
    previous_val: &Value,
    map: &ValueMap,
    n: &usize,
    is_lazy: &bool,
) -> Value {
    let resolver = Operation::from_type(ASSOC_GROWER_RESOLVER_OP(map.to_owned(), *n, *is_lazy));
    // if lazy, don't need to evaluate value and can call resolver directly. else, first evaluate value.
    if *is_lazy {
        eval.val_queue
            .push_back(LAZYVAL(Box::from(eval.exp_queue.pop_back().unwrap())));
        return resolver.value(eval, previous_val);
    } else {
        eval.op_queue
            .push_back(Operation::from_type(ASSOC_GROWER_RESOLVER_OP(
                map.to_owned(),
                *n,
                *is_lazy,
            )));
        NOTAVAL
    }
}

pub fn return_cleanup_op(eval: &mut Evaluator) -> Value {
    let return_val = eval.val_queue.pop_back().unwrap();
    while let Some(op) = eval.op_queue.pop_back() {
        op.flush(eval);
        if let SCOPE_CLOSURE_OP(_) = op.otype {
            break;
        }
    }
    // if, after having exited block, we were also inside an @@ application, exit from that as well.
    // the exited iteration was also enqueuing 1 exp and 1 val.
    if let Some(op) = eval.op_queue.back() {
        if let ITERATIVE_PARAM_BINDER(_, _, _) = op.otype {
            eval.op_queue.pop_back().unwrap().flush(eval);
            eval.exp_queue.pop_back();
            eval.val_queue.pop_back();
        }
    }
    eval.env.destroy_scope();
    return_val
}

pub fn scope_closure_op(eval: &mut Evaluator, previous_val: &Value) -> Value {
    eval.env.destroy_scope();
    previous_val.to_owned()
}

pub fn unary_op(eval: &mut Evaluator, tok: &Token) -> Value {
    let val = eval.val_queue.pop_back().unwrap();
    let try_unary = match tok.ttype {
        EXTRACT => val.unwrap_option(),
        ASBOOL => val.as_bool_val(),
        NOT => val.not_it(),
        MINUS => val.minus_it(),
        _ => eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![val.clone()])),
    };
    if try_unary.type_equals(&ERRVAL) {
        eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![val]))
    } else {
        try_unary
    }
}

pub fn logic_op(eval: &mut Evaluator, tok: &&Token) -> Value {
    let rhs = eval.val_queue.pop_back().unwrap();
    let lhs = eval.val_queue.pop_back().unwrap();
    match (rhs.as_bool_val(), &tok.ttype, lhs.as_bool_val()) {
        (BOOLEANVAL(b1), AND, BOOLEANVAL(b2)) => BOOLEANVAL(b1 && b2),
        (BOOLEANVAL(b1), OR, BOOLEANVAL(b2)) => BOOLEANVAL(b1 || b2),
        (BOOLEANVAL(b1), XOR, BOOLEANVAL(b2)) => BOOLEANVAL(b1 ^ b2),
        _ => eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![lhs, rhs])),
    }
}

pub fn print_op(eval: &mut Evaluator, tag: &Option<String>) -> Value {
    let val = eval.val_queue.pop_back().unwrap();
    val.print_it(eval.env, tag.to_owned());
    val.clone()
}

pub fn ti_rebinder_op(eval: &mut Evaluator) -> Value {
    let ti = eval.val_queue.pop_back().unwrap();
    eval.env.write_binding(&"ti".to_string(), &ti);
    NOTAVAL
}
