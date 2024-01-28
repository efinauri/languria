use std::ops::Deref;

use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::errors::ErrorType::*;
use crate::evaluator::Evaluator;
use crate::evaluator::operation::OperationType::*;
use crate::lexer::{Coord, Token};
use crate::lexer::TokenType::*;
use crate::parser::Expression;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum OperationType {
    BINARY_OP(Token),
    OPTIONAL_OP,
    LAZY_LOGIC_OP(Token),
    LOGIC_OP(Token),
    UNARY_OP(Token),
    VARASSIGN_OP(String, Token),
    SCOPE_DURATION_COUNTDOWN_OP(usize, bool),
    RETURN_CLEANUP,
    ASSOC_GROWER_OP(ValueMap, usize, bool),
    PULL_OP(Token),
    BIND_APPLICATION_ARGS_TO_PARAMS_OP(usize, Token),
    AT_APPLICABLE_RESOLVER_OP,
    ASSOC_PUSHER_OP,
    ITERATIVE_PARAM_BINDER(usize, Box<Value>, Box<Expression>),
    TI_REBINDER_OP,
}

#[derive(Debug)]
pub struct Operation {
    pub needed_to_see_values: usize,
    pub seen_values: usize,
    pub otype: OperationType,
    pub coord: Coord,
}

impl Operation {
    pub fn needed_to_keep_values(&self) -> usize {
        match self.otype {
            SCOPE_DURATION_COUNTDOWN_OP(_, _) => 1,
            _ => self.needed_to_see_values
        }
    }

    pub fn from_type(otype: OperationType) -> Operation {
        let needed_values = match &otype {
            BINARY_OP { .. } => 2,
            OPTIONAL_OP => 1,
            LAZY_LOGIC_OP(_) => 1,
            LOGIC_OP(_) => 2,
            UNARY_OP(_) => 1,
            VARASSIGN_OP(_, _) => 1,
            SCOPE_DURATION_COUNTDOWN_OP(n, _) => *n,
            RETURN_CLEANUP => 1,
            ASSOC_GROWER_OP(_, _, _) => 1,
            PULL_OP(_) => 2,
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(n, _) => *n,
            AT_APPLICABLE_RESOLVER_OP => 1,
            ASSOC_PUSHER_OP => 3,
            ITERATIVE_PARAM_BINDER(_, _, _) => 1,
            TI_REBINDER_OP => 1,
        };
        Operation {
            seen_values: 0,
            otype,
            needed_to_see_values: needed_values,
            coord: Default::default(),
        }
    }

    /// removes the operation's pending state. used when the operation is popped from the queue
    /// without being normally executed.
    fn flush(&self, eval: &mut Evaluator) {
        // trim values calculated for its future execution.
        eval.val_queue.truncate(1 + eval.val_queue.len() - self.needed_to_keep_values());
        // trim expression that would've calculated the rest of the needed values.
        eval.exp_queue.truncate(1 + eval.exp_queue.len() + self.seen_values - self.needed_to_see_values);
        // +1 in both cases because the expr that caused the early dropping should be counted.
    }

    pub fn set_coord(&mut self, coord: &Coord) { self.coord = (*coord).clone() }

    pub fn value(&self, eval: &mut Evaluator) -> Value {
        match &self.otype {
            BINARY_OP(tok) => {
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
            OPTIONAL_OP => { OPTIONVAL(Some(Box::from(eval.val_queue.pop_back().unwrap()))) }
            LAZY_LOGIC_OP(tok) => {
                let lhs = eval.val_queue.back().unwrap();
                if lhs.as_bool_val().type_equals(&ERRVAL) {
                    return eval.error(EVAL_NOT_BOOLEANABLE(lhs.to_owned()));
                }
                let can_exit_early = match (&tok.ttype, lhs.as_bool_val()) {
                    (AND, BOOLEANVAL(bool)) => { !bool }
                    (OR, BOOLEANVAL(bool)) => { bool }
                    _ => { false }
                };
                if can_exit_early {
                    // no need to eval next expr. remove rhs from stack, pop value that was read and return.
                    // early exit from "or" is because whole expr is true, and the inverse goes for "and"
                    eval.exp_queue.pop_back();
                    eval.val_queue.pop_back();
                    BOOLEANVAL(tok.type_equals(&OR))
                } else {
                    eval.op_queue.push_back(Operation::from_type(LOGIC_OP(tok.to_owned())));
                    eval.val_queue.pop_back().unwrap() // will be wrapped back as an expr and evaluated by LOGIC OP
                }
            }
            LOGIC_OP(tok) => {
                let rhs = eval.val_queue.pop_back().unwrap();
                let lhs = eval.val_queue.pop_back().unwrap();
                match (rhs.as_bool_val(), &tok.ttype, lhs.as_bool_val()) {
                    (BOOLEANVAL(b1), AND, BOOLEANVAL(b2)) => BOOLEANVAL(b1 && b2),
                    (BOOLEANVAL(b1), OR, BOOLEANVAL(b2)) => BOOLEANVAL(b1 || b2),
                    (BOOLEANVAL(b1), XOR, BOOLEANVAL(b2)) => BOOLEANVAL(b1 ^ b2),
                    _ => eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![lhs, rhs]))
                }
            }
            UNARY_OP(tok) => {
                let val = eval.val_queue.pop_back().unwrap();
                let try_unary = match tok.ttype {
                    EXTRACT => val.extract(),
                    ASBOOL => { val.as_bool_val() }
                    NOT => { val.not_it() }
                    MINUS => { val.minus_it() }
                    DOLLAR => {
                        val.print_it(tok.coord.row, eval.env, None);
                        val.clone()
                    }
                    _ => {
                        eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![val.clone()]))
                    }
                };
                if try_unary.type_equals(&ERRVAL) {
                    eval.error(EVAL_INVALID_OP(tok.ttype.to_owned(), vec![val]))
                } else { try_unary }
            }
            VARASSIGN_OP(varname, op) => {
                eval.env.write(varname, &eval.val_queue.pop_back().unwrap(), op)
            }
            SCOPE_DURATION_COUNTDOWN_OP(_, optmizing_tail_call) => {
                if !optmizing_tail_call { eval.env.destroy_scope() };
                eval.val_queue.pop_back().unwrap()
            }
            RETURN_CLEANUP => {
                let return_val = eval.val_queue.pop_back().unwrap();
                while let Some(op) = eval.op_queue.pop_back() {
                    op.flush(eval);
                    if let SCOPE_DURATION_COUNTDOWN_OP(_, _) = op.otype { break; }
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
                dbg!(&eval.exp_queue);
                dbg!(&eval.op_queue);
                dbg!(&eval.val_queue);
                return_val
            }
            ASSOC_GROWER_OP(map, n, lazy) => {
                let mut map = map.to_owned();
                let k = eval.val_queue.pop_back().unwrap();
                if *lazy {
                    let v = LAZYVAL(Box::from(eval.exp_queue.pop_back().unwrap()));
                    if k.type_equals(&UNDERSCOREVAL) {
                        map.default = Some(Box::from(v.to_owned()));
                    } else { map.insert(k, v); }
                }
                if *n == 1 {
                    return ASSOCIATIONVAL(map);
                }
                eval.op_queue.push_back(Operation::from_type(ASSOC_GROWER_OP(map, n - 1, *lazy)));
                NOTAVAL
            }
            PULL_OP(tok) => {
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
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(amount_of_passed_args, tok) => {
                if tok.type_equals(&AT) { eval.env.create_scope(); }
                eval.env.current_scope_usages += 1;
                if tok.type_equals(&ATAT) {
                    // assoc_arg @@ it-expression. another operation takes over, binding one iteration of params at a time.
                    let arg = eval.val_queue.pop_back().unwrap();
                    return match arg {
                        ASSOCIATIONVAL(_) => {
                            Operation::from_type(ITERATIVE_PARAM_BINDER(
                                0, Box::from(arg), Box::from(eval.exp_queue.back().unwrap().clone())))
                                .value(eval)
                        }
                        STRINGVAL(_) => {
                            Operation::from_type(ITERATIVE_PARAM_BINDER(
                                0, Box::from(arg), Box::from(eval.exp_queue.back().unwrap().clone())))
                                .value(eval)
                        }
                        _ => eval.error(EVAL_ITER_APPL_ON_NONITER(arg))
                    };
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
                    return eval.error(EVAL_ARGS_TO_NOT_APPLICABLE);
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
                        return eval.error(EVAL_UNEXPECTED_NUMBER_OF_PARAMS {
                            passed: *amount_of_passed_args,
                            expected: params.len(),
                        });
                    }
                }
                for param in params {
                    if let Expression::VAR_RAW(_, varname) = *param {
                        eval.env.write_binding(&varname, &eval.val_queue.pop_back().unwrap());
                    } else { return eval.error(GENERICERROR); }
                }
                NOTAVAL
            }
            ITERATIVE_PARAM_BINDER(past_iterations, iterand, body) => {
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
                    _ => { return eval.error(EVAL_ITER_APPL_ON_NONITER(iterand.deref().clone())); }
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
                } else { eval.op_queue.push_back(Operation::from_type(SCOPE_DURATION_COUNTDOWN_OP(1, false))) }
                // before all of this, however, we need to unlazy the map's value and put it back into ti.
                if let LAZYVAL(ex) = ti {
                    eval.exp_queue.push_back(*ex);
                    eval.op_queue.push_back(Operation::from_type(TI_REBINDER_OP));
                }
                NOTAVAL
            }
            TI_REBINDER_OP => {
                let ti = eval.val_queue.pop_back().unwrap();
                eval.env.write_binding(&"ti".to_string(), &ti);
                NOTAVAL
            }
            AT_APPLICABLE_RESOLVER_OP => {
                // evaluate the body
                let body_val = eval.val_queue.pop_back().unwrap();
                let body = if let LAMBDAVAL { params: _params, body } = body_val {
                    body
                } else { return eval.error(EVAL_ARGS_TO_NOT_APPLICABLE); };
                let optimizing_tail_call = body.is_tail_call_optimizable();
                eval.env.current_scope_usages += optimizing_tail_call as usize;
                eval.exp_queue.push_back(*body);
                eval.op_queue.push_back(Operation::from_type(SCOPE_DURATION_COUNTDOWN_OP(1, optimizing_tail_call)));
                return NOTAVAL;
            }
            ASSOC_PUSHER_OP => {
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
        }
    }
}