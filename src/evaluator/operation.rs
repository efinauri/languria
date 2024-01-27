use std::collections::VecDeque;
use std::ops::Deref;

use crate::environment::Environment;
use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::evaluator::operation::OperationType::*;
use crate::lexer::Token;
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
    SCOPE_DURATION_COUNTDOWN_OP(usize),
    RETURN_CLEANUP,
    ASSOC_GROWER_OP(ValueMap, usize, bool),
    PULL_OP(Token),
    BIND_APPLICATION_ARGS_TO_PARAMS_OP(usize, Token),
    BOUND_APPLICATION_EVALUATOR_OP,
    ASSOC_PUSHER_OP,
    ITERATIVE_PARAM_BINDER(usize, Box<Value>, Box<Expression>),
}

#[derive(Debug)]
pub struct Operation {
    pub needed_values: usize,
    pub seen_values: usize,
    pub otype: OperationType,
}

impl Operation {
    pub fn from_type(otype: OperationType) -> Operation {
        let needed_values = match &otype {
            BINARY_OP { .. } => 2,
            OPTIONAL_OP => 1,
            LAZY_LOGIC_OP(_) => 1,
            LOGIC_OP(_) => 2,
            UNARY_OP(_) => 1,
            VARASSIGN_OP(_, _) => 1,
            SCOPE_DURATION_COUNTDOWN_OP(n) => *n,
            RETURN_CLEANUP => 1,
            ASSOC_GROWER_OP(_, _, _) => 1,
            PULL_OP(_) => 2,
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(n, _) => *n,
            BOUND_APPLICATION_EVALUATOR_OP => 1,
            ASSOC_PUSHER_OP => 3,
            ITERATIVE_PARAM_BINDER(_, _, _) => 1
        };
        Operation {
            seen_values: 0,
            otype,
            needed_values,
        }
    }

    /// removes the operation's pending state. used when the operation is popped from the queue
    /// without being normally executed.
    fn flush(&self,
             vals: &mut VecDeque<Value>,
             _ops: &mut VecDeque<Operation>,
             exprs: &mut &mut VecDeque<Expression>,
             _env: &mut Environment,
    ) {
        //usually an operation keeps every value it sees because it needs them all to make sense,
        // but in other situations this is not necessary.
        let kept_values = match self.otype {
            SCOPE_DURATION_COUNTDOWN_OP(_) => 0,
            _ => self.seen_values
        };
        // trim values calculated for its future execution.
        vals.truncate(vals.len() - kept_values);
        // trim expression that would've calculated the rest of the needed values.
        // +1 because the expr that caused the early dropping was part of the scope.
        exprs.truncate(exprs.len() + 1 + self.seen_values - self.needed_values);
    }


    pub fn value(&self,
                 vals: &mut VecDeque<Value>,
                 ops: &mut VecDeque<Operation>,
                 exprs: &mut &mut VecDeque<Expression>,
                 env: &mut Environment,
    ) -> Value { // TODO replace naked ERRVALs with env.error(right etype for the situation)
        match &self.otype {
            BINARY_OP(tok) => {
                let rval = vals.pop_back().unwrap();
                let lval = vals.pop_back().unwrap();
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
                    _ => { ERRVAL }
                }
            }
            OPTIONAL_OP => { OPTIONVAL(Some(Box::from(vals.pop_back().unwrap()))) }
            LAZY_LOGIC_OP(tok) => {
                let lhs = vals.back().unwrap();
                if lhs.as_bool_val().type_equals(&ERRVAL) { return ERRVAL; }
                let can_exit_early = match (&tok.ttype, lhs.as_bool_val()) {
                    (AND, BOOLEANVAL(bool)) => { !bool }
                    (OR, BOOLEANVAL(bool)) => { bool }
                    _ => { false }
                };
                if can_exit_early {
                    // no need to eval next expr. remove rhs from stack, pop value that was read and return.
                    // early exit from "or" is because whole expr is true, and the inverse goes for "and"
                    exprs.pop_back();
                    vals.pop_back();
                    BOOLEANVAL(tok.type_equals(&OR))
                } else {
                    ops.push_back(Operation::from_type(LOGIC_OP(tok.to_owned())));
                    vals.pop_back().unwrap() // will be wrapped back as an expr and evaluated by LOGIC OP
                }
            }
            LOGIC_OP(tok) => {
                let rhs = vals.pop_back().unwrap();
                let lhs = vals.pop_back().unwrap();
                match (rhs.as_bool_val(), &tok.ttype, lhs.as_bool_val()) {
                    (BOOLEANVAL(b1), AND, BOOLEANVAL(b2)) => BOOLEANVAL(b1 && b2),
                    (BOOLEANVAL(b1), OR, BOOLEANVAL(b2)) => BOOLEANVAL(b1 || b2),
                    (BOOLEANVAL(b1), XOR, BOOLEANVAL(b2)) => BOOLEANVAL(b1 ^ b2),
                    _ => ERRVAL
                }
            }
            UNARY_OP(tok) => {
                let val = vals.pop_back().unwrap();
                match tok.ttype {
                    EXTRACT => val.extract(),
                    ASBOOL => { val.as_bool_val() }
                    NOT => { val.not_it() }
                    MINUS => { val.minus_it() }
                    DOLLAR => { /*TODO val.print_it();*/ val.clone() }
                    _ => ERRVAL
                }
            }
            VARASSIGN_OP(varname, op) => {
                env.write(varname, &vals.pop_back().unwrap(), op)
            }
            SCOPE_DURATION_COUNTDOWN_OP(_) => {
                env.destroy_scope();
                vals.pop_back().unwrap()
            }
            RETURN_CLEANUP => {
                let return_val = vals.pop_back().unwrap();
                while let Some(op) = ops.pop_back() {
                    dbg!("flushing.", &op);
                    op.flush(vals, ops, exprs, env);
                    if let SCOPE_DURATION_COUNTDOWN_OP(_) = op.otype { break; }
                }
                env.destroy_scope();
                return_val
            }
            ASSOC_GROWER_OP(map, n, lazy) => {
                let mut map = map.to_owned();
                let k = vals.pop_back().unwrap();
                if *lazy {
                    let v = LAZYVAL(Box::from(exprs.pop_back().unwrap()));
                    if k.type_equals(&UNDERSCOREVAL) {
                        map.default = Some(Box::from(v.to_owned()));
                    } else { map.insert(k, v); }
                }
                if *n == 1 {
                    return ASSOCIATIONVAL(map);
                }
                ops.push_back(Operation::from_type(ASSOC_GROWER_OP(map, n - 1, *lazy)));
                NOTAVAL
            }
            PULL_OP(tok) => {
                let key = vals.pop_back().unwrap();
                let source = vals.pop_back().unwrap();
                return match source {
                    ASSOCIATIONVAL(map) => {
                        // try to read key, if absent try to grab default
                        let val = map.get(&key).or(
                            map.default.as_deref().map(|v| v.clone())
                        );
                        let val = match (&tok.ttype, val) {
                            (PULL, Some(v)) => { v }
                            (PULLEXTRACT, Some(v)) => { v }
                            (PULL, None) => { OPTIONVAL(None) }
                            _ => ERRVAL
                        };
                        if let LAZYVAL(expr) = val {
                            exprs.push_back(*expr);
                            if &tok.ttype == &PULL { ops.push_back(Operation::from_type(OPTIONAL_OP)) }
                            NOTAVAL
                        } else { val }
                    }
                    _ => ERRVAL
                };
            }
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(_, tok) => {
                env.create_scope();
                if tok.type_equals(&ATAT) {
                    // assoc_arg @@ it-expression. another operation takes over, binding one iteration of params at a time.
                    let arg = vals.pop_back().unwrap();
                    return match arg {
                        ASSOCIATIONVAL(_) => {
                            Operation::from_type(ITERATIVE_PARAM_BINDER(
                                0, Box::from(arg), Box::from(exprs.back().unwrap().clone())))
                                .value(vals, ops, exprs, env)
                        }
                        STRINGVAL(_) => {
                            Operation::from_type(ITERATIVE_PARAM_BINDER(
                                0, Box::from(arg), Box::from(exprs.back().unwrap().clone())))
                                .value(vals, ops, exprs, env)
                        }
                        _ => ERRVAL
                    };
                }
                // | args | @ lambdaval, where lambdaval = | params | body
                let lambda_contents = if let Expression::APPLICABLE_EXPR {
                    params, body
                } = exprs.back().unwrap() {
                    (params.deref().clone(), body.deref().clone())
                } else { return ERRVAL; };
                let params = lambda_contents.0;

                let params = if let Expression::ARGS(params) = params {
                    params
                } else { unreachable!() };
                for param in params {
                    if let Expression::VAR_RAW(_, varname) = *param {
                        env.write_binding(&varname, &vals.pop_back().unwrap());
                    } else { return ERRVAL; }
                }
                NOTAVAL
            }
            ITERATIVE_PARAM_BINDER(past_iterations, iterand, body) => {
                // eat previous iteration. this doesn't eat the last iteration since that is consumed in
                // the closing operation
                if *past_iterations != 0 { vals.pop_back().unwrap(); }
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
                    _ => { return ERRVAL; }
                }
                env.write_binding(&"it".to_string(), &it);
                env.write_binding(&"ti".to_string(), &ti);
                env.write_binding(&"idx".to_string(), &idx);
                if len > past_iterations + 1 {
                    ops.push_back(Operation::from_type(ITERATIVE_PARAM_BINDER(
                        past_iterations + 1,
                        iterand.to_owned(), body.to_owned(),
                    )));
                }
                // always push back the body in the exprs even when the above condition is false:
                // still need to go through the last iteration of the application.
                exprs.push_back(body.deref().clone());
                NOTAVAL
            }
            BOUND_APPLICATION_EVALUATOR_OP => {
                // evaluate the body (one last time, in the case of an iterated application).
                let body = if let LAMBDAVAL {
                    params: _params, body
                } = vals.pop_back().unwrap() { body } else { return ERRVAL; };
                    exprs.push_back(*body);
                    ops.push_back(Operation::from_type(SCOPE_DURATION_COUNTDOWN_OP(1)));
                    return NOTAVAL;
            }
            ASSOC_PUSHER_OP => {
                let v = vals.pop_back().unwrap();
                let k = vals.pop_back().unwrap();
                let obj = vals.pop_back().unwrap();
                if let ASSOCIATIONVAL(mut map) = obj {
                    // removal if v == _, and change is on default if k == _
                    match (k, v) {
                        (UNDERSCOREVAL, UNDERSCOREVAL) => { map.default = None }
                        (UNDERSCOREVAL, val) => { map.default = Some(Box::from(val.to_owned())); }
                        (key, UNDERSCOREVAL) => { map.remove(key); }
                        (key, val) => { map.insert(key, val); }
                    }
                    ASSOCIATIONVAL(map)
                } else { ERRVAL }
            }
        }
    }
}