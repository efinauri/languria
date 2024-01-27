use std::collections::VecDeque;
use std::ops::Deref;

use crate::environment::Environment;
use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::{ASSOCIATIONVAL, BOOLEANVAL, ERRVAL, LAMBDAVAL, LAZYVAL, NOTAVAL, OPTIONVAL, UNDERSCOREVAL};
use crate::evaluator::operation::OperationType::*;
use crate::lexer::Token;
use crate::lexer::TokenType::{AND, ASBOOL, DIV, DOLLAR, EQ, EXTRACT, GT, GTE, LT, LTE, MINUS, MODULO, MUL, NOT, OR, PLUS, POW, PULL, PULLEXTRACT, UNEQ, XOR};
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
    SCOPE_CLEANUP_OP(usize),
    RETURN_CLEANUP,
    ASSOC_GROWER_OP(ValueMap, usize, bool),
    PULL_OP(Token),
    APPLICATION_OP(Token, usize),
    APPLICATION_CLEANUP,
    ASSOC_PUSHER_OP,
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
            SCOPE_CLEANUP_OP(n) => *n,
            RETURN_CLEANUP => 1,
            ASSOC_GROWER_OP(_, _, _) => 1,
            PULL_OP(_) => 2,
            APPLICATION_OP(_, n) => *n,
            APPLICATION_CLEANUP => 1,
            ASSOC_PUSHER_OP => 3,
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
             ops: &mut VecDeque<Operation>,
             exprs: &mut &mut VecDeque<Expression>,
             env: &mut Environment,
    ) {
        //usually an operation keeps every value it sees because it needs them all to make sense,
        // but in other situations this is not necessary.
        let kept_values = match self.otype {
            SCOPE_CLEANUP_OP(_) => 0,
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
                    // no need to eval next expr, and return overall result
                    // early exit from "or" is because whole expr is true, and the inverse goes for "and"
                    exprs.pop_back();
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
            SCOPE_CLEANUP_OP(_) => {
                env.destroy_scope();
                vals.pop_back().unwrap()
            }
            RETURN_CLEANUP => {
                let return_val = vals.pop_back().unwrap();
                while let Some(op) = ops.pop_back() {
                    op.flush(vals, ops, exprs, env);
                    if let SCOPE_CLEANUP_OP(_) = op.otype { break; }
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
            APPLICATION_OP(_, _) => {
                // | args | @ lambdaval, where lambdaval has params and actual body)
                env.create_scope();
                // body must contain lambda
                let lambda_contents = if let Expression::APPLICABLE_EXPR { params, body } = exprs.back().unwrap() {
                    (params.deref().clone(), body.deref().clone())
                } else { return ERRVAL; };
                let params = if let Expression::ARGS(params) = lambda_contents.0 { params } else { unreachable!() };
                for param in params {
                    if let Expression::VAR_RAW(_, varname) = *param {
                        env.write_binding(&varname, &vals.pop_back().unwrap());
                    } else { return ERRVAL; }
                }
                NOTAVAL
            }
            APPLICATION_CLEANUP => {
                if let LAMBDAVAL { params: _params, body } = vals.pop_back().unwrap() {
                    exprs.push_back(*body);
                };
                NOTAVAL
            }
            ASSOC_PUSHER_OP => {
                let v = vals.pop_back().unwrap();
                let k = vals.pop_back().unwrap();
                let obj = vals.pop_back().unwrap();
                if let ASSOCIATIONVAL(mut map) = obj {
                    // removal if v == _, and change is on default if k == _
                    match (k, v) {
                        (UNDERSCOREVAL, UNDERSCOREVAL) => { map.default = None }
                        (UNDERSCOREVAL, val) => {map.default = Some(Box::from(val.to_owned())); }
                        (key, UNDERSCOREVAL) => {map.remove(key); }
                        (key, val) => {map.insert(key, val); }
                    }
                    ASSOCIATIONVAL(map)
                } else { ERRVAL }
            }
        }
    }
}