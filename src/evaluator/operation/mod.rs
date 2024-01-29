use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::errors::ErrorType::*;
use crate::evaluator::Evaluator;
use crate::evaluator::operation::OperationType::*;
use crate::lexer::{Coord, Token};
use crate::lexer::TokenType::*;
use crate::parser::Expression;

mod lib;

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
            SCOPE_DURATION_COUNTDOWN_OP(_) => 0,
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
            SCOPE_DURATION_COUNTDOWN_OP(n) => *n,
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
    pub(crate) fn flush(&self, eval: &mut Evaluator) {
        // trim values calculated for its future execution.
        eval.val_queue.truncate(1 + eval.val_queue.len() - self.needed_to_keep_values());
        // trim expression that would've calculated the rest of the needed values.
        eval.exp_queue.truncate(1 + eval.exp_queue.len() + self.seen_values - self.needed_to_see_values);
    }

    pub fn set_coord(&mut self, coord: &Coord) { self.coord = (*coord).clone() }

    pub fn value(&self, eval: &mut Evaluator, previous_val: &Value) -> Value {
        match &self.otype {
            BINARY_OP(tok) => { lib::binary_op(eval, tok) }
            OPTIONAL_OP => { OPTIONVAL(Some(Box::from(eval.val_queue.pop_back().unwrap()))) }
            LAZY_LOGIC_OP(tok) => {
                match lib::lazy_logic_op(eval, &tok) {
                    Ok(value) => value,
                    Err(value) => return value,
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
            SCOPE_DURATION_COUNTDOWN_OP(_) => {
                eval.env.destroy_scope();
                previous_val.to_owned()
            }
            RETURN_CLEANUP => {
                let return_val = eval.val_queue.pop_back().unwrap_or(previous_val.to_owned());
                while let Some(op) = eval.op_queue.pop_back() {
                    op.flush(eval);
                    if let SCOPE_DURATION_COUNTDOWN_OP(_) = op.otype { break; }
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
            PULL_OP(tok) => { lib::pull_op(eval, tok) }
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(amount_of_passed_args, tok) => {
                match lib::bind_application_args_to_params_op(eval, amount_of_passed_args, tok) {
                    Ok(value) => value,
                    Err(value) => return value,
                }
            }
            ITERATIVE_PARAM_BINDER(past_iterations, iterand, body) => {
                match lib::iterative_param_binder_op(eval, past_iterations, iterand, body) {
                    Ok(value) => value,
                    Err(value) => return value,
                }
            }
            TI_REBINDER_OP => {
                let ti = eval.val_queue.pop_back().unwrap();
                eval.env.write_binding(&"ti".to_string(), &ti);
                NOTAVAL
            }
            AT_APPLICABLE_RESOLVER_OP => { lib::at_applicable_resolver_op(eval) }
            ASSOC_PUSHER_OP => { lib::assoc_pusher_op(eval) }
        }
    }
}