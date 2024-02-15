use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::errors::ErrorType;
use crate::evaluator::Evaluator;
use crate::evaluator::operation::OperationType::*;
use crate::lexer::{Coord, Token};
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
    SCOPE_CLOSURE_OP(usize),
    RETURN_CLEANUP,
    ASSOC_GROWER_SETUPPER_OP(ValueMap, usize, bool),
    ASSOC_GROWER_RESOLVER_OP(ValueMap, usize, bool),
    PULL_OP(Token),
    BIND_APPLICATION_ARGS_TO_PARAMS_OP(usize, Token),
    AT_APPLICABLE_RESOLVER_OP,
    NATIVE_FN_CALLER_OP(usize, String),
    ASSOC_PUSHER_OP,
    ITERATIVE_PARAM_BINDER(usize, Box<Value>, Box<Expression>),
    TI_REBINDER_OP,
    PRINT_OP(Option<String>),
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
            SCOPE_CLOSURE_OP(_) => 0,
            _ => self.needed_to_see_values,
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
            SCOPE_CLOSURE_OP(n) => *n,
            RETURN_CLEANUP => 1,
            ASSOC_GROWER_SETUPPER_OP(_, _, _) => 1,
            ASSOC_GROWER_RESOLVER_OP(_, _, _) => 1,
            PULL_OP(_) => 2,
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(n, _) => *n,
            AT_APPLICABLE_RESOLVER_OP => 1,
            NATIVE_FN_CALLER_OP(n, _) => *n,
            ASSOC_PUSHER_OP => 3,
            ITERATIVE_PARAM_BINDER(_, _, _) => 1,
            TI_REBINDER_OP => 1,
            PRINT_OP(_) => 1,
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
    pub fn flush(&self, eval: &mut Evaluator) {
        // the +1 is counting the operation that flush() was called from.
        // trim values calculated for its future execution.
        eval.val_queue
            .truncate(1 + eval.val_queue.len() - self.needed_to_keep_values());
        // trim the expressions that would've calculated the rest of the needed values.
        eval.exp_queue
            .truncate(1 + eval.exp_queue.len() + self.seen_values - self.needed_to_see_values);
    }

    pub fn update_coord(&mut self, coord: &Coord) {
        self.coord = (*coord).clone()
    }

    pub fn value(&self, eval: &mut Evaluator, previous_val: &Value) -> Value {
        match &self.otype {
            PRINT_OP(tag) => lib::print_op(eval, tag),
            BINARY_OP(tok) => lib::binary_op(eval, tok),
            OPTIONAL_OP => OPTIONVAL(Some(Box::from(eval.val_queue.pop_back().unwrap()))),
            LAZY_LOGIC_OP(tok) => lib::lazy_logic_op(eval, &tok),
            LOGIC_OP(tok) => lib::logic_op(eval, &tok),
            UNARY_OP(tok) => lib::unary_op(eval, tok),
            VARASSIGN_OP(varname, op) => {
                let written_val = eval.env
                    .write(varname, &eval.val_queue.pop_back().unwrap(), op);
                if written_val == ERRVAL {
                    eval.error(ErrorType::EVAL_PROTECTED_VARIABLE(varname.clone()))
                } else { written_val }
            }
            SCOPE_CLOSURE_OP(_) => lib::scope_closure_op(eval, previous_val),
            RETURN_CLEANUP => lib::return_cleanup_op(eval),
            ASSOC_GROWER_SETUPPER_OP(map, n, is_lazy) => {
                lib::assoc_grower_setupper_op(eval, previous_val, map, n, is_lazy)
            }
            ASSOC_GROWER_RESOLVER_OP(map, n, is_lazy) => {
                lib::assoc_grower_resolver_op(eval, map, n, is_lazy)
            }
            PULL_OP(tok) => lib::pull_op(eval, tok),
            BIND_APPLICATION_ARGS_TO_PARAMS_OP(amount_of_passed_args, tok) => {
                lib::bind_application_args_to_params_op(eval, amount_of_passed_args, tok)
            }
            ITERATIVE_PARAM_BINDER(past_iterations, iterand, body) => {
                lib::iterative_param_binder_op(eval, past_iterations, iterand, body)
            }
            TI_REBINDER_OP => lib::ti_rebinder_op(eval),
            AT_APPLICABLE_RESOLVER_OP => lib::at_applicable_resolver_op(eval),
            NATIVE_FN_CALLER_OP(_, str) => lib::native_fn_caller_op(eval, str),
            ASSOC_PUSHER_OP => lib::assoc_pusher_op(eval),
        }
    }
}
