use std::collections::VecDeque;
use std::ops::Deref;

use log::{error, info};

use crate::environment::Environment;
use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::errors::ErrorType::EVAL_INVALID_PUSH;
use crate::evaluator::operation::Operation;
use crate::evaluator::operation::OperationType::*;
use crate::evaluator::OperationStatus::*;
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::{AssociationState, Expression};

mod tests;
mod lib;

mod operation;

pub struct Evaluator<'a> {
    pub exp_queue: &'a mut VecDeque<Expression>,
    pub op_queue: VecDeque<Operation>,
    pub val_queue: VecDeque<Value>,
    pub scribe: &'a mut ErrorScribe,
    pub env: &'a mut Environment,
}

#[derive(PartialEq)]
enum OperationStatus {
    NOOP,
    WAITING,
    READY,
}

impl<'a> Evaluator<'a> {
    pub fn new(
        exprs: &'a mut VecDeque<Expression>,
        scribe: &'a mut ErrorScribe,
        env: &'a mut Environment,
    ) -> Evaluator<'a> {
        Evaluator {
            exp_queue: exprs,
            op_queue: Default::default(),
            val_queue: Default::default(),
            scribe,
            env,
        }
    }

    pub fn read_var(&mut self, varname: &String) -> Value { self.env.read(varname, self.scribe).clone() }

    fn error(&mut self, etype: ErrorType) -> Value {
        self.scribe.annotate_error(Error::on_coord(
            &self.env.coord, etype));
        ERRVAL
    }

    fn update_waiting_op(&mut self, ret: &mut Value) {
        let op = self.op_queue.back_mut().unwrap();
        if op.needed_to_keep_values() > op.seen_values {
            self.val_queue.push_back(ret.clone());
        }
        op.seen_values += 1;
        if self.op_status() == READY {
            let op = self.op_queue.pop_back().unwrap();
            let val = op.value(self, &ret);
            self.exp_queue.push_back(Expression::VALUE_WRAPPER(Box::from(val)));
        }
    }

    fn op_status(&self) -> OperationStatus {
        if let Some(op) = self.op_queue.back() {
            if op.seen_values == op.needed_to_see_values { READY } else { WAITING }
        } else { NOOP }
    }

    fn replace_string_placeholders(&mut self, str: &String) -> String {
        let mut result = String::new();
        let mut varname = String::new();
        for ch in str.chars() {
            match ch {
                '{' => { varname = "_".to_string(); }
                '}' => {
                    varname.remove(0);
                    let val = self.read_var(&varname);
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

    /// produces the STRINGVAL [file.lgr:4]
    fn print_debug_brick(&mut self, tok: &Token) -> Value {
        let line = &tok.coord.row;
        let start_new_line = self.env.last_print_line != *line;
        self.env.last_print_line = *line;
        let to_print = format!("[{}:{}]", self.env.curr_scope().entry_point, line);
        if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
        STRINGVAL(to_print)
    }

    pub fn value(&mut self) -> Value {
        let _ = env_logger::try_init();
        // if you insert directly to the evaluator you have to be careful about reversing the order of the inserted items.
        // pushing front to these queries and merging them with the evaluator at the end of the iteration is simpler
        // to reason about
        let mut aux_op_queue = VecDeque::new();
        let mut aux_exp_queue = VecDeque::new();
        let mut ret = NOTAVAL;

        while let Some(expr) = self.exp_queue.pop_back() {
            // dbg!(&self.val_queue);
            // dbg!(&self.op_queue);
            // dbg!(&self.exp_queue);
            self.env.coord = expr.coord().clone();
            ret = match expr {
                Expression::PRINT_EXPR(expr, tag) => {
                    aux_exp_queue.push_front(*expr);
                    aux_op_queue.push_front(Operation::from_type(PRINT_OP(tag)));
                    NOTAVAL
                }
                Expression::VAR_RAW(_, varname) => { self.env.read(&varname, self.scribe).clone() }
                Expression::ARGS(_) => { self.error(ErrorType::EVAL_UNEXPECTED_EXPRESSION) }
                Expression::APPLICABLE_EXPR { params: arg, body } => {
                    LAMBDAVAL { params: arg.clone(), body: body.clone() }
                }
                Expression::NOTANEXPR => { self.error(ErrorType::EVAL_INVALID_EXPR) }
                Expression::VALUE_WRAPPER(val) => val.deref().clone(),
                Expression::OPTION_EXPR(ex) => {
                    if ex.type_equals(&Expression::UNDERSCORE_EXPR(Default::default())) { OPTIONVAL(None) } else {
                        aux_exp_queue.push_front(*ex);
                        aux_op_queue.push_front(Operation::from_type(OPTIONAL_OP));
                        NOTAVAL
                    }
                }
                Expression::SET_DECLARATION_EXPR { input_type, items, is_lazy } => {
                    lib::desugar_association_declaration(AssociationState::SET, input_type, items, is_lazy,
                                                         self, &mut aux_exp_queue, &mut aux_op_queue)
                }
                Expression::LIST_DECLARATION_EXPR { input_type, items, is_lazy } => {
                    lib::desugar_association_declaration(AssociationState::LIST, input_type, items, is_lazy,
                                                         self, &mut aux_exp_queue, &mut aux_op_queue)
                }
                Expression::PUSH_EXPR { obj, args } => {
                    if let Expression::VAR_RAW(_, varname) = obj.deref() {
                        // when obj being pushed into is a var, push needs to be first desugared from
                        // "var << |k,v|" to "var=var << |k,v|"
                        let obj = Box::new(Expression::VALUE_WRAPPER(
                            Box::new(self.read_var(varname).clone())));
                        self.exp_queue.push_back(Expression::VAR_ASSIGN {
                            varname: varname.clone(),
                            op: Token::new(ASSIGN, obj.coord().row, obj.coord().column),
                            varval: Box::new(Expression::PUSH_EXPR {
                                obj,
                                args: args.clone(),
                            }),
                        });
                        NOTAVAL
                    } else {
                        if let Expression::ARGS(exprs) = args.deref() {
                            if exprs.len() != 2 { return self.error(EVAL_INVALID_PUSH); }
                            aux_exp_queue.push_front(*obj);
                            for ex in exprs { aux_exp_queue.push_front(*ex.to_owned()); }
                            aux_op_queue.push_front(Operation::from_type(ASSOC_PUSHER_OP));
                            NOTAVAL
                        } else { return self.error(EVAL_INVALID_PUSH); }
                    }
                }
                Expression::APPLIED_EXPR { arg, op, body } => {
                    self.create_scope_lazily();
                    let mut args_size = 1;
                    if let Expression::ARGS(exprs) = arg.deref() {
                        args_size = exprs.len();
                        for ex in exprs { aux_exp_queue.push_front(*ex.to_owned()); }
                    } else { aux_exp_queue.push_front(*arg) }
                    aux_exp_queue.push_front(*body);

                    aux_op_queue.push_front(Operation::from_type(BIND_APPLICATION_ARGS_TO_PARAMS_OP(args_size, op.clone())));
                    if op.type_equals(&AT) {
                        aux_op_queue.push_front(Operation::from_type(AT_APPLICABLE_RESOLVER_OP));
                    }
                    NOTAVAL
                }
                Expression::PULL_EXPR { source, op, key } => {
                    aux_exp_queue.push_front(*source);
                    aux_exp_queue.push_front(*key);
                    aux_op_queue.push_front(Operation::from_type(PULL_OP(op)));
                    NOTAVAL
                }
                Expression::ASSOCIATION_EXPR(pairs, is_lazy) => {
                    if pairs.is_empty() { ASSOCIATIONVAL(ValueMap::new()) } else {
                        aux_op_queue.push_front(Operation::from_type(
                            ASSOC_GROWER_SETUPPER_OP(ValueMap::new(), pairs.len(), is_lazy)));
                        for (k, v) in pairs {
                            aux_exp_queue.push_front(*k.clone());
                            aux_exp_queue.push_front(*v.clone());
                        }
                        NOTAVAL
                    }
                }
                Expression::RETURN_EXPR(expr) => {
                    aux_exp_queue.push_front(*expr);
                    aux_op_queue.push_front(Operation::from_type(RETURN_CLEANUP));
                    NOTAVAL
                }
                Expression::BLOCK(exprs) => {
                    for ex in &exprs { aux_exp_queue.push_front(*ex.clone()); }
                    self.create_scope_lazily();
                    self.add_scope_closure_lazily(exprs.len());
                    NOTAVAL
                }
                Expression::GROUPING(expr) => {
                    aux_exp_queue.push_front(*expr);
                    NOTAVAL
                }
                Expression::VAR_ASSIGN { varname, op, varval } => {
                    aux_exp_queue.push_front(*varval);
                    aux_op_queue.push_front(Operation::from_type(VARASSIGN_OP(varname, op)));
                    NOTAVAL
                }
                Expression::UNDERSCORE_EXPR(_) => UNDERSCOREVAL,
                Expression::LITERAL(value) => {
                    match &value.ttype {
                        EOLPRINT => { self.print_debug_brick(&value) }
                        FALSE => BOOLEANVAL(false),
                        TRUE => BOOLEANVAL(true),
                        STRING(str) => STRINGVAL(self.replace_string_placeholders(str)),
                        INTEGER(int) => INTEGERVAL(*int),
                        FLOAT(flt) => FLOATVAL(*flt),
                        IT => { self.read_var(&"it".to_string()) }
                        TI => { self.read_var(&"ti".to_string()) }
                        IDX => { self.read_var(&"idx".to_string()) }
                        _ => { self.error(ErrorType::EVAL_INVALID_LITERAL) }
                    }
                }
                Expression::UNARY { op, expr } => {
                    aux_exp_queue.push_front(*expr);
                    aux_op_queue.push_front(Operation::from_type(UNARY_OP(op)));
                    NOTAVAL
                }
//                     if op.ttype == BANGBANG {
//                         return match expr.deref() {
//                             Expression::ASSOCIATION_EXPR(pairs) => { lib::eval_association(&pairs, false, env, scribe) }
//                             Expression::LIST_DECLARATION_EXPR { range, items } => {
//                                 lib::eval_association_declaration(AssociationState::LIST, range, false, items, env, scribe)
//                             }
//                             Expression::SET_DECLARATION_EXPR { range, items } => {
//                                 lib::eval_association_declaration(AssociationState::SET, range, false, items, env, scribe)
//                             }
//                             _ => {
//                                 scribe.annotate_error(Error::on_coord(&env.coord,
//                                                                       ErrorType::EVAL_INVALID_OP(BANGBANG, vec![continue;])));
//                                 return ERRVAL;

                Expression::LOGIC { lhs, op, rhs } => {
                    aux_exp_queue.push_front(*lhs);
                    aux_exp_queue.push_front(*rhs);
                    aux_op_queue.push_front(Operation::from_type(LAZY_LOGIC_OP(op)));
                    NOTAVAL
                }
                Expression::BINARY { lhs, op, rhs } => {
                    aux_exp_queue.push_front(*lhs);
                    aux_exp_queue.push_front(*rhs);
                    aux_op_queue.push_front(Operation::from_type(BINARY_OP(op)));
                    NOTAVAL
                }
            };

            for op in &mut aux_op_queue { op.set_coord(&self.env.coord); }
            // append also clears the aux queues for the next iteration.
            self.exp_queue.append(&mut aux_exp_queue);

            self.op_queue.append(&mut aux_op_queue);

            if ret.type_equals(&NOTAVAL) { continue; }
            if self.op_status() != WAITING { continue; }
            if ret.type_equals(&ERRVAL) {
                error!("\nlast val: {:?}\nlast op: {:?}\nlast exp: {:?}\n",
                    &self.val_queue.back(),
                    &self.op_queue.back(),
                    &self.exp_queue.back());
                self.scribe.enact_termination_policy();
            }

            self.update_waiting_op(&mut ret);
        }
        self.was_evaluation_consistent();
        ret
    }

    fn is_current_op_scope_closure(&self) -> Option<usize> {
        if let Some(op) = self.op_queue.back() {
            if let SCOPE_CLOSURE_OP(n) = op.otype { return Some(n); }
        }
        None
    }

    /// if the current operation is a scope closure, and we're about to add another one, we can instead collapse them.
    fn add_scope_closure_lazily(&mut self, block_size: usize) {
        if self.is_current_op_scope_closure().is_some() {
            self.op_queue.pop_back();
        }
        self.op_queue.push_back(Operation::from_type(SCOPE_CLOSURE_OP(block_size)));
    }

    /// if the last instruction of a block needs to create a scope, it can instead just reuse the current block
    /// as long as this isn't the main scope.
    fn create_scope_lazily(&mut self) {
        let mut recycle = false;
        if self.env.scopes.len() == 1 { return self.env.create_or_reuse_scope(false); }
        if let Some(op) = self.op_queue.back() {
            match op.otype {
                SCOPE_CLOSURE_OP(n) => { recycle = op.seen_values + 1 == n; }
                AT_APPLICABLE_RESOLVER_OP => { recycle = true; }
                _ => {}
            }
            self.env.create_or_reuse_scope(recycle);
        }
    }
}
