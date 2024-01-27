use std::collections::VecDeque;
use std::ops::Deref;

use log::{error, info};

use operation::Operation;
use operation::OperationType::*;

use crate::environment::Environment;
use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::evaluator::OperationStatus::*;
use crate::lexer::{Coord, Token};
use crate::lexer::TokenType::*;
use crate::parser::{AssociationState, Expression};

mod tests;
mod lib;
mod operation;

pub struct Evaluator<'a> {
    exp_queue: &'a mut VecDeque<Expression>,
    op_queue: VecDeque<Operation>,
    val_queue: VecDeque<Value>,
    scribe: &'a mut ErrorScribe,
    env: &'a mut Environment,
}

#[derive(PartialEq)]
enum OperationStatus {
    NOOP,
    WAITING,
    READY,
}


impl<'a> Evaluator<'a> {
    pub fn from_parser(exprs: &'a mut VecDeque<Expression>, scribe: &'a mut ErrorScribe, env: &'a mut Environment) -> Evaluator<'a> {
        Evaluator {
            exp_queue: exprs,
            op_queue: Default::default(),
            val_queue: Default::default(),
            scribe,
            env,
        }
    }

    fn op_status(&self) -> OperationStatus {
        if let Some(op) = self.op_queue.back() {
            if op.seen_values == op.needed_values { READY } else { WAITING }
        } else { NOOP }
    }

    fn error(&mut self, etype: ErrorType) -> Value {
        self.scribe.annotate_error(Error::on_coord(
            &self.env.coord, etype));
        ERRVAL
    }

    fn inc_available_values(&mut self) {
        self.op_queue.back_mut().unwrap().seen_values += 1;
    }

    fn read_var(&mut self, varname: &String) -> Value { self.env.read(varname, self.scribe).clone() }

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
    fn eolprint(&mut self, tok: &Token) -> Value {
        let line = &tok.coord.row;
        let start_new_line = self.env.last_print_line != *line;
        self.env.last_print_line = *line;
        let to_print = format!("[{}:{}]", self.env.curr_scope().entry_point, line);
        if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
        STRINGVAL(to_print)
    }

    pub fn value(&mut self) -> Value {
        // if you insert directly to the evaluator you have to be careful about reversing the order of the inserted items.
        // pushing front to these queries and merging them with the evaluator at the end of the iteration is simpler
        // to reason about
        let mut aux_op_queue = VecDeque::new();
        let mut aux_exp_queue = VecDeque::new();
        let mut ret = NOTAVAL;
        while let Some(expr) = self.exp_queue.pop_back() {
            ret = match expr {
                Expression::VAR_RAW(_, varname) => { self.env.read(&varname, self.scribe).clone() }
                Expression::ARGS(_) => { self.error(ErrorType::EVAL_UNEXPECTED_EXPRESSION) }
                Expression::APPLICABLE_EXPR { params: arg, body } => {
                    LAMBDAVAL { params: arg.clone(), body: body.clone() }
                }
                Expression::NOTANEXPR => { self.error(ErrorType::EVAL_INVALID_EXPR) }
                Expression::VALUE_WRAPPER(val) => val.deref().clone(),
                Expression::OPTION_EXPR(ex) => {
                    if ex.type_equals(&Expression::UNDERSCORE_EXPR(Coord::new())) { OPTIONVAL(None) } else {
                        aux_exp_queue.push_front(*ex);
                        aux_op_queue.push_front(Operation::from_type(OPTIONAL_OP));
                        NOTAVAL
                    }
                }
                Expression::SET_DECLARATION_EXPR { input_type, items } => {
                    lib::desugar_association_declaration(AssociationState::SET, input_type, items,
                                                         self, &mut aux_exp_queue, &mut aux_op_queue)
                }
                Expression::LIST_DECLARATION_EXPR { input_type, items } => {
                    lib::desugar_association_declaration(AssociationState::LIST, input_type, items,
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
                            if exprs.len() != 2 { return ERRVAL; }
                            aux_exp_queue.push_front(*obj);
                            for ex in exprs { aux_exp_queue.push_front(*ex.to_owned()); }
                            aux_op_queue.push_front(Operation::from_type(ASSOC_PUSHER_OP));
                            NOTAVAL
                        } else { ERRVAL }
                    }
                }
                Expression::APPLIED_EXPR { arg, op, body } => {
                    let mut args_size = 1;
                    if let Expression::ARGS(exprs) = arg.deref() {
                        args_size = exprs.len();
                        for ex in exprs { aux_exp_queue.push_front(*ex.to_owned()); }
                    } else { aux_exp_queue.push_front(*arg) }
                    aux_exp_queue.push_front(*body);

                    aux_op_queue.push_front(Operation::from_type(APPLICATION_OP(op, args_size)));
                    aux_op_queue.push_front(Operation::from_type(APPLICATION_CLEANUP));
                    NOTAVAL
                }
                Expression::PULL_EXPR { source, op, key } => {
                    aux_exp_queue.push_front(*source);
                    aux_exp_queue.push_front(*key);
                    aux_op_queue.push_front(Operation::from_type(PULL_OP(op)));
                    NOTAVAL
                }
                Expression::ASSOCIATION_EXPR(pairs) => {
                    aux_op_queue.push_front(Operation::from_type(
                        ASSOC_GROWER_OP(ValueMap::new(), pairs.len(), true)));
                    for (k, v) in pairs {
                        aux_exp_queue.push_front(*k.clone());
                        aux_exp_queue.push_front(*v.clone());
                    }
                    NOTAVAL
                }
                Expression::RETURN_EXPR(expr) => {
                    aux_exp_queue.push_front(*expr);
                    aux_op_queue.push_front(Operation::from_type(RETURN_CLEANUP));
                    NOTAVAL
                }
                Expression::BLOCK(exprs) => {
                    self.env.create_scope();
                    for ex in &exprs { aux_exp_queue.push_front(*ex.clone()); }
                    aux_op_queue.push_front(Operation::from_type(SCOPE_CLEANUP_OP(exprs.len())));
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
                        EOLPRINT => { self.eolprint(&value) }
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
                    aux_exp_queue.push_back(*expr);
                    aux_op_queue.push_back(Operation::from_type(UNARY_OP(op)));
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
                _ => {
                    dbg!("TODO");
                    self.error(ErrorType::GENERICERROR)
                }
            };
            // append also clears the aux queues for the next iteration.
            self.exp_queue.append(&mut aux_exp_queue);
            self.op_queue.append(&mut aux_op_queue);

            if self.op_status() != WAITING { continue; }
            if ret.type_equals(&NOTAVAL) { continue; }
            self.val_queue.push_back(ret.clone());
            self.inc_available_values();
            self.hook_for_waiting_operations();
            if self.op_status() == READY {
                let op = self.op_queue.pop_back().unwrap();
                let val = op.value(
                    &mut self.val_queue,
                    &mut self.op_queue,
                    &mut self.exp_queue,
                    self.env,
                );
                self.exp_queue.push_back(Expression::VALUE_WRAPPER(Box::from(val)));
            }
        }
        if !self.op_queue.is_empty() {
            error!("*** UNCONSUMED OPS! *** {:?}", &self.op_queue);
        }
        if !self.exp_queue.is_empty() {
            error!("*** UNCONSUMED EXPS! *** {:?}", &self.exp_queue);
        }
        if !self.val_queue.is_empty() {
            error!("*** UNCONSUMED VALS! *** {:?}", &self.val_queue);
        }
        if self.op_queue.len() + self.val_queue.len() + self.exp_queue.len() == 0 {
            info!("*** EVERYTHING WAS REGULARLY CONSUMED ***")
        }
        ret.clone()
    }

    fn hook_for_waiting_operations(&mut self) {
        if self.op_status() != WAITING { return; }
        // avoid saving memory of values that don't get used in a scope
        if let Some(back) = self.op_queue.back_mut() {
            if let SCOPE_CLEANUP_OP(_) = back.otype {
                self.val_queue.pop_back();
            }
        }
    }
}
