use std::collections::VecDeque;
use std::ops::Deref;

use log::error;

use crate::environment::Environment;
use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::*;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::evaluator::OperationStatus::*;
use crate::evaluator::OperationType::*;
use crate::lexer::{Coord, Token};
use crate::lexer::TokenType::*;
use crate::parser::Expression;

mod tests;
mod lib;

#[derive(Debug)]
#[allow(non_camel_case_types)]
enum OperationType {
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
    APPLICATION_CLEANUP(usize),
}

#[derive(Debug)]
struct Operation {
    needed_values: usize,
    seen_values: usize,
    otype: OperationType,
}

impl Operation {
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
    ) -> Value {
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
            APPLICATION_CLEANUP(_) => {
                if let LAMBDAVAL { params, body } = vals.pop_back().unwrap() {
                    exprs.push_back(*body);
                };
                NOTAVAL
            }
        }
    }
}

impl Operation {
    fn from_type(otype: OperationType) -> Operation {
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
            APPLICATION_CLEANUP(_) => 1,
        };

        Operation {
            seen_values: 0,
            otype,
            needed_values,
        }
    }
}

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

    pub fn value(&mut self) -> Value {
        env_logger::init();
        let mut ret = NOTAVAL;
        dbg!(&self.exp_queue);
        while let Some(expr) = self.exp_queue.pop_back() {
            ret = match expr {
                Expression::VAR_RAW(_, varname) => { self.env.read(&varname, self.scribe).clone() }
                Expression::ARGS(_) => { self.error(ErrorType::EVAL_UNEXPECTED_EXPRESSION) }
                Expression::APPLICABLE_EXPR { params: arg, body } => { LAMBDAVAL { params: arg.clone(), body: body.clone() } }
                Expression::NOTANEXPR => { self.error(ErrorType::EVAL_INVALID_EXPR) }
                Expression::VALUE_WRAPPER(val) => val.deref().clone(),
                Expression::OPTION_EXPR(ex) => {
                    if ex.type_equals(&Expression::UNDERSCORE_EXPR(Coord::new())) { OPTIONVAL(None) } else {
                        self.exp_queue.push_back(*ex);
                        self.op_queue.push_back(Operation::from_type(OPTIONAL_OP));
                        NOTAVAL
                    }
                }
//                 Expression::LIST_DECLARATION_EXPR { range, items } => {
//                     lib::eval_association_declaration(AssociationState::LIST, range, true, items, env, scribe)
//                 }
//                 Expression::SET_DECLARATION_EXPR { range, items } => {
//                     lib::eval_association_declaration(AssociationState::SET, range, true, items, env, scribe)
//                 }
//
//                 Expression::PUSH_EXPR { obj, args } => {
//                     // desugaring var << |k, v| into
//                     // var = var << |k, v|
//                     if let Expression::VAR_RAW(_, varname) = obj.deref() {
//                         return eval_expr(
//                             &Expression::VAR_ASSIGN {
//                                 varname: varname.clone(),
//                                 op: Token::new(ASSIGN, obj.coord().row, obj.coord().column),
//                                 varval: Box::new(Expression::PUSH_EXPR {
//                                     obj: Box::new(Expression::VALUE_WRAPPER(Box::new((*env.read(varname, scribe)).clone()))),
//                                     args: args.clone(),
//                                 }),
//                             }, env, scribe);
//                     }
//                     lib::eval_push(obj, args, env, scribe)
//                 }
//                 Expression::PULL_EXPR { source, op, key: field } => {
//                     let field = eval_expr(field, env, scribe);
//                     let source = eval_expr(source, env, scribe);
//                     lib::eval_pull(field, op, source, env, scribe)
//                 }
//                 Expression::APPLIED_EXPR { arg, op, body } => {
//                     lib::eval_application(arg, op, body, env, scribe)
//                 }
                Expression::APPLIED_EXPR { arg, op, body } => {
                    let mut args_size = 1;
                    self.exp_queue.push_back(*body);
                    if let Expression::ARGS(exprs) = arg.deref() {
                        args_size = exprs.len();
                        for ex in exprs.iter().rev() {
                            self.exp_queue.push_back(*ex.to_owned());
                        }
                    } else { self.exp_queue.push_back(*arg) }
                    self.op_queue.push_back(Operation::from_type(APPLICATION_CLEANUP(args_size + 1)));
                    self.op_queue.push_back(Operation::from_type(APPLICATION_OP(op, args_size)));
                    NOTAVAL
                }
                Expression::PULL_EXPR { source, op, key } => {
                    self.exp_queue.push_back(*key);
                    self.exp_queue.push_back(*source);
                    self.op_queue.push_back(Operation::from_type(PULL_OP(op)));
                    NOTAVAL
                }
                Expression::ASSOCIATION_EXPR(pairs) => { lib::eval_association(self, pairs, true) }
                Expression::RETURN_EXPR(expr) => {
                    self.exp_queue.push_back(*expr);
                    self.op_queue.push_back(Operation::from_type(RETURN_CLEANUP));
                    NOTAVAL
                }
                Expression::BLOCK(exprs) => {
                    self.env.create_scope();
                    self.op_queue.push_back(Operation::from_type(SCOPE_CLEANUP_OP(exprs.len())));
                    for ex in exprs.iter().rev() {
                        self.exp_queue.push_back(*ex.clone());
                    }
                    NOTAVAL
                }
                Expression::GROUPING(expr) => {
                    self.exp_queue.push_back(*expr);
                    NOTAVAL
                }
                Expression::VAR_ASSIGN { varname, op, varval } => {
                    self.exp_queue.push_back(*varval);
                    self.op_queue.push_back(Operation::from_type(VARASSIGN_OP(varname, op)));
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
//
                Expression::UNARY { op, expr } => {
                    self.exp_queue.push_back(*expr);
                    self.op_queue.push_back(Operation::from_type(UNARY_OP(op)));
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
                    self.exp_queue.push_back(*lhs);
                    self.exp_queue.push_back(*rhs);
                    self.op_queue.push_back(Operation::from_type(LAZY_LOGIC_OP(op)));
                    NOTAVAL
                }
                Expression::BINARY { lhs, op, rhs } => {
                    self.exp_queue.push_back(*lhs);
                    self.exp_queue.push_back(*rhs);
                    self.op_queue.push_back(Operation::from_type(BINARY_OP(op)));
                    NOTAVAL
                }
                _ => {
                    dbg!("TODO");
                    self.error(ErrorType::GENERICERROR)
                }
            };
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
            error!("*** EVERYTHING WAS REGULARLY CONSUMED ***")
        }
        ret.clone()
    }
    fn inc_available_values(&mut self) {
        self.op_queue.back_mut().unwrap().seen_values += 1;
    }
    fn read_var(&mut self, varname: &String) -> Value { self.env.read(varname, self.scribe).clone() }

    fn hook_for_waiting_operations(&mut self) {
        if self.op_status() != WAITING { return; }
        // avoid saving memory of values that don't get used in a scope
        if let Some(back) = self.op_queue.back_mut() {
            if let SCOPE_CLEANUP_OP(_) = back.otype {
                self.val_queue.pop_back();
            }
        }
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
    fn eolprint(&mut self, tok: &Token) -> Value {
        let line = &tok.coord.row;
        let start_new_line = self.env.last_print_line != *line;
        self.env.last_print_line = *line;
        let to_print = format!("[{}:{}]", self.env.curr_scope().entry_point, line);
        if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
        STRINGVAL(to_print)
    }
}
//
//
//     pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, scribe: &mut ErrorScribe, env: &mut Environment, subscoping: bool) -> Value {
//         let mut result = NOTAVAL;
//         let mut subscoping = subscoping;
//         if let Some(ex) = exprs.last() {
//             subscoping = subscoping && ex.is_tail_call_optimizable()
//         }
//         if subscoping { env.create_scope(); }
//
//         for (expr) in exprs {
//             let eval = eval_expr(&expr, env, scribe);
//             match eval {
//                 //exit on first evaluation error.
//                 ERRVAL => {
//                     result = eval;
//                     break;
//                 }
//                 RETURNVAL(val) => {
//                     if subscoping { env.destroy_scope(); }
//                     return val.deref().clone();
//                 }
//                 _ => { result = eval; }
//             }
//         }
//         if subscoping { env.destroy_scope(); }
//         result
//     }
// }
