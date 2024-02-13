use std::collections::VecDeque;
use std::ops::Deref;

use log::error;

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
use crate::stdlib_modules::MODULE_NAMES;

mod lib;
mod tests;

pub(crate) mod operation;

pub struct Evaluator<'a> {
    pub exp_queue: &'a mut VecDeque<Expression>,
    pub op_queue: VecDeque<Operation>,
    pub val_queue: VecDeque<Value>,
    pub scribe: &'a mut ErrorScribe,
    pub env: &'a mut Environment,
    pub verbose: bool,
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
            verbose: false,
        }
    }

    pub fn read_var(&mut self, varname: &String) -> Value {
        self.env.read(varname, self.scribe).clone()
    }

    fn error(&mut self, etype: ErrorType) -> Value {
        self.scribe
            .annotate_error(Error::on_coord(&self.env.coord, etype));
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
            self.exp_queue
                .push_back(Expression::VALUE_WRAPPER(Box::from(val)));
        }
    }

    fn op_status(&self) -> OperationStatus {
        if let Some(op) = self.op_queue.back() {
            if op.seen_values == op.needed_to_see_values {
                READY
            } else {
                WAITING
            }
        } else {
            NOOP
        }
    }

    fn replace_string_placeholders(&mut self, str: &String) -> String {
        let mut result = String::new();
        let mut varname = String::new();
        for ch in str.chars() {
            match ch {
                '{' => {
                    varname = "_".to_string();
                }
                '}' => {
                    varname.remove(0);
                    let val = self.read_var(&varname);
                    result += &*val.to_string();
                    varname.clear();
                }
                _ => {
                    if varname.len() > 0 {
                        varname.push(ch);
                    } else {
                        result.push(ch);
                    }
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
        if start_new_line {
            print!("\n{}", to_print);
        } else {
            print!(" {} ", to_print);
        }
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
            self.env.coord = expr.coord().clone();
            ret = match expr {
                Expression::PRINT_EXPR(expr, tag) => {
                    aux_exp_queue.push_front(*expr);
                    aux_op_queue.push_front(Operation::from_type(PRINT_OP(tag)));
                    NOTAVAL
                }

                Expression::IMPORT_EXPR(str) => {
                    let module = MODULE_NAMES.get(str.as_str());
                    if module.is_none() { return self.error(ErrorType::EVAL_NO_SUCH_MODULE(str)); } else {
                        self.env.imported_modules.push(module.unwrap().clone());
                        continue;
                    }
                }

                Expression::VAR_RAW(_, varname) => self.env.read(&varname, self.scribe).clone(),

                Expression::ARGS(_) => self.error(ErrorType::EVAL_UNEXPECTED_EXPRESSION),

                Expression::APPLICABLE_EXPR { params: arg, body } => LAMBDAVAL {
                    params: arg.clone(),
                    body: body.clone(),
                },

                Expression::NOTANEXPR => self.error(ErrorType::EVAL_INVALID_EXPR),

                Expression::VALUE_WRAPPER(val) => val.deref().clone(),

                Expression::OPTION_EXPR(ex) => {
                    if ex.type_equals(&Expression::UNDERSCORE_EXPR(Default::default())) {
                        OPTIONVAL(None)
                    } else {
                        aux_exp_queue.push_front(*ex);
                        aux_op_queue.push_front(Operation::from_type(OPTIONAL_OP));
                        NOTAVAL
                    }
                }

                Expression::SET_DECLARATION_EXPR {
                    input_type,
                    items,
                    is_lazy,
                } => lib::desugar_association_declaration(
                    AssociationState::SET,
                    input_type,
                    items,
                    is_lazy,
                    self,
                    &mut aux_exp_queue,
                    &mut aux_op_queue,
                ),

                Expression::LIST_DECLARATION_EXPR {
                    input_type,
                    items,
                    is_lazy,
                } => lib::desugar_association_declaration(
                    AssociationState::LIST,
                    input_type,
                    items,
                    is_lazy,
                    self,
                    &mut aux_exp_queue,
                    &mut aux_op_queue,
                ),

                Expression::PUSH_EXPR { obj, args } => {
                    if let Expression::VAR_RAW(_, varname) = obj.deref() {
                        // when obj being pushed into is a var, push needs to be first desugared from
                        // "var << |k,v|" to "var=var << |k,v|"
                        let obj = Box::new(Expression::VALUE_WRAPPER(Box::new(
                            self.read_var(varname).clone(),
                        )));
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
                            if exprs.len() != 2 {
                                return self.error(EVAL_INVALID_PUSH);
                            }
                            aux_exp_queue.push_front(*obj);
                            for ex in exprs {
                                aux_exp_queue.push_front(*ex.to_owned());
                            }
                            aux_op_queue.push_front(Operation::from_type(ASSOC_PUSHER_OP));
                            NOTAVAL
                        } else {
                            return self.error(EVAL_INVALID_PUSH);
                        }
                    }
                }

                Expression::APPLIED_EXPR {
                    it_arg,
                    op,
                    mut body,
                    contour_args,
                } => {
                    self.create_scope_lazily();
                    // package the args and the operation to evaluate them.
                    let mut total_args_size = 1;
                    if let Some(args) = contour_args {
                        total_args_size += args.len();
                        for a in args {
                            aux_exp_queue.push_front(*a.to_owned());
                        }
                    }
                    aux_exp_queue.push_front(*it_arg);

                    // if application is builtin there's no fn body to queue, since the evaluation is with Rust.
                    // instead we queue up the args and said evaluation.
                    let mut evaluated_as_native = false;
                    if let Expression::VAR_RAW(_, str) = body.deref() {
                        if self.env.module_having(str).is_some() {
                            evaluated_as_native = true;
                            aux_op_queue.push_front(Operation::from_type(NATIVE_FN_CALLER_OP(total_args_size, str.clone())));
                        }
                    }
                    if !evaluated_as_native {
                        aux_op_queue.push_front(Operation::from_type(
                            BIND_APPLICATION_ARGS_TO_PARAMS_OP(total_args_size, op.clone()),
                        ));

                        if op.type_equals(&AT) {
                            body = Box::from(body.into_applicable());
                            aux_op_queue.push_front(Operation::from_type(AT_APPLICABLE_RESOLVER_OP));
                        }
                        aux_exp_queue.push_front(*body);
                    }
                    aux_op_queue.push_front(Operation::from_type(SCOPE_CLOSURE_OP(1)));
                    NOTAVAL
                }

                Expression::PULL_EXPR { source, op, key } => {
                    aux_exp_queue.push_front(*source);
                    aux_exp_queue.push_front(*key);
                    aux_op_queue.push_front(Operation::from_type(PULL_OP(op)));
                    NOTAVAL
                }

                Expression::ASSOCIATION_EXPR(pairs, is_lazy) => {
                    if pairs.is_empty() {
                        ASSOCIATIONVAL(ValueMap::new())
                    } else {
                        aux_op_queue.push_front(Operation::from_type(ASSOC_GROWER_SETUPPER_OP(
                            ValueMap::new(),
                            pairs.len(),
                            is_lazy,
                        )));
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
                    if exprs.is_empty() {return self.error(ErrorType::EVAL_EMPTY_BLOCK)}
                    for ex in &exprs {
                        aux_exp_queue.push_front(*ex.clone());
                    }
                    self.create_scope_lazily();
                    aux_op_queue.push_front(Operation::from_type(SCOPE_CLOSURE_OP(exprs.len())));
                    NOTAVAL
                }

                Expression::GROUPING(expr) => {
                    aux_exp_queue.push_front(*expr);
                    NOTAVAL
                }

                Expression::VAR_ASSIGN {
                    varname,
                    op,
                    varval,
                } => {
                    aux_exp_queue.push_front(*varval);
                    aux_op_queue.push_front(Operation::from_type(VARASSIGN_OP(varname, op)));
                    NOTAVAL
                }

                Expression::UNDERSCORE_EXPR(_) => UNDERSCOREVAL,

                Expression::LITERAL(value) => match &value.ttype {
                    EOLPRINT => self.print_debug_brick(&value),
                    FALSE => BOOLEANVAL(false),
                    TRUE => BOOLEANVAL(true),
                    STRING(str) => STRINGVAL(self.replace_string_placeholders(str)),
                    INTEGER(int) => INTEGERVAL(*int),
                    FLOAT(flt) => FLOATVAL(*flt),
                    IT => self.read_var(&"it".to_string()),
                    TI => self.read_var(&"ti".to_string()),
                    IDX => self.read_var(&"idx".to_string()),
                    _ => self.error(ErrorType::EVAL_INVALID_LITERAL),
                },

                Expression::UNARY { op, expr } => {
                    aux_exp_queue.push_front(*expr);
                    aux_op_queue.push_front(Operation::from_type(UNARY_OP(op)));
                    NOTAVAL
                }

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

            for op in &mut aux_op_queue {
                op.update_coord(&self.env.coord);
            }
            // append also clears the aux queues for the next iteration.
            self.exp_queue.append(&mut aux_exp_queue);

            self.op_queue.append(&mut aux_op_queue);

            if ret.type_equals(&NOTAVAL) {
                continue;
            }
            if self.op_status() != WAITING {
                continue;
            }
            if ret.type_equals(&ERRVAL) {
                error!(
                    "\nlast val: {:?}\nlast op: {:?}\nlast exp: {:?}\n",
                    &self.val_queue.back(),
                    &self.op_queue.back(),
                    &self.exp_queue.back()
                );
                self.scribe.enact_termination_policy();
            }

            self.update_waiting_op(&mut ret);
        }
        self.was_evaluation_consistent();
        ret
    }

    /// if a new scope is requested when we're about to exit from one, we can recycle the latter instead.
    fn create_scope_lazily(&mut self) {
        let mut can_recycle_current_scope_instead = false;
        if let Some(op) = self.op_queue.back() {
            if let SCOPE_CLOSURE_OP(_) = op.otype {
                if op.needed_to_see_values == 1 {
                    can_recycle_current_scope_instead = true;
                }
            }
        }
        if can_recycle_current_scope_instead {
            self.op_queue.pop_back();
        } else {
            self.env.create_scope();
        }
    }
}
