use std::ops::Deref;

use crate::environment::{Environment, print_eol};
use crate::environment::value::Value::*;
use crate::environment::value::Value;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::Token;
use crate::lexer::TokenType::*;
use crate::parser::{AssociationState, Expression};
use crate::parser::Expression::UNDERSCORE_EXPR;

mod tests;
mod lib;

pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, es: &mut ErrorScribe, env: &mut Environment, subscoping: bool) -> Value {
    if subscoping { env.create_scope(); }
    let mut result = NOTAVAL;
    for expr in exprs {
        let eval = eval_expr(&expr, env, es);
        match eval {
            //exit on first evaluation error. careful about side effects.
            ERRVAL => {
                result = eval;
                break;
            }
            RETURNVAL(val) => {
                env.destroy_scope();
                return val.deref().clone();
            }
            _ => { result = eval; }
        }
    }
    env.destroy_scope();
    result
}


fn eval_expr(expr: &Expression, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
    if let Expression::VAR_RAW(varname) = expr {
        return env.read(varname, scribe).clone();

        // let val = env.read(varname, scribe).clone();
        // return if let LAMBDAVAL(ex) = val {
        //     eval_expr(ex.deref(), env, scribe)
        // } else { val };
    }

    match expr {
        Expression::VAR_RAW(_) | Expression::ARGS(_) | UNDERSCORE_EXPR => {
            dbg!(expr);
            scribe.annotate_error(Error::on_line(env.curr_line,
                                                 ErrorType::EVAL_UNEXPECTED_EXPRESSION));
            ERRVAL
        }
        Expression::APPLICABLE_EXPR { params: arg, body } => {
            LAMBDAVAL { params: arg.clone(), body: body.clone() }
        }
        Expression::NOTANEXPR => {
            scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_INVALID_EXPR));
            ERRVAL
        }

        Expression::VALUE_WRAPPER(val) => val.deref().clone(),
        Expression::OPTION_EXPR(ex) => {
            if ex.type_equals(&UNDERSCORE_EXPR) { OPTIONVAL(None) } else {
                OPTIONVAL(Some(Box::new(eval_expr(ex, env, scribe))))
            }
        }
        Expression::LIST_DECLARATION_EXPR { range, items } => {
            lib::eval_association_declaration(AssociationState::LIST, range, true, items, env, scribe)
        }
        Expression::SET_DECLARATION_EXPR { range, items } => {
            lib::eval_association_declaration(AssociationState::SET, range, true, items, env, scribe)
        }

        Expression::PUSH_EXPR { obj, args } => {
            // desugaring var << |k, v| into
            // var = var << |k, v|
            if let Expression::VAR_RAW(varname) = obj.deref() {
                return eval_expr(
                    &Expression::VAR_ASSIGN {
                        varname: varname.clone(),
                        op: Token::new(ASSIGN, env.curr_line),
                        varval: Box::new(Expression::PUSH_EXPR {
                            obj: Box::new(Expression::VALUE_WRAPPER(Box::new((*env.read(varname, scribe)).clone()))),
                            args: args.clone(),
                        }),
                    }, env, scribe);
            }
            lib::eval_push(obj, args, env, scribe)
        }
        Expression::PULL_EXPR { source, op, key: field } => {
            let field = eval_expr(field, env, scribe);
            let source = eval_expr(source, env, scribe);
            lib::eval_pull(field, op, source, env, scribe)
        }
        Expression::ASSOCIATION_EXPR(pairs) => { lib::eval_association(pairs, true, env, scribe) }
        Expression::RETURN_EXPR(expr) => { RETURNVAL(Box::new(eval_expr(expr, env, scribe))) }
        Expression::APPLIED_EXPR { arg, op, body } => {
            lib::eval_application(arg, op, body, env, scribe)
        }
        Expression::BLOCK(exprs) => {
            evaluate_expressions(exprs, scribe, env, true) }
        Expression::GROUPING(expr) => { eval_expr(expr, env, scribe) }
        Expression::VAR_ASSIGN { varname, op, varval } => {
            let val = eval_expr(varval, env, scribe);
            if val.type_equals(&ERRVAL) { return val; }
            env.write(varname, &val, op)
        }
        Expression::LITERAL(value) => {
            match &value.ttype {
                EOLPRINT => print_eol(env, &value.line),
                FALSE => BOOLEANVAL(false),
                TRUE => BOOLEANVAL(true),
                STRING(str) => STRINGVAL(lib::replace_string_placeholders(str, env, scribe)),
                INTEGER(int) => INTEGERVAL(*int),
                FLOAT(flt) => FLOATVAL(*flt),
                IT => { env.read(&"it".to_string(), scribe).clone() }
                TI => { env.read(&"ti".to_string(), scribe).clone() }
                IDX => { env.read(&"idx".to_string(), scribe).clone() }
                _ => {
                    scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_INVALID_LITERAL));
                    ERRVAL
                }
            }
        }

        Expression::UNARY { op, expr } => {
            if op.ttype == BANGBANG {
                return match expr.deref() {
                    Expression::ASSOCIATION_EXPR(pairs) => { lib::eval_association(&pairs, false, env, scribe) }
                    Expression::LIST_DECLARATION_EXPR { range, items } => {
                        lib::eval_association_declaration(AssociationState::LIST, range, false, items, env, scribe)
                    }
                    Expression::SET_DECLARATION_EXPR { range, items } => {
                        lib::eval_association_declaration(AssociationState::SET, range, false, items, env, scribe)
                    }
                    _ => {
                        scribe.annotate_error(Error::on_line(env.curr_line,
                                                             ErrorType::EVAL_INVALID_OP(BANGBANG, vec![NOTAVAL])));
                        return ERRVAL;
                    }
                };
            }

            let val = eval_expr(expr, env, scribe);
            let ret = match op.ttype {
                EXTRACT => { val.extract() }
                ASBOOL => { val.as_bool_val() }
                NOT => { val.not_it() }
                MINUS => { val.minus_it() }
                DOLLAR => {
                    val.print_it(op.line, env, None);
                    val.clone()
                }
                _ => {
                    ERRVAL
                }
            };
            if ret.type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(
                    env.curr_line, ErrorType::EVAL_INVALID_OP(
                        op.clone().ttype, vec![val.clone()])));
            }
            ret
        }

        Expression::LOGIC { lhs, op, rhs } => {
            let lval = eval_expr(lhs, env, scribe);
            if lval.as_bool_val().type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_NOT_BOOLEANABLE(lval)));
                return ERRVAL;
            }
            let ret = match (&op.ttype, lval.as_bool_val()) {
                (AND, BOOLEANVAL(bool)) => {
                    if !bool { return lval; } else { eval_expr(rhs, env, scribe) }
                }
                (OR, BOOLEANVAL(bool)) => {
                    if bool { return lval; } else { eval_expr(rhs, env, scribe) }
                }
                (XOR, BOOLEANVAL(bool)) => {
                    let ehrs = eval_expr(rhs, env, scribe);
                    if bool { ehrs.not_it() } else { ehrs }
                }
                _ => {
                    ERRVAL
                }
            };
            if ret.as_bool_val().type_equals(&ERRVAL) {
                scribe.annotate_error(
                    Error::on_line(env.curr_line, ErrorType::EVAL_NOT_BOOLEANABLE(ret.clone())));
                return ERRVAL;
            }
            ret
        }
        Expression::BINARY { lhs, op, rhs } => {
            let lval = eval_expr(lhs, env, scribe);
            if op.ttype == DOLLAR {
                lval.print_it(op.line, env, Some(rhs));
                return lval;
            }
            let rval = eval_expr(rhs, env, scribe);

            let ret = match op.ttype {
                MINUS => { lval.minus_them(&rval) }
                PLUS => { lval.plus_them(&rval) }
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
                _ => {
                    ERRVAL
                }
            };
            if ret.type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(
                    env.curr_line, ErrorType::EVAL_INVALID_OP(
                        op.clone().ttype, vec![lval.clone(), rval.clone()])));
            }
            ret
        }
    }
}