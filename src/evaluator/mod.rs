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

pub fn is_expr_applicable(expr: &Expression, env: &Environment, scribe: &mut ErrorScribe) -> bool {
    match expr {
        Expression::VAR_RAW(varname)
        => {
            env.try_read(varname).is_some_and(|v|v.type_equals(&LAMBDAVAL(Box::new(Expression::NOTANEXPR))))
        }

        Expression::LITERAL(value) => {
            [IT, TI, IDX].contains(&value.ttype) }
        Expression::APPLICABLE_EXPR { .. } => { true }

        Expression::APPLICATION_EXPR { .. } |
        Expression::VALUE_WRAPPER(_) |
        Expression::NOTANEXPR |
        Expression::VAR_ASSIGN { .. } |
        UNDERSCORE_EXPR => { false }

        Expression::OPTION_EXPR(expr) |
        Expression::RETURN_EXPR(expr) |
        Expression::UNARY { expr, .. } |
        Expression::GROUPING(expr)
        => { is_expr_applicable(expr, env, scribe) }

        Expression::BINARY { rhs, lhs, .. } |
        Expression::LOGIC { rhs, lhs, .. } |
        Expression::PULL_EXPR { source: rhs, key: lhs, .. }
        => { is_expr_applicable(rhs, env, scribe) || is_expr_applicable(lhs, env, scribe) }

        Expression::ARGS(exprs) |
        Expression::SET_DECLARATION_EXPR { items: exprs, .. } |
        Expression::LIST_DECLARATION_EXPR { items: exprs, .. } |
        Expression::BLOCK(exprs)
        => { exprs.iter().any(|expr| is_expr_applicable(expr, env, scribe)) }

        Expression::ASSOCIATION_EXPR(pairs)
        => { pairs.iter().any(|(k, v)|
            is_expr_applicable(k, env, scribe) || is_expr_applicable(v, env, scribe)) }

        Expression::PUSH_EXPR { obj, args }
        => { is_expr_applicable(obj, env, scribe) || is_expr_applicable(args, env, scribe) }
    }
}

pub fn evaluate_expressions(exprs: &Vec<Box<Expression>>, es: &mut ErrorScribe, env: &mut Environment, subscoping: bool, evaluand: bool) -> Value {
    if subscoping { env.create_scope(); }
    let mut result = NOTAVAL;
    for expr in exprs {
        let eval = eval_expr(&expr, env, es, evaluand);
        match eval {
            //exit on first evaluation error. careful about side effects.
            ERRVAL => {
                result = eval;
                break;
            }
            RETURNVAL(val) => {
                if subscoping { env.destroy_scope(); }
                return val.deref().clone();
            }
            _ => { result = eval; }
        }
    }
    if subscoping { env.destroy_scope(); }
    result
}


fn eval_expr(expr: &Expression, env: &mut Environment, scribe: &mut ErrorScribe, evaluand: bool) -> Value {
    if let Expression::VAR_RAW(varname) = expr {
        let val = env.read(varname, scribe).clone();
        return if let LAMBDAVAL(ex) = val {
            eval_expr(ex.deref(), env, scribe, evaluand)
        } else { val };
    }
    if !evaluand && is_expr_applicable(expr, env, scribe) { return LAMBDAVAL(Box::new(expr.clone())); }

    match expr {
        Expression::VAR_RAW(_) | Expression::ARGS(_) | UNDERSCORE_EXPR => {
            scribe.annotate_error(Error::on_line(env.curr_line,
                                                 ErrorType::EVAL_UNEXPECTED_EXPRESSION));
            ERRVAL
        }
        Expression::APPLICABLE_EXPR { .. } => {
            scribe.annotate_error(Error::on_line(env.curr_line,
                                                 ErrorType::EVAL_VAL_TO_NONIT_APPLICABLE));
            ERRVAL
        }
        Expression::NOTANEXPR => {
            scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_INVALID_EXPR));
            ERRVAL
        }

        Expression::VALUE_WRAPPER(val) => val.deref().clone(),
        Expression::OPTION_EXPR(ex) => {
            if ex.type_equals(&UNDERSCORE_EXPR) { OPTIONVAL(None) } else {
                OPTIONVAL(Some(Box::new(eval_expr(ex, env, scribe, evaluand))))
            }
        }
        Expression::LIST_DECLARATION_EXPR { range, items } => {
            lib::eval_association_declaration(AssociationState::LIST, range, true, items, env, scribe, evaluand)
        }
        Expression::SET_DECLARATION_EXPR { range, items } => {
            lib::eval_association_declaration(AssociationState::SET, range, true, items, env, scribe, evaluand)
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
                    }, env, scribe,
                    evaluand);
            }
            lib::eval_push(obj, args, env, scribe, evaluand)
        }
        Expression::PULL_EXPR { source, op, key: field } => {
            let field = eval_expr(field, env, scribe, evaluand);
            let source = eval_expr(source, env, scribe, evaluand);
            lib::eval_pull(field, op, source, env, scribe, evaluand)
        }
        Expression::ASSOCIATION_EXPR(pairs) => { lib::eval_association(pairs, true, env, scribe, evaluand) }
        Expression::RETURN_EXPR(expr) => { RETURNVAL(Box::new(eval_expr(expr, env, scribe, evaluand))) }
        Expression::APPLICATION_EXPR { arg, op, body } => {
            lib::eval_application(arg, op.clone(), body, env, scribe, evaluand)
        }
        Expression::BLOCK(exprs) => { evaluate_expressions(exprs, scribe, env, true, evaluand) }
        Expression::GROUPING(expr) => { eval_expr(expr, env, scribe, evaluand) }
        Expression::VAR_ASSIGN { varname, op, varval } => {
            let val = eval_expr(varval, env, scribe, evaluand);
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
                    Expression::ASSOCIATION_EXPR(pairs) => { lib::eval_association(&pairs, false, env, scribe, evaluand) }
                    Expression::LIST_DECLARATION_EXPR { range, items } => {
                        lib::eval_association_declaration(AssociationState::LIST, range, false, items, env, scribe, evaluand)
                    }
                    Expression::SET_DECLARATION_EXPR { range, items } => {
                        lib::eval_association_declaration(AssociationState::SET, range, false, items, env, scribe, evaluand)
                    }
                    _ => {
                        scribe.annotate_error(Error::on_line(env.curr_line,
                                                             ErrorType::EVAL_INVALID_OP(BANGBANG, vec![NOTAVAL])));
                        return ERRVAL;
                    }
                };
            }

            let val = eval_expr(expr, env, scribe, evaluand);
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
            let lval = eval_expr(lhs, env, scribe, evaluand);
            if lval.as_bool_val().type_equals(&ERRVAL) {
                scribe.annotate_error(Error::on_line(env.curr_line, ErrorType::EVAL_NOT_BOOLEANABLE(lval)));
                return ERRVAL;
            }
            let ret = match (&op.ttype, lval.as_bool_val()) {
                (AND, BOOLEANVAL(bool)) => {
                    if !bool { return lval; } else { eval_expr(rhs, env, scribe, evaluand) }
                }
                (OR, BOOLEANVAL(bool)) => {
                    if bool { return lval; } else { eval_expr(rhs, env, scribe, evaluand) }
                }
                (XOR, BOOLEANVAL(bool)) => {
                    let ehrs = eval_expr(rhs, env, scribe, evaluand);
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
            let lval = eval_expr(lhs, env, scribe, evaluand);
            if op.ttype == DOLLAR {
                lval.print_it(op.line, env, Some(rhs));
                return lval;
            }
            let rval = eval_expr(rhs, env, scribe, evaluand);

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