// pub fn eval_application(arg: &Box<Expression>,
//                         op: &Token,
//                         body: &Box<Expression>,
//                         env: &mut Environment,
//                         scribe: &mut ErrorScribe) -> Value {
//     let args = eval_application_args(arg, env, scribe);
//     let ret;
//     if op.ttype == ATAT {
//         env.create_scope();
//         ret = eval_iterated_application(args.get(0).unwrap().clone(), body, env, scribe)
//     } else {
//         let body_value = eval_expr(body, env, scribe);
//         env.create_scope();
//         if let Expression::ARGS(_) = arg.deref() {
//             ret = eval_parametrized_application(&args, &body_value, env, scribe);
//         } else {
//             ret = eval_parametrized_application(&args, &body_value, env, scribe);
//         }
//     }
//     env.destroy_scope();
//     return ret;
// }
// fn eval_iterated_application(
//     arg: Value,
//     body: &Box<Expression>,
//     env: &mut Environment,
//     scribe: &mut ErrorScribe,
// ) -> Value {
//     let mut ret = ERRVAL;
//     return if let ASSOCIATIONVAL(map) = arg {
//         for ((it, ti), idx) in map.iter().zip(0..) {
//             let unlazy_ti;
//             if let LAZYVAL(ex) = ti.deref() { unlazy_ti = eval_expr(ex, env, scribe); } else { unlazy_ti = ti.deref().clone() }
//             env.write_binding(&String::from("it"), &it);
//             env.write_binding(&String::from("ti"), &unlazy_ti);
//             env.write_binding(&String::from("idx"), &INTEGERVAL(idx));
//             ret = eval_expr(body, env, scribe);
//         }
//         ret
//     } else if let STRINGVAL(str) = arg {
//         for (it, idx) in str.chars().zip(0..) {
//             env.write_binding(&String::from("it"), &STRINGVAL(it.to_string()));
//             env.write_binding(&String::from("idx"), &INTEGERVAL(idx));
//             ret = eval_expr(body, env, scribe);
//         }
//         ret
//     } else {
//         scribe.annotate_error(Error::on_coord(&env.coord,
//                                               ErrorType::EVAL_ITER_APPL_ON_NONITER(arg)));
//         ERRVAL
//     }
// }
//
//
//

use std::collections::VecDeque;
use std::ops::Deref;

use crate::environment::value::{Value, ValueMap};
use crate::environment::value::Value::NOTAVAL;
use crate::errors::ErrorType::EVAL_INVALID_RANGE;
use crate::evaluator::Evaluator;
use crate::evaluator::operation::Operation;
use crate::evaluator::operation::OperationType::ASSOC_GROWER_OP;
use crate::lexer::{Token, TokenType};
use crate::parser::{AssociationState, Expression, InputType};

fn build_items_vec(
    input_type: InputType,
    items: Vec<Box<Expression>>,
) -> Option<Vec<Expression>> {
    if input_type != InputType::RANGE {
        return Some(items.iter()
            .map(|it| it.deref().to_owned())
            .collect());
    }
    if items.len() != 2 { return None; }

    let mut int_range: [i64; 2] = [0, 0];
    for (it, i) in items.iter().zip(0..) {
        let int = if let Expression::LITERAL(tok) = it.deref() {
            if let TokenType::INTEGER(s) = tok.ttype { s } else { return None; }
        } else { return None; };
        int_range[i] = int;
    }

    let ret;
    if int_range[0] <= int_range[1] {
        ret = (int_range[0]..int_range[1])
            .map(|i| Expression::LITERAL(Token::new(TokenType::INTEGER(i), 0, 0)))
            .collect::<Vec<Expression>>();
    } else {
        ret = (int_range[1] + 1..int_range[0] + 1).rev()
            .map(|i| Expression::LITERAL(Token::new(TokenType::INTEGER(i), 0, 0)))
            .collect::<Vec<Expression>>();
    }
    return Some(ret);
}

pub fn desugar_association_declaration(
    association_state: AssociationState,
    input_type: InputType,
    items: Vec<Box<Expression>>,
    eval: &mut Evaluator,
    exp_queue: &mut VecDeque<Expression>,
    op_queue: &mut VecDeque<Operation>,
) -> Value {
    let items = if let Some(it) = build_items_vec(input_type, items)
    { it } else { return eval.error(EVAL_INVALID_RANGE); };
    op_queue.push_front(Operation::from_type(
        ASSOC_GROWER_OP(ValueMap::new(), items.len(), true)
    ));
    for (it, i) in items.iter().zip(0..) {
        if association_state == AssociationState::LIST {
            exp_queue.push_front(Expression::LITERAL(Token::new(TokenType::INTEGER(i), 0, 0)));
            exp_queue.push_front(it.to_owned());
        } else {
            exp_queue.push_front(it.to_owned());
            exp_queue.push_front(Expression::LITERAL(Token::new(TokenType::TRUE, 0, 0)));
        }
    }
    NOTAVAL
}