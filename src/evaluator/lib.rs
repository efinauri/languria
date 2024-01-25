// use std::ops::Deref;
//
// use crate::environment::Environment;
// use crate::environment::value::{Value, ValueMap};
// use crate::environment::value::Value::{ASSOCIATIONVAL, ERRVAL, INTEGERVAL, LAMBDAVAL, LAZYVAL, OPTIONVAL, STRINGVAL};
// use crate::errors::{Error, ErrorScribe, ErrorType};
// use crate::lexer::{Coord, Token, TokenType};
// use crate::lexer::TokenType::{ASSIGN, ATAT, PULL, PULLEXTRACT, PUSH};
// use crate::parser::{AssociationState, Expression, InputState};
// use crate::parser::Expression::UNDERSCORE_EXPR;
//
// pub fn eval_association(
//     pairs: &Vec<(Box<Expression>, Box<Expression>)>,
//     lazy: bool,
//     env: &mut Environment,
//     scribe: &mut ErrorScribe,
// ) -> Value {
//     let mut map = ValueMap::new();
//     // all code branching needs to be lazily evaluated, even the ones that aren't applicables
//     // (because of side effects)
//     for (k, v) in pairs {
//         let v = if lazy { LAZYVAL(v.clone()) } else { eval_expr(v, env, scribe) };
//         if k.type_equals(&UNDERSCORE_EXPR(Coord::new())) {
//             map.default = Some(Box::new(v));
//             continue;
//         }
//         map.insert(eval_expr(k, env, scribe), v);
//     }
//     ASSOCIATIONVAL(map)
// }
//
// pub fn eval_pull(field: Value, op: &Token, source: Value, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
//     return match source {
//         ASSOCIATIONVAL(map) => {
//             let mut query_expr = None;
//             let mut query_val = None;
//             if let Some(val) = map.get(&field) {
//                 if let LAZYVAL(ex) = val { query_expr = Some(ex); } else { query_val = Some(val); };
//             } else if let Some(val) = map.default {
//                 if let LAZYVAL(ex) = val.deref() { query_expr = Some(ex.clone()); } else { query_val = Some(*val); };
//             }
//             let val = match (query_expr, query_val) {
//                 (Some(ex), None) => { Some(Box::new(eval_expr(&ex, env, scribe))) }
//                 (None, Some(val)) => { Some(Box::new(val.clone())) }
//                 (None, None) => { None }
//                 (_, _) => { unreachable!() }
//             };
//             return match op.ttype {
//                 PULL => { OPTIONVAL(val) }
//                 PULLEXTRACT => { if let Some(v) = val { *v } else { ERRVAL } }
//                 _ => { unreachable!() }
//             };
//         }
//         _ => {
//             scribe.annotate_error(Error::on_coord(&env.coord,
//                                                   ErrorType::EVAL_INVALID_OP(PULL, vec![source])));
//             ERRVAL
//         }
//     };
// }
//
// pub fn eval_push(
//     obj: &Expression, args: &Expression, env: &mut Environment, scribe: &mut ErrorScribe,
// ) -> Value {
//     // the caller performs desugaring to ensure that obj is not a variable
//
//     let mut obj = eval_expr(obj, env, scribe);
//     let else_branch = vec![];
//     let exprs = if let Expression::ARGS(exprs) = args { exprs } else { &else_branch };
//     if exprs.len() != 2 {
//         scribe.annotate_error(Error::on_coord(&env.coord,
//                                               ErrorType::EVAL_INVALID_PUSH));
//         return ERRVAL;
//     }
//     return match obj {
//         ASSOCIATIONVAL(ref mut map) => {
//             match (exprs.get(0).unwrap().deref(), exprs.get(1).unwrap().deref()) {
//                 (UNDERSCORE_EXPR(_), UNDERSCORE_EXPR(_)) => { map.default = None; }
//                 (UNDERSCORE_EXPR(_), val) => { map.default = Some(Box::new(eval_expr(val, env, scribe))); }
//                 (key, UNDERSCORE_EXPR(_)) => { map.remove(eval_expr(key, env, scribe)); }
//                 (key, val) => {
//                     map.insert(
//                         eval_expr(key, env, scribe),
//                         eval_expr(val, env, scribe),
//                     );
//                 }
//             };
//             obj
//         }
//         _ => {
//             scribe.annotate_error(Error::on_coord(&env.coord,
//                                                   ErrorType::EVAL_INVALID_OP(PUSH, vec![obj])));
//             return ERRVAL;
//         }
//     };
// }
//
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
//
// fn eval_application_args(arg: &Box<Expression>, env: &mut Environment, scribe: &mut ErrorScribe) -> Vec<Value> {
//     if let Expression::ARGS(args) = arg.deref() {
//         args.iter().map(|ex| eval_expr(ex, env, scribe)).collect()
//     } else { vec![eval_expr(arg, env, scribe)] }
// }
//
// fn eval_parametrized_application(args: &Vec<Value>, body: &Value, env: &mut Environment, scribe: &mut ErrorScribe) -> Value {
//     // | args | @ lambdaval (with params and body)
//
//     // body must contain lambda
//     let lambda_contents = if let LAMBDAVAL { params, body } = body {
//         (params.deref().clone(), body.deref().clone())
//     } else {
//         scribe.annotate_error(Error::on_coord(&env.coord, ErrorType::EVAL_ARGS_TO_NOT_APPLICABLE));
//         return ERRVAL;
//     };
//     let params = if let Expression::ARGS(params) = lambda_contents.0 { params } else { unreachable!() };
//     for (arg, param) in args.iter().zip(params) {
//         if let Expression::VAR_RAW(_, name) = param.deref() {
//             env.write(
//                 &name,
//                 arg,
//                 &Token::new(ASSIGN, 0, 0),
//             );
//         }
//     }
//     eval_expr(&lambda_contents.1, env, scribe)
// }
//
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
// pub fn replace_string_placeholders(str: &String, env: &mut Environment, scribe: &mut ErrorScribe) -> String {
//     let mut result = String::new();
//     let mut varname = String::new();
//     for ch in str.chars() {
//         match ch {
//             '{' => { varname = "_".to_string(); }
//             '}' => {
//                 varname.remove(0);
//                 let val = env.read(&varname, scribe);
//                 result += &*val.to_string();
//                 varname.clear();
//             }
//             _ => {
//                 if varname.len() > 0 { varname.push(ch); } else { result.push(ch); }
//             }
//         }
//     }
//     result
// }
//
//
// pub fn eval_association_declaration(
//     assoc_state: AssociationState,
//     input_state: &InputState,
//     is_lazy: bool,
//     items: &Vec<Box<Expression>>,
//     env: &mut Environment,
//     scribe: &mut ErrorScribe,
// ) -> Value {
//     match desugar_association_pairs(assoc_state, input_state, items, env, scribe) {
//         Some(pairs) => { eval_association(&pairs, is_lazy, env, scribe) }
//         None => {
//             scribe.annotate_error(Error::on_coord(&env.coord,
//                                                   ErrorType::EVAL_INVALID_RANGE));
//             ERRVAL
//         }
//     }
// }
//
// fn desugar_association_pairs(
//     assoc_state: AssociationState,
//     input_state: &InputState,
//     items: &Vec<Box<Expression>>,
//     env: &mut Environment,
//     scribe: &mut ErrorScribe,
// ) -> Option<Vec<(Box<Expression>, Box<Expression>)>> {
//     let mut pairs = vec![];
//     if input_state == &InputState::RANGE {
//         let range = match eval_range(&items, env, scribe) {
//             Some(r) => { r }
//             None => { return None; }
//         };
//         for (el, i) in range.zip(0..) {
//             pairs.push(
//                 if assoc_state == AssociationState::LIST {
//                     (Box::new(Expression::LITERAL(Token::new(TokenType::INTEGER(i), 0, 0))),
//                      Box::new(Expression::LITERAL(Token::new(TokenType::INTEGER(el), 0, 0))))
//                 } else {
//                     (Box::new(Expression::LITERAL(Token::new(TokenType::INTEGER(el), 0, 0))),
//                      Box::new(Expression::LITERAL(Token::new(TokenType::TRUE, 0, 0))))
//                 }
//             );
//         }
//     } else {
//         for (el, i) in items.iter().zip(0..) {
//             pairs.push(
//                 if assoc_state == AssociationState::LIST {
//                     (Box::new(Expression::LITERAL(Token::new(TokenType::INTEGER(i), 0, 0))),
//                      (*el).clone())
//                 } else {
//                     ((*el).clone(),
//                      Box::new(Expression::LITERAL(Token::new(TokenType::TRUE, 0, 0))))
//                 }
//             );
//         }
//     }
//     Some(pairs)
// }
//
// fn eval_range(items: &Vec<Box<Expression>>, env: &mut Environment, scribe: &mut ErrorScribe) -> Option<Box<dyn Iterator<Item=i64>>> {
//     if items.len() != 2 { return None; }
//     let lo = eval_expr(items.get(0).unwrap(), env, scribe);
//     let hi = eval_expr(items.get(1).unwrap(), env, scribe);
//     return match (lo, hi) {
//         (INTEGERVAL(i), INTEGERVAL(j)) => {
//             if i > j { Some(Box::new((1 + j..1 + i).rev())) } else { Some(Box::new(i..j)) }
//         }
//         (_, _) => { None }
//     };
// }