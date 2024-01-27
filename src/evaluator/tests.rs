#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use rand::Rng;

    use crate::environment::Environment;
    use crate::environment::value::Value::{BOOLEANVAL, INTEGERVAL, NOTAVAL, OPTIONVAL, STRINGVAL};
    use crate::environment::value::Value;
    use crate::errors::ErrorScribe;
    use crate::evaluator::Evaluator;
    use crate::lexer::{Coord, Token};
    use crate::lexer::TokenType::*;
    use crate::parser::Expression;
    use crate::parser::Expression::*;

    fn int_expr(n: i64) -> Box<Expression> {
        Box::new(
            LITERAL(
                Token::debug(INTEGER(n))))
    }

    fn str_expr(str: &str) -> Box<Expression> {
        Box::new(
            LITERAL(
                Token::debug(STRING(str.parse().unwrap()))))
    }

    fn bool_expr(b: bool) -> Box<Expression> {
        Box::new(
            LITERAL(
                Token::debug(if b { TRUE } else { FALSE })))
    }

    fn int_val(n: i64) -> Value { INTEGERVAL(n) }

    fn yes_val(v: Value) -> Value { OPTIONVAL(Some(Box::new(v))) }

    fn str_val(str: &str) -> Value { STRINGVAL(str.parse().unwrap()) }

    impl<'a> Evaluator<'a> {
        fn from_debug(exp_queue: &'a mut VecDeque<Expression>, scribe: &'a mut ErrorScribe, env: &'a mut Environment) -> Evaluator<'a> {
            Evaluator {
                exp_queue,
                op_queue: Default::default(),
                val_queue: Default::default(),
                should_enqueue_val: false,
                scribe,
                env,
            }
        }
    }

    #[test]
    fn empty_eval() {
        let mut vec = VecDeque::new();
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert!(v.type_equals(&NOTAVAL));
    }

    #[test]
    fn eval_pull() {
        let mut vec = VecDeque::from(
            vec![
                PULL_EXPR {
                    source: Box::new(
                        ASSOCIATION_EXPR(vec![
                            (str_expr("s"), int_expr(2))
                        ])
                    ),
                    op: Token::debug(PULL),
                    key: str_expr("s"),
                }
            ]);
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert_eq!(v, yes_val(int_val(2)));
        assert_ne!(v, str_val("s"))
    }

    #[test]
    fn eval_default_pull() {
        let mut vec = VecDeque::from(
            vec![
                PULL_EXPR {
                    source: Box::new(
                        ASSOCIATION_EXPR(vec![
                            (Box::new(UNDERSCORE_EXPR(Coord::new())), int_expr(2))
                        ])
                    ),
                    op: Token::debug(PULL),
                    key: str_expr("r"),
                }
            ]);
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert_eq!(v, yes_val(int_val(2)));
        assert_ne!(v, str_val("r"))
    }

    #[test]
    fn return_in_block() {
        let mut vec = VecDeque::from(
            vec![BLOCK(
                vec![
                    Box::new(RETURN_EXPR(int_expr(2))),
                    str_expr("s"),
                ]
            )
            ]);
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert_eq!(v, int_val(2));
        assert_ne!(v, str_val("s"))
    }

    #[test]
    fn application() {
        let mut vec = VecDeque::from(
            vec![
                APPLIED_EXPR {
                    arg: int_expr(2),
                    op: Token::debug(AT),
                    body: Box::new(APPLICABLE_EXPR {
                        params: Box::new(ARGS(vec![Box::new(VAR_RAW(Coord::new(), "n".to_string()))])),
                        body: Box::new(BINARY {
                            lhs: Box::new(VAR_RAW(Coord::new(), "n".parse().unwrap())),
                            op: Token::debug(MUL),
                            rhs: str_expr("s"),
                        }),
                    }),
                }
            ]);
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert_ne!(v, int_val(2));
        assert_eq!(v, str_val("ss"))
    }

    #[test]
    fn unary() {
        let mut vec = VecDeque::from(
            vec![
                UNARY {
                    op: Token::debug(NOT),
                    expr: int_expr(2),
                },
            ]);
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert_ne!(v, int_val(2));
        assert_eq!(v, BOOLEANVAL(false))
    }

    #[test]
    fn logic() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        for lhs in [true, false] {
            for rhs in [true, false] {
                for (op, tok) in
                [
                    |a, b| a || b,
                    |a, b| a && b,
                    |a, b| a ^ b
                ]
                    .iter().zip([
                    Token::debug(OR),
                    Token::debug(AND),
                    Token::debug(XOR)]) {
                    let mut vec = VecDeque::from(
                        vec![
                            LOGIC {
                                lhs: bool_expr(lhs),
                                op: tok,
                                rhs: bool_expr(rhs),
                            }
                        ]);
                    let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
                    let v = eval.value();
                    dbg!(&v);
                    assert_eq!(v, BOOLEANVAL(op(lhs, rhs)))
                }
            }
        }
    }

    #[test]
    fn int_binary() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        for _ in 0..10 {
            for (op, tok) in
            [
                |a, b| a + b,
                |a, b| a - b,
                |a, b| a * b,
                |a, b| a / b,
                |a, b| a % b,
            ]
                .iter().zip([
                Token::debug(PLUS),
                Token::debug(MINUS),
                Token::debug(MUL),
                Token::debug(DIV),
                Token::debug(MODULO),
            ]) {
                let a = rand::thread_rng().gen_range(0..10);
                let b = rand::thread_rng().gen_range(1..10);
                let mut vec = VecDeque::from(
                    vec![
                        BINARY {
                            lhs: int_expr(a),
                            op: tok.clone(),
                            rhs: int_expr(b),
                        }
                    ]);
                let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
                let v = eval.value();
                dbg!(&v);
                assert_eq!(v, INTEGERVAL(op(a, b)))
            }
        }
    }

    #[test]
    fn bool_binary() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        for _ in 0..10 {
            for (op, tok) in
            [
                |a, b| a > b,
                |a, b| a < b,
                |a, b| a >= b,
                |a, b| a <= b,
                |a, b| a == b,
                |a, b| a != b,
            ]
                .iter().zip([
                Token::debug(GT),
                Token::debug(LT),
                Token::debug(GTE),
                Token::debug(LTE),
                Token::debug(EQ),
                Token::debug(UNEQ),
            ]) {
                let a = rand::thread_rng().gen_range(0..10);
                let b = rand::thread_rng().gen_range(1..10);
                let mut vec = VecDeque::from(
                    vec![
                        BINARY {
                            lhs: int_expr(a),
                            op: tok,
                            rhs: int_expr(b),
                        }
                    ]);
                let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
                let v = eval.value();
                dbg!(&v);
                assert_eq!(v, BOOLEANVAL(op(a, b)))
            }
        }
    }

    #[test]
    fn fstring() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let mut vec = VecDeque::from(
            vec![
                LITERAL(Token::debug(STRING("x={x}{x}".to_string()))),
                VAR_ASSIGN {
                    varname: "x".to_string(),
                    op: Token::debug(ASSIGN),
                    varval: int_expr(2),
                }
            ]);
        let mut eval = Evaluator::from_debug(&mut vec, &mut es, &mut env);
        let v = eval.value();
        dbg!(&v);
        assert_eq!(v, str_val("x=22"))
    }
}
