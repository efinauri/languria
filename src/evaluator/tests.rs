#[cfg(test)]
mod tests {
    use rand::Rng;

    use crate::environment::{Environment, Value};
    use crate::environment::Value::{BOOLEANVAL, INTEGERVAL, NOTAVAL, STRINGVAL};
    use crate::errors::ErrorScribe;
    use crate::evaluator::evaluate_expressions;
    use crate::lexer::Token;
    use crate::lexer::TokenType::*;
    use crate::parser::Expression;
    use crate::parser::Expression::*;

    fn int_expr(n: i32) -> Box<Expression> {
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

    fn int_val(n: i32) -> Value { INTEGERVAL(n) }

    fn str_val(str: &str) -> Value { STRINGVAL(str.parse().unwrap()) }

    fn bool_val(b: bool) -> Value { BOOLEANVAL(b) }

    #[test]
    fn empty_eval() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_eq!(v, NOTAVAL);
    }

    #[test]
    fn eval_query() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![
                Box::new(QUERY {
                    source: Box::new(
                        ASSOCIATION(vec![
                            (str_expr("s"), int_expr(2))
                        ])
                    ),
                    field: str_expr("s"),
                })
            ],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_eq!(v, int_val(2));
        assert_ne!(v, str_val("s"))
    }

    #[test]
    fn eval_query_default() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![
                Box::new(QUERY {
                    source: Box::new(
                        ASSOCIATION(vec![
                            (Box::new(LITERAL(Token::debug(UNDERSCORE))), int_expr(2))
                        ])
                    ),
                    field: str_expr("r"),
                })
            ],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_eq!(v, int_val(2));
        assert_ne!(v, str_val("r"))
    }

    #[test]
    fn return_in_block() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![
                Box::new(BLOCK(
                    vec![
                        Box::new(RETURN_EXPR(int_expr(2))),
                        str_expr("s"),
                    ]
                ))
            ],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_eq!(v, int_val(2));
        assert_ne!(v, str_val("s"))
    }

    #[test]
    fn application() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![
                Box::new(APPLICATION {
                    arg: int_expr(2),
                    body: Box::new(BINARY {
                        lhs: str_expr("s"),
                        op: Token::debug(MUL),
                        rhs: Box::new(LITERAL(Token::debug(IT))),
                    }),
                })],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_ne!(v, int_val(2));
        assert_eq!(v, str_val("ss"))
    }

    #[test]
    fn unary() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![
                Box::new(UNARY {
                    op: Token::debug(BANG),
                    expr: int_expr(2),
                }),
            ],
            &mut es,
            &mut env, false);
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
                    let v = evaluate_expressions(
                        &vec![
                            Box::new(
                                LOGIC {
                                    lhs: bool_expr(lhs),
                                    op: tok,
                                    rhs: bool_expr(rhs),
                                }
                            )
                        ],
                        &mut es,
                        &mut env, false);
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
            ]
                .iter().zip([
                Token::debug(PLUS),
                Token::debug(MINUS),
                Token::debug(MUL),
                Token::debug(DIV),
            ]) {
                let a = rand::thread_rng().gen_range(0..10);
                let b = rand::thread_rng().gen_range(1..10);
                let v = evaluate_expressions(
                    &vec![
                        Box::new(
                            BINARY {
                                lhs: int_expr(a),
                                op: tok.clone(),
                                rhs: int_expr(b),
                            }
                        )
                    ],
                    &mut es,
                    &mut env, false);
                dbg!(a, &tok, b, &v);
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
                let v = evaluate_expressions(
                    &vec![
                        Box::new(
                            BINARY {
                                lhs: int_expr(a),
                                op: tok,
                                rhs: int_expr(b),
                            }
                        )
                    ],
                    &mut es,
                    &mut env, false);
                dbg!(&v);
                assert_eq!(v, BOOLEANVAL(op(a, b)))
            }
        }
    }

    #[test]
    fn fstring() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![
                Box::new(VAR_ASSIGN{
                    varname: "x".to_string(),
                    op: Token::debug(ASSIGN),
                    varval: int_expr(2),
                }),
                str_expr("x={x}{x}")
            ],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_eq!(v, str_val("x=22"))
    }
}
