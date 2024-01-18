#[cfg(test)]
mod tests {
    use crate::parser::Parser;
use crate::errors::ErrorScribe;
    use crate::lexer::Token;
    use crate::lexer::TokenType::*;
    use crate::parser::Expression;
    use crate::parser::Expression::*;
    use crate::shared::WalksCollection;

    impl Expression {
        pub fn type_equals(&self, other: &Self) -> bool {
            match (self, other) {
                (RETURN_EXPR(expr), other) => {
                    expr.type_equals(other)
                }
                (BLOCK(exprs), _) => {
                    match exprs.last() {
                        None => { false }
                        Some(e) => { e.type_equals(other) }
                    }
                }
                (LITERAL(_), LITERAL(_)) |
                (UNARY { ..}, UNARY { .. }) |
                (BINARY {.. }, BINARY {.. }) |
                (LOGIC {.. }, LOGIC { .. }) |
                (GROUPING(_), GROUPING(_)) |
                (VAR_ASSIGN { .. }, VAR_ASSIGN { .. }) |
                (VAR_RAW(_), VAR_RAW(_)) |
                (APPLICATION { .. }, APPLICATION { .. }) |
                (ASSOCIATION(_), ASSOCIATION(_)) |
                (QUERY { .. }, QUERY { .. }) |
                (NOTANEXPR, NOTANEXPR) => true,
                (_, _) => false
            }
        }
    }


    #[test]
    fn print_tree() {
        let mul = Token::debug(MUL);
        let four = Token::debug(INTEGER(4));
        let six_point_two = Token::debug(FLOAT(6.2));
        let minus = Token::debug(MINUS);

        let e = BINARY {
            lhs: Box::from(UNARY {
                op: minus,
                expr: Box::from(LITERAL(four)),
            }),
            op: mul,
            rhs: Box::from(GROUPING(Box::from(LITERAL(six_point_two)))),
        };
        dbg!(&e);
        println!("{:#?}", e);
    }

    #[test]
    fn query_eval() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(LBRACKET),
                Token::debug(INTEGER(2)),
                Token::debug(COLON),
                Token::debug(INTEGER(3)),
                Token::debug(RBRACKET),
                Token::debug(POUND),
                Token::debug(INTEGER(2)),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&QUERY {
            source: Box::new(NOTANEXPR),
            op: Token::debug(POUNDPOUND),
            field: Box::new(NOTANEXPR),
        }));
    }

    #[test]
    fn application_eval() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(INTEGER(2)),
                Token::debug(AT),
                Token::debug(IT),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&APPLICATION {
            arg: Box::new(NOTANEXPR),
            op: Token::debug(NOTATOKEN),
            body: Box::new(NOTANEXPR),
        }));
    }


    #[test]
    fn empty_eval() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&NOTANEXPR));
    }

    #[test]
    fn logic_exprs() {
        let mut es = ErrorScribe::debug();
        for (op_tok, idx) in vec![
            Token::debug(AND),
            Token::debug(OR),
            Token::debug(XOR),
        ].iter().zip(0..)
        {
            let mut p = Parser::from_tokens(
                vec![
                    Token::debug(TRUE),
                    op_tok.clone(),
                    Token::debug(FALSE),
                ],
                &mut es);
            let expr = p.build_expression();
            dbg!(idx, &expr);
            assert!(expr.type_equals(&LOGIC {
                lhs: Box::new(NOTANEXPR),
                op: Token::debug(NOTATOKEN),
                rhs: Box::new(NOTANEXPR),
            }));
        }
    }

    #[test]
    fn binary_exprs() {
        let mut es = ErrorScribe::debug();
        for (op_tok, idx) in vec![
            Token::debug(EQ),
            Token::debug(GT),
            Token::debug(PLUS),
            Token::debug(DIV),
        ].iter().zip(0..)
        {
            let mut p = Parser::from_tokens(
                vec![
                    Token::debug(IDENTIFIER(String::from("x"))),
                    op_tok.clone(),
                    Token::debug(INTEGER(2)),
                ],
                &mut es);
            let expr = p.build_expression();
            dbg!(idx, &expr);
            assert!(expr.type_equals(&BINARY {
                lhs: Box::new(NOTANEXPR),
                op: Token::debug(NOTATOKEN),
                rhs: Box::new(NOTANEXPR),
            }));
        }
    }

    #[test]
    fn unary_exprs() {
        let mut es = ErrorScribe::debug();
        for (op_tok, idx) in vec![
            Token::debug(BANG),
            Token::debug(DOLLAR),
        ].iter().zip(0..)
        {
            let mut p = Parser::from_tokens(
                vec![
                    op_tok.clone(),
                    Token::debug(INTEGER(2)),
                ],
                &mut es);
            let expr = p.build_expression();
            dbg!(idx, &expr);
            assert!(expr.type_equals(&UNARY {
                op: Token::debug(NOTATOKEN),
                expr: Box::new(NOTANEXPR),
            }));
        }
    }

    #[test]
    fn primary_exprs() {
        let mut es = ErrorScribe::debug();
        for (op_tok, idx) in vec![
            Token::debug(INTEGER(2)),
            Token::debug(FLOAT(2.2)),
            Token::debug(STRING(String::from("hi"))),
        ].iter().zip(0..)
        {
            let mut p = Parser::from_tokens(
                vec![
                    op_tok.clone(),
                    Token::debug(INTEGER(2)),
                ],
                &mut es);
            let expr = p.build_expression();
            dbg!(idx, &expr);
            assert!(expr.type_equals(&LITERAL(Token::debug(NOTATOKEN))));
        }
    }

    #[test]
    fn grouping() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(LPAREN),
                Token::debug(INTEGER(2)),
                Token::debug(RPAREN),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&GROUPING(Box::new(NOTANEXPR))));
    }

    #[test]
    fn raw_var() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(IDENTIFIER(String::from("x"))),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&VAR_RAW("".to_string())));
    }

    #[test]
    fn next_in_and_curr_is_seq() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(IDENTIFIER(String::from("x"))),
                Token::debug(GT),
                Token::debug(LT),
                Token::debug(GTE),
                Token::debug(LTE),
            ],
            &mut es);
        dbg!(&p.read_curr());
        assert!(p.assert_curr_is(IDENTIFIER(String::from("Y"))));
        assert!(p.curr_in(&[IDENTIFIER(String::from("Y"))]));
        p.cursor.mov(2);
        dbg!(&p.read_curr());
        assert!(p.curr_in(&[LT, DOLLAR, BANG]));
        assert!(p.curr_in(&[LT]));
        assert!(!p.curr_in(&[DOLLAR, BANG]));
        assert!(p.assert_curr_is(LT));
        assert!(p.curr_is_seq(&[LT, GTE]));
        assert!(!p.curr_is_seq(&[LT, GTE, LTE, LTE]));
        assert!(!p.curr_is_seq(&[GT, BANG]));
        assert!(!p.curr_is_seq(&[]));
        p.cursor.mov(100);
        assert!(!p.assert_curr_is(LT));
    }

    #[test]
    fn assignment() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(IDENTIFIER(String::from("x"))),
                Token::debug(ASSIGN),
                Token::debug(INTEGER(2)),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&VAR_ASSIGN {
            varname: "".to_string(),
            op: Token::debug(NOTATOKEN),
            varval: Box::new(NOTANEXPR),
        }));
    }

    #[test]
    fn return_expr() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(RETURN),
                Token::debug(INTEGER(2)),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&LITERAL(Token::debug(INTEGER(2)))));
    }

    #[test]
    fn code_block() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(LBRACE),
                Token::debug(NOTATOKEN),
                Token::debug(NOTATOKEN),
                Token::debug(INTEGER(2)),
                Token::debug(RBRACE),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&LITERAL(Token::debug(INTEGER(2)))));
    }

    #[test]
    fn association() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(LBRACKET),
                Token::debug(INTEGER(1)),
                Token::debug(COLON),
                Token::debug(INTEGER(2)),
                Token::debug(COMMA),
                Token::debug(INTEGER(1)),
                Token::debug(COLON),
                Token::debug(INTEGER(2)),
                Token::debug(COMMA),
                Token::debug(UNDERSCORE),
                Token::debug(COLON),
                Token::debug(INTEGER(2)),
                Token::debug(RBRACKET),
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert!(expr.type_equals(&ASSOCIATION(vec![])));
    }
}
