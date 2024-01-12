#[cfg(test)]
mod tests {
    use crate::errors::ErrorScribe;
    use crate::lexer::Token;
    use crate::lexer::TokenType::*;
    use crate::parser::{LITERAL, Parser};
    use crate::parser::Expression::{APPLICATION, BINARY, GROUPING, NOTANEXPR, UNARY, VAR_ASSIGN, VAR_RAW};
    use crate::shared::WalksCollection;

    #[test]
    fn print_tree() {
        let mul = Token::debug(MUL);
        let four = Token::debug(INTEGER(4));
        let six_point_two = Token::debug(FLOAT(6.2));
        let minus = Token::debug(MINUS);

        let e = BINARY {
            lhs: Box::from(UNARY {
                op: minus,
                expr: Box::from(LITERAL { value: four }),
            }),
            op: mul,
            rhs: Box::from(GROUPING {
                expr: Box::from(LITERAL { value: six_point_two })
            }),
        };
        dbg!(&e);
        println!("{}", e);
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
            assert!(expr.type_equals(&LITERAL {
                value: Token::debug(NOTATOKEN),
            }));
        }
    }

    #[test]
    fn it_and_ti_to_vars() {
        let mut es = ErrorScribe::debug();
        for (op_tok, idx) in vec![
            Token::debug(IT),
            Token::debug(TI),
        ].iter().zip(0..)
        {
            let mut p = Parser::from_tokens(
                vec![
                    op_tok.clone(),
                ],
                &mut es);
            let expr = p.build_expression();
            dbg!(idx, &expr);
            assert!(expr.type_equals(&VAR_RAW { varname: String::new() }));
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
        assert!(expr.type_equals(&GROUPING {
            expr: Box::new(NOTANEXPR),
        }));
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
        assert!(expr.type_equals(&VAR_RAW {
            varname: "".to_string(),
        }));
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
        assert!(p.curr_is_seq(&[GT, LT, GTE]));
        assert!(!p.curr_is_seq(&[GT, LT, GTE, LTE, LTE]));
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
        assert!(expr.type_equals(&LITERAL { value: Token::debug(INTEGER(2)) }));
    }

    #[test]
    fn application() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(INTEGER(3)),
                Token::debug(AT),
                Token::debug(LPAREN),
                Token::debug(IT),
                Token::debug(RPAREN),
            ],
            &mut es);
        p.parse();
        let binding = p.into_expressions();
        let expr = binding.get(0).unwrap();
        dbg!(&expr);
        assert!(expr.type_equals(&APPLICATION {
            arg: Box::new(NOTANEXPR),
            body: vec![],
        }));
    }
}
