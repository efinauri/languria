#[cfg(test)]
mod tests {
    use crate::errors::ErrorScribe;
    use crate::lexer::Token;
    use crate::lexer::TokenType::*;
    use crate::parser::{LITERAL, Parser};
    use crate::parser::Expression::{BINARY, GROUPING, NOTANEXPR, UNARY, VAR_ASSIGN, VAR_RAW};
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
        assert_eq!(expr.type_equals(&NOTANEXPR), true);
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
        assert_eq!(expr.type_equals(&VAR_ASSIGN {
            varname: "".to_string(),
            op: Token::debug(NOTATOKEN),
            varval: Box::new(NOTANEXPR),
        }), true);
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
            assert_eq!(expr.type_equals(&BINARY {
                lhs: Box::new(NOTANEXPR),
                op: Token::debug(NOTATOKEN),
                rhs: Box::new(NOTANEXPR),
            }), true);
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
            assert_eq!(expr.type_equals(&UNARY {
                op: Token::debug(NOTATOKEN),
                expr: Box::new(NOTANEXPR),
            }), true);
        }
    }

    #[test]
    fn primary_exprs() {
        let mut es = ErrorScribe::debug();
        for (op_tok, idx) in vec![
            Token::debug(INTEGER(2)),
            Token::debug(FLOAT(2.2)),
            Token::debug(STRING(String::from("hi")))
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
            assert_eq!(expr.type_equals(&LITERAL {
                value: Token::debug(NOTATOKEN),
            }), true);
        }
    }

    #[test]
    fn grouping() {
        let mut es = ErrorScribe::debug();
        let mut p = Parser::from_tokens(
            vec![
                Token::debug(LPAREN),
                Token::debug(INTEGER(2)),
                Token::debug(RPAREN)
            ],
            &mut es);
        let expr = p.build_expression();
        dbg!(&expr);
        assert_eq!(expr.type_equals(&GROUPING {
            expr: Box::new(NOTANEXPR),
        }), true);
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
        assert_eq!(expr.type_equals(&VAR_RAW {
            varname: "".to_string(),
        }), true);
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
        assert_eq!(p.assert_curr_is(IDENTIFIER(String::from("Y"))), true);
        p.cursor.mov(2);
        dbg!(&p.read_curr());
        assert_eq!(p.curr_in(&[LT, DOLLAR, BANG]), true);
        assert_eq!(p.curr_in(&[LT]), true);
        assert_eq!(p.curr_in(&[DOLLAR, BANG]), false);
        assert_eq!(p.assert_curr_is(LT), true);
        assert_eq!(p.curr_is_seq(&[GT, LT, GTE]), true);
        assert_eq!(p.curr_is_seq(&[GT, LT, GTE, LTE, LTE]), false);
        assert_eq!(p.curr_is_seq(&[GT, BANG]), false);
        assert_eq!(p.curr_is_seq(&[]), false);
        p.cursor.mov(100);
        assert_eq!(p.assert_curr_is(LT), false);
    }
}
