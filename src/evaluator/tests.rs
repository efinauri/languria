#[cfg(test)]
mod tests {
    use crate::errors::ErrorScribe;
    use crate::evaluator::{Environment, evaluate_expressions};
    use crate::evaluator::Value::{BOOLEAN, NOTAVAL};
    use crate::lexer::Token;
    use crate::lexer::TokenType::*;
    use crate::parser::Expression::*;

    #[test]
    fn empty_eval() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let v = evaluate_expressions(
            &vec![],
            &mut es,
            &mut env);
        dbg!(&v);
        assert_eq!(v, NOTAVAL);
    }

    #[test]
    fn var_permanence() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let val = LITERAL { value: Token::debug(INTEGER(2)) };
        let v = evaluate_expressions(
            &vec![
                Box::new(VAR_ASSIGN {
                    varname: "x".to_string(),
                    op: Token::debug(ASSIGN),
                    varval: Box::new(val.clone()),
                }),
                Box::new(BINARY {
                    lhs: Box::new(VAR_RAW { varname: "x".parse().unwrap() }),
                    op: Token::debug(EQ),
                    rhs: Box::new(val.clone()),
                }),
            ],
            &mut es,
            &mut env);
        dbg!(&v);
        // assert_eq!(scope.read(&String::from("x")), Some(&Value::INTEGER(2)));
        assert_eq!(v, BOOLEAN(true));
    }
}