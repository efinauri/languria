#[cfg(test)]
mod tests {
    use crate::environment::{Environment, Value};
    use crate::errors::ErrorScribe;
    use crate::environment::Value::{BOOLEANVAL, NOTAVAL};
    use crate::evaluator::evaluate_expressions;
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
            &mut env, false);
        dbg!(&v);
        assert_eq!(v, NOTAVAL);
    }

    #[test]
    fn var_permanence() {
        let mut es = ErrorScribe::debug();
        let mut env = Environment::new();
        let val = LITERAL(Token::debug(INTEGER(2)));
        let v = evaluate_expressions(
            &vec![
                Box::new(VAR_ASSIGN {
                    varname: "x".to_string(),
                    op: Token::debug(ASSIGN),
                    varval: Box::new(val.clone()),
                }),
                Box::new(BINARY {
                    lhs: Box::new(VAR_RAW ("x".parse().unwrap())),
                    op: Token::debug(EQ),
                    rhs: Box::new(val.clone()),
                }),
            ],
            &mut es,
            &mut env, false);
        dbg!(&v);
        assert_eq!(env.read(&String::from("x")), Some(&Value::INTEGERVAL(2)));
        // assert_eq!(v, BOOLEANVAL(true));
    }
}