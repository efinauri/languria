#[cfg(test)]
mod tests {
    use crate::errors::ErrorScribe;
    use crate::evaluator::{evaluate_expressions, Scope, Value};
    use crate::evaluator::Value::{BOOLEAN, NOTAVAL};
    use crate::lexer::Token;
    use crate::lexer::TokenType::*;
    use crate::parser::Expression::*;

    #[test]
    fn empty_eval() {
        let mut scope = Scope::new();
        let mut es = ErrorScribe::debug();
        let v = evaluate_expressions(
            vec![],
            &mut scope,
            &mut es);
        dbg!(&v);
        assert_eq!(v, NOTAVAL);
    }
    #[test]
    fn var_permanence() {
        let mut scope = Scope::new();
        let mut es = ErrorScribe::debug();
        let val = LITERAL { value: Token::debug(INTEGER(2)) };
        let v = evaluate_expressions(
            vec![
                VAR_ASSIGN {
                    varname: "x".to_string(),
                    op: Token::debug(ASSIGN),
                    varval: Box::new(val.clone()),
                },
                BINARY {
                    lhs: Box::new(VAR_RAW {varname: "x".parse().unwrap() }),
                    op: Token::debug(EQ),
                    rhs: Box::new(val.clone()),
                }
            ],
            &mut scope,
            &mut es);
        dbg!(&v);
        assert_eq!(scope.read(&String::from("x")), Some(&Value::INTEGER(2)));
        assert_eq!(v, BOOLEAN(true));
    }
}