use std::fmt::{Display, Formatter};

use crate::lexer::Token;

#[derive(Debug)]
pub enum Expression {
    LITERAL { value: Token },
    UNARY { op: Token, expr: Box<Expression> },
    BINARY { lhs: Box<Expression>, op: Token, rhs: Box<Expression> },
    GROUPING { expr: Box<Expression> },
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expression::LITERAL { value } =>
                { f.write_str(value.to_string().as_str()).unwrap(); }
            Expression::UNARY { op, expr } =>
                { f.write_str(&*format!("({} {})", op, expr)).unwrap(); }
            Expression::BINARY { lhs, op, rhs } =>
                { f.write_str(&*format!("{} {} {}", op, lhs, rhs)).unwrap(); }
            Expression::GROUPING { expr } =>
                { f.write_str(&*format!("(group {})", expr)).unwrap(); }
        }
        Ok(())
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    //  tree: ,
}


#[cfg(test)]
mod tests {
    use crate::lexer::Token;
    use crate::lexer::TokenType::{INTEGER, MINUS, MUL};
    use crate::parser::Expression;
    use crate::parser::Expression::LITERAL;

    #[test]
    fn print_tree() {
        let e = Expression::BINARY {
            lhs: Box::from(Expression::UNARY {
                op: Token::from_debug(MINUS),
                expr: Box::from(LITERAL { value: Token::from_debug(INTEGER(4)) }),
            }),
            op: Token::from_debug(MUL),
            rhs: Box::from(Expression::GROUPING {
                expr: Box::from(LITERAL { value: Token::from_debug(INTEGER(6)) })
            }),
        };
        println!("{}", e);
    }
}