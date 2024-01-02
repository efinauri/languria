use std::fmt::{Display, Formatter};

use crate::lexer::{Token, TokenType};
use crate::lexer::TokenType::{BANG, DIV, EQ, FALSE, FLOAT, GT, GTE, INTEGER, LPAREN, LT, LTE, MINUS, MUL, PLUS, RPAREN, STRING, TRUE, UNEQ};
use crate::parser::Expression::{BINARY, GROUPING, LITERAL, UNARY};
use crate::shared::{Counter, WalksCollection};

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
            LITERAL { value } =>
                { f.write_str(value.to_string().as_str()).unwrap(); }
            UNARY { op, expr } =>
                { f.write_str(&*format!("({} {})", op, expr)).unwrap(); }
            BINARY { lhs, op, rhs } =>
                { f.write_str(&*format!("{} {} {}", op, lhs, rhs)).unwrap(); }
            GROUPING { expr } =>
                { f.write_str(&*format!("(group {})", expr)).unwrap(); }
        }
        Ok(())
    }
}

const EQ_TOKENS: [TokenType; 2] = [UNEQ, EQ];
const CMP_TOKENS: [TokenType; 4] = [GT, LT, GTE, LTE];
const MATH_LO_PRIORITY_TOKENS: [TokenType; 2] = [PLUS, MINUS];
const MATH_HI_PRIORITY_TOKENS: [TokenType; 2] = [DIV, MUL];
const UNARY_TOKENS: [TokenType; 2] = [BANG, MINUS];

pub struct Parser {
    tokens: Vec<Token>,
    counter: Counter,
}

impl WalksCollection<'_, Vec<Token>, Token> for Parser {
    fn cnt(&self) -> &Counter { &self.counter }
    fn mut_cnt(&mut self) -> &mut Counter { &mut self.counter }
    fn arr(&self) -> &Vec<Token> { &self.tokens }
}


impl Parser {
    pub fn from_tokens(tokens: Vec<Token>) -> Parser { Parser { tokens, counter: Counter::new() } }
    pub fn parse(&mut self) -> Expression { self.build_expression() }
    fn build_expression(&mut self) -> Expression { self.equality() }
    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();

        while self.next_in(&EQ_TOKENS) {
            self.counter.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.comparison()),
            };
        }
        expr
    }


    fn comparison(&mut self) -> Expression {
        let mut expr = self.term();
        while self.next_in(&CMP_TOKENS) {
            self.counter.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.term()),
            }
        }
        expr
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while self.next_in(&MATH_LO_PRIORITY_TOKENS) {
            self.counter.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.factor()),
            }
        }
        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();

        while self.next_in(&MATH_HI_PRIORITY_TOKENS) {
            self.counter.step_fwd();

            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.unary()),
            }
        }
        expr
    }

    fn unary(&mut self) -> Expression {
        if self.next_in(&UNARY_TOKENS) {
            self.counter.step_fwd();
            return UNARY { op: self.read_prev().clone(), expr: Box::new(self.unary()) };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expression {
        return match &self.read_curr().ttype {
            FALSE | TRUE | INTEGER(_) | STRING(_) | FLOAT(_) => {
                self.counter.step_fwd();
                LITERAL { value: self.read_prev().clone() }
            },
            LPAREN => {
                let expr = self.build_expression();
                self.next_must_be(RPAREN);
                self.counter.step_fwd();
                GROUPING { expr: Box::new(expr) }
            }
            _ => { panic!("TODO") }
        };
    }

    fn next_must_be(&self, ttype: TokenType) {
        if !self.can_consume() || ttype != self.read_curr().ttype {
            //TODO raise error
        }
    }
    fn next_in(&self, ttypes: &[TokenType]) -> bool {
        self.can_consume() && ttypes.contains(&self.read_curr().ttype)
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::Token;
    use crate::lexer::TokenType::{FLOAT, INTEGER, MINUS, MUL};
    use crate::parser::Expression::{BINARY, GROUPING, UNARY};
    use crate::parser::LITERAL;

    #[test]
    fn print_tree() {
        let mul = Token::from_debug(MUL);
        let four = Token::from_debug(INTEGER(4));
        let six_point_two = Token::from_debug(FLOAT(6.2));
        let minus = Token::from_debug(MINUS);

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
        println!("{}", e);
    }
}