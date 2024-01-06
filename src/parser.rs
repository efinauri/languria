use std::fmt::{Display, Formatter};

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::{Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::Expression::*;
use crate::shared::{Counter, WalksCollection};

#[derive(Debug, Clone)]
pub enum Expression {
    LITERAL { value: Token },
    UNARY { op: Token, expr: Box<Expression> },
    BINARY { lhs: Box<Expression>, op: Token, rhs: Box<Expression> },
    GROUPING { expr: Box<Expression> },
    VAR_ASSIGN { varname: String, varval: Box<Expression> },
    VAR_RAW { varname: String },
    NOTANEXPR,
}


impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            LITERAL { value } =>
                { f.write_str(value.to_string().as_str()).unwrap(); }
            UNARY { op, expr } =>
                { f.write_str(&*format!("({} {})", op, expr)).unwrap(); }
            BINARY { lhs, op, rhs } =>
                { f.write_str(&*format!("{} <{} {}>", op, lhs, rhs)).unwrap(); }
            GROUPING { expr } =>
                { f.write_str(&*format!("(group {})", expr)).unwrap(); }
            NOTANEXPR => { let _ = f.write_str("ERR"); }
            VAR_ASSIGN { varname, varval } =>
                { f.write_str(&*format!("{}<-{}", varname, varval)).unwrap(); }
            VAR_RAW { varname } =>
                { f.write_str(&*format!("?<-{}", varname)).unwrap() }
        }
        Ok(())
    }
}

const EQ_TOKENS: [TokenType; 2] = [UNEQ, EQ];
const CMP_TOKENS: [TokenType; 4] = [GT, LT, GTE, LTE];
const MATH_LO_PRIORITY_TOKENS: [TokenType; 2] = [PLUS, MINUS];
const MATH_HI_PRIORITY_TOKENS: [TokenType; 2] = [DIV, MUL];
const UNARY_TOKENS: [TokenType; 3] = [BANG, MINUS, DOLLAR];

pub struct Parser<'a> {
    tokens: Vec<Token>,
    counter: Counter,
    scribe: &'a mut ErrorScribe,
}

impl WalksCollection<'_, Vec<Token>, Token> for Parser<'_> {
    fn cnt(&self) -> &Counter { &self.counter }
    fn mut_cnt(&mut self) -> &mut Counter { &mut self.counter }
    fn arr(&self) -> &Vec<Token> { &self.tokens }
}


impl Parser<'_> {
    pub fn from_tokens(tokens: Vec<Token>, scribe: &mut ErrorScribe) -> Parser {
        Parser {
            tokens,
            counter: Counter::new(),
            scribe,
        }
    }

    pub fn parse(&mut self) -> Vec<Expression> {
        let mut expressions = vec![];
        if self.tokens.is_empty() { return expressions; }
        while self.can_consume() {
            expressions.push(self.build_expression())
        }
        expressions
    }

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
            let seq = [DOLLAR, LT, IDENTIFIER(String::new()), GT];
            if self.curr_is_seq(&seq) {
                let op = self.read_prev().clone();
                self.counter.step_fwd();
                let value = self.read_curr().clone();
                self.counter.mov(2);
                return BINARY { op: op, rhs: Box::new(LITERAL { value }), lhs: Box::new(self.unary()) };
            }
            return UNARY { op: self.read_prev().clone(), expr: Box::new(self.unary()) };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expression {
        if !self.can_consume() {
            self.scribe.annotate_error(Error::on_line(
                self.read_prev().line,
                ErrorType::EXPECTEDLITERAL { found: EOF }));
            return NOTANEXPR;
        }
        return match &self.tokens.get(self.counter.get()).unwrap().ttype {
            IDENTIFIER(str) => {
                self.counter.step_fwd();
                let assign = [ASSIGN];
                if self.next_in(&assign) {
                    self.counter.step_fwd();
                    VAR_ASSIGN { varname: str.clone(), varval: Box::new(self.build_expression()) }
                } else { VAR_RAW { varname: str.clone() } }
            }
            FALSE | TRUE | INTEGER(_) | STRING(_) | FLOAT(_) | EOLPRINT => {
                self.counter.step_fwd();
                LITERAL { value: self.read_prev().clone() }
            }
            LPAREN => {
                self.counter.step_fwd();
                let expr = self.build_expression();
                self.assert_next_is(RPAREN);
                self.counter.step_fwd();
                GROUPING { expr: Box::new(expr) }
            }
            _ => {
                NOTANEXPR
            }
        };
    }

    fn assert_next_is(&mut self, ttype: TokenType) {
        if !self.can_consume() || ttype != self.read_curr().ttype {
            self.scribe.annotate_error(Error::on_line(
                0,
                ErrorType::EXPECTEDTOKEN { ttype }));
        }
    }

    fn curr_is_seq(&self, ttypes: &[TokenType]) -> bool {
        if !self.can_peek(ttypes.len() - 1) { return false; }
        if self.counter.get() == 0 { return false; }
        if !self.read_prev().type_equals(&ttypes[0]) { return false; }
        ttypes[1..].iter()
            .zip(0..)
            .all(|(tt, i)| self.peek(i).type_equals(tt))
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