use std::fmt::{Display, Formatter};

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::{Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::Expression::*;
use crate::shared::{Cursor, WalksCollection};

mod tests;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum Expression {
    LITERAL { value: Token },
    UNARY { op: Token, expr: Box<Expression> },
    BINARY { lhs: Box<Expression>, op: Token, rhs: Box<Expression> },
    GROUPING { expr: Box<Expression> },
    VAR_ASSIGN { varname: String, op: Token, varval: Box<Expression> },
    VAR_RAW { varname: String },
    BLOCK { exprs: Vec<Box<Expression>> },
    NOTANEXPR,
}

impl Expression {
    #[allow(dead_code)]
    pub fn type_equals(&self, other: &Self) -> bool {
        match (self, other) {
            (BLOCK { exprs }, _) => {
                match exprs.last() {
                    None => { false }
                    Some(e) => { e.type_equals(other) }
                }
            }
            (LITERAL { value: _ }, LITERAL { value: _ }) |
            (UNARY { op: _, expr: _ }, UNARY { op: _, expr: _ }) |
            (BINARY { lhs: _, op: _, rhs: _ }, BINARY { lhs: _, op: _, rhs: _ }) |
            (GROUPING { expr: _ }, GROUPING { expr: _ }) |
            (VAR_ASSIGN { varname: _, op: _, varval: _ },
                VAR_ASSIGN { varname: _, op: _, varval: _ }) |
            (VAR_RAW { varname: _ }, VAR_RAW { varname: _ }) |
            (NOTANEXPR, NOTANEXPR) => true,
            (_, _) => false
        }
    }
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
            VAR_ASSIGN { varname, op: _, varval } =>
                { f.write_str(&*format!("{}<-{}", varname, varval)).unwrap(); }
            VAR_RAW { varname } =>
                { f.write_str(&*format!("?<-{}", varname)).unwrap() }
            BLOCK { exprs } =>
                { f.write_str(&*format!("[[{:?}]]", exprs)).unwrap() }
        }
        Ok(())
    }
}

const ASSIGN_TOKENS: [TokenType; 8] = [ASSIGN, INTO, MINASSIGN, MAXASSIGN, PLUSASSIGN, MINUSASSIGN, MULASSIGN, DIVASSIGN];
const EQ_TOKENS: [TokenType; 2] = [UNEQ, EQ];
const CMP_TOKENS: [TokenType; 4] = [GT, LT, GTE, LTE];
const MATH_LO_PRIORITY_TOKENS: [TokenType; 2] = [PLUS, MINUS];
const MATH_HI_PRIORITY_TOKENS: [TokenType; 2] = [DIV, MUL];
const UNARY_TOKENS: [TokenType; 3] = [BANG, MINUS, DOLLAR];

pub struct Parser<'a> {
    tokens: Vec<Token>,
    cursor: Cursor,
    scribe: &'a mut ErrorScribe,
}

impl WalksCollection<'_, Vec<Token>, Token> for Parser<'_> {
    fn cnt(&self) -> &Cursor { &self.cursor }
    fn mut_cnt(&mut self) -> &mut Cursor { &mut self.cursor }
    fn arr(&self) -> &Vec<Token> { &self.tokens }
}


impl Parser<'_> {
    pub fn from_tokens(tokens: Vec<Token>, scribe: &mut ErrorScribe) -> Parser {
        Parser { tokens, cursor: Cursor::new(), scribe }
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

        while self.curr_in(&EQ_TOKENS) {
            self.cursor.step_fwd();
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
        while self.curr_in(&CMP_TOKENS) {
            self.cursor.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.term()),
            };
        }
        expr
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        while self.curr_in(&MATH_LO_PRIORITY_TOKENS) {
            self.cursor.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.factor()),
            };
        }
        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();

        while self.curr_in(&MATH_HI_PRIORITY_TOKENS) {
            self.cursor.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.unary()),
            };
        }
        expr
    }

    fn unary(&mut self) -> Expression {
        if self.curr_in(&UNARY_TOKENS) {
            let seq = [DOLLAR, LT, IDENTIFIER(String::new()), GT];
            if self.curr_is_seq(&seq) {
                let op = self.read_prev().clone();
                self.cursor.step_fwd();
                let value = self.read_curr().clone();
                self.cursor.mov(2);
                return BINARY { op, rhs: Box::new(LITERAL { value }), lhs: Box::new(self.unary()) };
            }
            self.cursor.step_fwd();
            return UNARY { op: self.read_prev().clone(), expr: Box::new(self.unary()) };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expression {
        if !self.can_consume() {
            self.scribe.annotate_error(Error::on_line(
                if self.tokens.is_empty() { 0 } else { self.read_curr().line },
                ErrorType::EXPECTEDLITERAL { found: EOF }));
            return NOTANEXPR;
        }
        let ttype = &self.tokens.get(self.cursor.get()).unwrap().ttype.clone();
        return match ttype {
            IDENTIFIER(str) => {
                self.cursor.step_fwd();
                self.process_assignment(str)
            }
            FALSE | TRUE | INTEGER(_) | STRING(_) | FLOAT(_) | EOLPRINT => {
                self.cursor.step_fwd();
                LITERAL { value: self.read_prev().clone() }
            }
            LPAREN => {
                self.cursor.step_fwd();
                let expr = self.build_expression();
                self.assert_curr_is(RPAREN);
                self.cursor.step_fwd();
                GROUPING { expr: Box::new(expr) }
            }
            LBRACE => { self.process_code_block() }

            _ => {
                self.scribe.annotate_error(Error::on_line(
                    self.read_curr().line, ErrorType::PARSER_UNEXPECTED_TOKEN {
                        ttype: ttype.clone()
                    }));
                self.cursor.step_fwd();
                NOTANEXPR
            }
        };
    }

    fn process_assignment(&mut self, str: &String) -> Expression {
        if self.can_consume() && self.curr_in(&ASSIGN_TOKENS) {
            self.cursor.step_fwd();
            return match &self.peek_back(2).ttype {
                IDENTIFIER(str) => {
                    VAR_ASSIGN {
                        varname: str.clone(),
                        op: self.read_prev().clone(),
                        varval: Box::new(self.build_expression()),
                    }
                }
                _ => {
                    self.scribe.annotate_error(Error::on_line(self.read_curr().line,
                                                              ErrorType::BADASSIGNMENTLHS));
                    NOTANEXPR
                }
            };
        }
        VAR_RAW { varname: str.clone() }
    }

    fn process_code_block(&mut self) -> Expression {
        let mut exprs = vec![];
        self.cursor.step_fwd();
        while self.can_consume() && !self.curr_in(&[RBRACE]) {
            exprs.push(Box::new(self.build_expression()));
        }
        self.assert_curr_is(RBRACE);
        self.cursor.step_fwd();
        BLOCK { exprs }
    }

    fn assert_curr_is(&mut self, ttype: TokenType) -> bool {
        if !self.can_consume() || !self.read_curr().type_equals(&ttype) {
            self.scribe.annotate_error(Error::on_line(
                0,
                ErrorType::EXPECTEDTOKEN { ttype }));
            return false;
        }
        true
    }

    fn curr_is_seq(&self, ttypes: &[TokenType]) -> bool {
        if self.tokens.is_empty() || ttypes.is_empty() { return false; }
        if !self.can_peek(ttypes.len() - 1) { return false; }
        if self.cursor.get() == 0 { return false; }
        if !self.read_prev().type_equals(&ttypes[0]) { return false; }
        ttypes[1..].iter()
            .zip(0..)
            .all(|(tt, i)| self.peek(i).type_equals(tt))
    }

    fn curr_in(&self, ttypes: &[TokenType]) -> bool {
        self.can_consume() && ttypes.contains(&self.read_curr().ttype)
    }
}