use std::vec;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::{Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::Expression::*;
use crate::shared::{Cursor, WalksCollection};

mod tests;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub enum Expression {
    LITERAL(Token),
    UNARY { op: Token, expr: Box<Expression> },
    BINARY { lhs: Box<Expression>, op: Token, rhs: Box<Expression> },
    LOGIC { lhs: Box<Expression>, op: Token, rhs: Box<Expression> },
    GROUPING(Box<Expression>),
    VAR_ASSIGN { varname: String, op: Token, varval: Box<Expression> },
    VAR_RAW(String),
    BLOCK(Vec<Box<Expression>>),
    APPLICATION { arg: Box<Expression>, op: Token, body: Box<Expression> },
    RETURN_EXPR(Box<Expression>),
    ASSOCIATION(Vec<(Box<Expression>, Box<Expression>)>),
    QUERY { source: Box<Expression>, op: Token, field: Box<Expression> },
    NOTANEXPR,
}

pub enum AssociationState {
    SET,
    LIST,
    MAP,
}

const ASSIGN_TOKENS: [TokenType; 7] = [ASSIGN, MINASSIGN, MAXASSIGN, PLUSASSIGN, MINUSASSIGN, MULASSIGN, DIVASSIGN];
const EQ_TOKENS: [TokenType; 2] = [UNEQ, EQ];
const CMP_TOKENS: [TokenType; 4] = [GT, LT, GTE, LTE];
const MATH_LO_PRIORITY_TOKENS: [TokenType; 2] = [PLUS, MINUS];
const MATH_HI_PRIORITY_TOKENS: [TokenType; 3] = [DIV, MUL, MODULO];
const UNARY_TOKENS: [TokenType; 3] = [BANG, MINUS, DOLLAR];
const LOGIC_TOKENS: [TokenType; 3] = [AND, OR, XOR];

pub struct Parser<'a> {
    tokens: Vec<Token>,
    cursor: Cursor,
    scribe: &'a mut ErrorScribe,
    exprs: Vec<Expression>,
}

impl WalksCollection<'_, Vec<Token>, Token> for Parser<'_> {
    fn cnt(&self) -> &Cursor { &self.cursor }
    fn mut_cnt(&mut self) -> &mut Cursor { &mut self.cursor }
    fn arr(&self) -> &Vec<Token> { &self.tokens }
}

impl Parser<'_> {
    pub fn into_expressions(self) -> Vec<Expression> { self.exprs }
    pub fn from_tokens(tokens: Vec<Token>, scribe: &mut ErrorScribe) -> Parser {
        Parser { tokens, cursor: Cursor::new(), scribe, exprs: vec![] }
    }

    pub fn parse(&mut self) {
        if self.tokens.is_empty() { return; }
        while self.can_consume() {
            let expr = self.build_expression();
            self.exprs.push(expr);
        }
    }

    fn build_expression(&mut self) -> Expression {
        let expr = self.logic();
        if self.curr_in(&[POUND, POUNDPOUND]) {
            self.cursor.step_fwd();
            QUERY { source: Box::new(expr), op: self.read_prev().clone(), field: Box::new(self.build_expression()) }
        } else if self.curr_in(&[AT, ATAT]) {
            self.cursor.step_fwd();
            APPLICATION { arg: Box::new(expr), op: self.read_prev().clone(), body: Box::new(self.build_expression()) }
        } else { expr }
    }

    fn logic(&mut self) -> Expression {
        let mut expr = self.equality();

        while self.curr_in(&LOGIC_TOKENS) {
            self.cursor.step_fwd();
            expr = LOGIC {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.equality()),
            }
        }
        expr
    }

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
                return BINARY { op, rhs: Box::new(LITERAL(value)), lhs: Box::new(self.unary()) };
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
                ErrorType::PARSER_EXPECTED_LITERAL(EOF)));
            return NOTANEXPR;
        }
        let ttype = &self.tokens.get(self.cursor.get()).unwrap().ttype.clone();
        return match ttype {
            LBRACKET => { self.process_association(AssociationState::MAP) }
            IDENTIFIER(str) => { self.process_assignment(str) }
            LBRACE => { self.process_code_block() }
            RETURN => {
                self.cursor.step_fwd();
                RETURN_EXPR(Box::new(self.build_expression()))
            }
            IT | TI | IDX | FALSE | TRUE | INTEGER(_) | STRING(_) | FLOAT(_) | EOLPRINT | UNDERSCORE => {
                self.cursor.step_fwd();
                LITERAL(self.read_prev().clone())
            }
            LPAREN => {
                self.cursor.step_fwd();
                let expr = self.build_expression();
                self.assert_curr_is(RPAREN);
                self.cursor.step_fwd();
                GROUPING(Box::new(expr))
            }
            _ => {
                self.scribe.annotate_error(Error::on_line(
                    self.read_curr().line, ErrorType::PARSER_UNEXPECTED_TOKEN(ttype.clone())));
                self.cursor.step_fwd();
                NOTANEXPR
            }
        };
    }

    fn process_association(&mut self, association_state: AssociationState) -> Expression {
        self.cursor.step_fwd();
        let mut keys = vec![];
        let mut vals = vec![];
        while self.can_consume() && !self.curr_in(&[RBRACKET]) {
            keys.push(Box::new(self.build_expression()));
            if self.curr_in(&[COLON]) {
                self.cursor.step_fwd();
                vals.push(Box::new(self.build_expression()));
                if self.curr_in(&[COMMA]) {
                    self.cursor.step_fwd();
                    continue;
                }
            }
        }
        self.assert_curr_is(RBRACKET);
        self.cursor.step_fwd();
        ASSOCIATION(keys.iter().map(|k| k.to_owned()).zip(vals).collect())
    }

    fn process_code_block(&mut self) -> Expression {
        self.cursor.step_fwd();
        let mut exprs = vec![];
        while self.can_consume() && !self.curr_in(&[RBRACE]) {
            let expr = self.build_expression();
            self.exprs.push(expr.clone());
            exprs.push(Box::new(expr));
        }
        self.assert_curr_is(RBRACE);
        self.cursor.step_fwd();
        let exprs_to_remove = if self.exprs.len() > exprs.len() {
            self.exprs.len() - exprs.len()
        } else { 0 };
        self.exprs.truncate(exprs_to_remove);
        BLOCK(exprs)
    }

    fn process_assignment(&mut self, str: &String) -> Expression {
        self.cursor.step_fwd();
        if self.can_consume() && self.curr_in(&ASSIGN_TOKENS) {
            self.cursor.step_fwd();
            return if let IDENTIFIER(str) = &self.peek_back(2).ttype {
                VAR_ASSIGN {
                    varname: str.clone(),
                    op: self.read_prev().clone(),
                    varval: Box::new(self.build_expression()),
                }
            } else { // should be unreachable since this fn is only called when the if let is satisfied.
                self.scribe.annotate_error(Error::on_line(self.read_curr().line,
                                                          ErrorType::PARSER_UNEXPECTED_TOKEN(
                                                              self.read_curr().ttype.clone()
                                                          )));
                NOTANEXPR
            };
        }
        VAR_RAW(str.clone())
    }


    fn assert_curr_is(&mut self, ttype: TokenType) -> bool {
        if !self.can_consume() || !self.read_curr().type_equals(&ttype) {
            self.scribe.annotate_error(Error::on_line(
                self.try_read_curr().map(|tok| tok.line).unwrap_or(0),
                ErrorType::PARSER_EXPECTED_TOKEN(ttype)));
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
        self.can_consume() && ttypes.iter().any(
            |tt| self.read_curr().type_equals(tt)
        )
    }
}