use std::collections::VecDeque;
use std::vec;

use crate::{Cursor, WalksCollection};
use crate::environment::value::Value;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::{Coord, Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::Expression::*;

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
    VAR_RAW(Coord, String),
    BLOCK(Vec<Box<Expression>>),
    APPLIED_EXPR { arg: Box<Expression>, op: Token, body: Box<Expression> },
    RETURN_EXPR(Box<Expression>),
    ASSOCIATION_EXPR(Vec<(Box<Expression>, Box<Expression>)>),
    LIST_DECLARATION_EXPR { range: InputState, items: Vec<Box<Expression>> },
    SET_DECLARATION_EXPR { range: InputState, items: Vec<Box<Expression>> },
    PULL_EXPR { source: Box<Expression>, op: Token, key: Box<Expression> },
    PUSH_EXPR { obj: Box<Expression>, args: Box<Expression> },
    ARGS(Vec<Box<Expression>>),
    APPLICABLE_EXPR { params: Box<Expression>, body: Box<Expression> },
    OPTION_EXPR(Box<Expression>),
    UNDERSCORE_EXPR(Coord),

    NOTANEXPR,
    // when an expr is desugared into a bigger one this is a way to evaluate once, and carry around,
    // something that appears multiple times in the desugar
    VALUE_WRAPPER(Box<Value>),
}

impl Expression {
    pub fn coord(&self) -> &Coord {
        match self {
            LITERAL(tok) => &tok.coord,

            APPLIED_EXPR { op, .. } |
            PULL_EXPR { op, .. } |
            VAR_ASSIGN { op, .. } |
            BINARY { op, .. } |
            LOGIC { op, .. } |
            UNARY { op, .. } => &op.coord,

            UNDERSCORE_EXPR(coord) |
            VAR_RAW(coord, _) => &coord,

            ARGS(exprs) |
            BLOCK(exprs) => exprs.first().map(|ex|ex.coord()).unwrap_or(&Coord::zero()),

            APPLICABLE_EXPR { params: expr, .. } |
            PUSH_EXPR { obj: expr, .. } |
            GROUPING(expr) |
            OPTION_EXPR(expr) |
            RETURN_EXPR(expr) => expr.coord(),

            ASSOCIATION_EXPR(v) => v.first().map(|(ex,_)| ex.coord()).unwrap_or(&Coord::zero()),

            LIST_DECLARATION_EXPR { .. } |
            SET_DECLARATION_EXPR { .. } |
            VALUE_WRAPPER(_) |
            NOTANEXPR => &Coord::zero(),

        }
    }

    pub fn last_instruction(&self) -> &Expression {
        match self {
            RETURN_EXPR(e) => e.last_instruction(),
            BLOCK(exprs) => exprs.last().map(|e|e.last_instruction()).unwrap_or(self),
            _=> self
        }
    }

    pub fn is_tail_call_optimizable(&self) -> bool {
        if let APPLIED_EXPR {..} = self.last_instruction() { return true; }
        false
    }

    pub fn type_equals(&self, other: &Self) -> bool {
        match (self, other) {
            (RETURN_EXPR(expr), other) => {
                expr.type_equals(other)
            }
            (BLOCK(exprs), _) => {
                match exprs.last() {
                    None => { false }
                    Some(e) => { e.type_equals(other) }
                }
            }
            (LITERAL(_), LITERAL(_)) |
            (UNARY { .. }, UNARY { .. }) |
            (BINARY { .. }, BINARY { .. }) |
            (LOGIC { .. }, LOGIC { .. }) |
            (GROUPING(_), GROUPING(_)) |
            (VAR_ASSIGN { .. }, VAR_ASSIGN { .. }) |
            (VAR_RAW(_,_), VAR_RAW(_,_)) |
            (APPLIED_EXPR { .. }, APPLIED_EXPR { .. }) |
            (ASSOCIATION_EXPR(_), ASSOCIATION_EXPR(_)) |
            (PULL_EXPR { .. }, PULL_EXPR { .. }) |
            (PUSH_EXPR { .. }, PUSH_EXPR { .. }) |
            (UNDERSCORE_EXPR(..), UNDERSCORE_EXPR(..)) |
            (NOTANEXPR, NOTANEXPR) => true,
            (_, _) => false
        }
    }
}

#[derive(PartialEq)]
pub enum AssociationState {
    SET,
    LIST,
}

#[derive(PartialEq, Clone, Debug)]
pub enum InputState {
    RANGE,
    ITEMS,
}

const PULL_TOKENS: [TokenType; 2] = [PULL, PULLEXTRACT];
const APPLICATION_TOKENS: [TokenType; 2] = [AT, ATAT];
const ASSIGN_TOKENS: [TokenType; 9] = [ASSIGN, MINASSIGN, MAXASSIGN, PLUSASSIGN, MINUSASSIGN, MULASSIGN, DIVASSIGN, MODULOASSIGN, POWASSIGN];
const EQ_TOKENS: [TokenType; 2] = [UNEQ, EQ];
const CMP_TOKENS: [TokenType; 4] = [GT, LT, GTE, LTE];
const MATH_LO_PRIORITY_TOKENS: [TokenType; 2] = [PLUS, MINUS];
const MATH_MED_PRIORITY_TOKENS: [TokenType; 3] = [DIV, MUL, MODULO];
const MATH_HI_PRIORITY_TOKENS: [TokenType; 1] = [POW];
const UNARY_TOKENS: [TokenType; 4] = [NOT, BANGBANG, MINUS, DOLLAR];
const POSTFIX_UNARY_TOKENS: [TokenType; 2] = [ASBOOL, EXTRACT];
const LOGIC_TOKENS: [TokenType; 3] = [AND, OR, XOR];

pub struct Parser<'a> {
    tokens: Vec<Token>,
    cursor: Cursor,
    scribe: &'a mut ErrorScribe,
    exprs: VecDeque<Expression>,
}

impl WalksCollection<'_, Vec<Token>, Token> for Parser<'_> {
    fn cnt(&self) -> &Cursor { &self.cursor }
    fn mut_cnt(&mut self) -> &mut Cursor { &mut self.cursor }
    fn arr(&self) -> &Vec<Token> { &self.tokens }
}

impl Parser<'_> {
    pub fn into_expressions(self) -> VecDeque<Expression> { self.exprs }
    pub fn from_tokens(tokens: Vec<Token>, scribe: &mut ErrorScribe) -> Parser {
        Parser { tokens, cursor: Cursor::new(), scribe, exprs: VecDeque::new() }
    }

    pub fn parse(&mut self) {
        if self.tokens.is_empty() { return; }
        while self.can_consume() {
            let expr = self.build_expression();
            self.exprs.push_back(expr);
        }
    }

    ///cursor will be after the built expression.
    fn build_expression(&mut self) -> Expression {
        if !self.can_consume() {
            self.scribe.annotate_error(Error::on_coord(
                if self.tokens.is_empty() { &Coord::zero() } else { &self.read_prev().coord },
                ErrorType::PARSER_EXPECTED_LITERAL(EOF)));
            return NOTANEXPR;
        }
        self.pull()
    }

    fn pull(&mut self) -> Expression {
        let mut expr = self.push();
        while self.curr_in(&PULL_TOKENS) {
            self.cursor.step_fwd();
            expr = PULL_EXPR {
                source: Box::new(expr),
                op: self.read_prev().clone(),
                key: Box::new(self.build_expression()),
            }
        }
        expr
    }

    fn push(&mut self) -> Expression {
        let mut expr = self.logic();

        while self.curr_in(&[PUSH]) {
            self.cursor.step_fwd();
            self.assert_curr_is(BAR);
            self.cursor.step_fwd();
            let exprs = if let Some(acc) =
                self.accumulate(BAR) { acc } else { return NOTANEXPR; };
            self.cursor.step_fwd();
            expr = PUSH_EXPR {
                obj: Box::new(expr),
                args: Box::new(ARGS(exprs)),
            }
        }
        expr
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
        let mut expr = self.exponend();

        while self.curr_in(&MATH_MED_PRIORITY_TOKENS) {
            self.cursor.step_fwd();
            expr = BINARY {
                lhs: Box::new(expr),
                op: self.read_prev().clone(),
                rhs: Box::new(self.unary()),
            };
        }
        expr
    }

    fn exponend(&mut self) -> Expression {
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
                let op = self.read_curr().clone();
                self.cursor.mov(2);
                let value = self.read_curr().clone();
                self.cursor.mov(2);
                return BINARY { op, rhs: Box::new(LITERAL(value)), lhs: Box::new(self.unary()) };
            }
            self.cursor.step_fwd();
            return UNARY { op: self.read_prev().clone(), expr: Box::new(self.unary()) };
        }
        let mut expr = self.application();
        if self.curr_in(&POSTFIX_UNARY_TOKENS) {
            self.cursor.step_fwd();
            expr = UNARY { op: self.read_prev().clone(), expr: Box::new(expr) }
        }
        expr
    }

    fn application(&mut self) -> Expression {
        let mut expr = self.primary();
        while self.curr_in(&APPLICATION_TOKENS) {
            self.cursor.step_fwd();
            expr = APPLIED_EXPR {
                arg: Box::new(expr),
                op: self.read_prev().clone(),
                body: Box::new(self.primary()),
            }
        }
        expr
    }


    fn primary(&mut self) -> Expression {
        if !self.can_consume() {
            self.scribe.annotate_error(Error::on_coord(
                if self.tokens.is_empty() { &Coord::zero() } else { &self.read_prev().coord },
                ErrorType::PARSER_EXPECTED_LITERAL(EOF)));
            return NOTANEXPR;
        }
        let tok = &self.tokens.get(self.cursor.get()).unwrap().clone();
        return match &tok.ttype {
            LIST => {
                self.cursor.step_fwd();
                self.build_association_declaration(true)
            }
            SET => {
                self.cursor.step_fwd();
                self.build_association_declaration(false)
            }
            QUESTIONMARK => {
                self.cursor.step_fwd();
                OPTION_EXPR(Box::new(self.primary()))
            }
            LBRACKET => { self.process_association() }
            IDENTIFIER(str) => { self.process_assignment(&str) }
            LBRACE => { self.process_code_block() }
            RETURN => {
                self.cursor.step_fwd();
                RETURN_EXPR(Box::new(self.build_expression()))
            }
            BAR => {
                self.cursor.step_fwd();
                let exprs = if let Some(acc) =
                    self.accumulate(BAR) { acc } else { return NOTANEXPR; };
                self.cursor.step_fwd();
                if !self.curr_in(&APPLICATION_TOKENS) {
                    APPLICABLE_EXPR { params: Box::new(ARGS(exprs)), body: Box::new(self.build_expression()) }
                } else { ARGS(exprs) }
            }
            UNDERSCORE => {
                self.cursor.step_fwd();
                UNDERSCORE_EXPR(tok.coord.clone())
            }
            IT | TI | IDX | FALSE | TRUE | INTEGER(_) | STRING(_) | FLOAT(_) | EOLPRINT => {
                self.cursor.step_fwd();
                LITERAL(self.read_prev().clone())
            }
            LPAREN => {
                self.cursor.step_fwd();
                let expr = self.build_expression();
                if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
                self.assert_curr_is(RPAREN);
                self.cursor.step_fwd();
                GROUPING(Box::new(expr))
            }
            _ => {
                self.scribe.annotate_error(Error::on_coord(
                    &tok.coord, ErrorType::PARSER_UNEXPECTED_TOKEN(tok.ttype.clone())));
                self.cursor.step_fwd();
                NOTANEXPR
            }
        };
    }

    fn process_association(&mut self) -> Expression {
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
        ASSOCIATION_EXPR(keys.iter().map(|k| k.to_owned()).zip(vals).collect())
    }

    fn process_code_block(&mut self) -> Expression {
        self.cursor.step_fwd();
        let mut exprs = vec![];
        while self.can_consume() && !self.curr_in(&[RBRACE]) {
            let expr = self.build_expression();
            if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
            self.exprs.push_back(expr.clone());
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
            } else { unreachable!() };
        }
        VAR_RAW(self.read_curr().coord.clone(), str.clone())
    }


    fn assert_curr_is(&mut self, ttype: TokenType) -> bool {
        if !self.can_consume() || !self.read_curr().type_equals(&ttype) {
            self.scribe.annotate_error(Error::on_coord(
                self.try_read_curr().map(|tok| &tok.coord).unwrap_or(&Coord::new()),
                ErrorType::PARSER_EXPECTED_TOKEN(ttype)));
            return false;
        }
        true
    }

    fn curr_is_seq(&self, ttypes: &[TokenType]) -> bool {
        if self.tokens.is_empty() || ttypes.is_empty() { return false; }
        if !self.can_peek(ttypes.len()) { return false; }
        ttypes.iter()
            .zip(0..)
            .all(|(tt, i)| self.peek(i).type_equals(tt))
    }

    fn curr_in(&self, ttypes: &[TokenType]) -> bool {
        self.can_consume() && ttypes.iter().any(
            |tt| self.read_curr().type_equals(tt)
        )
    }

    fn accumulate(&mut self, stop_ttype: TokenType) -> Option<Vec<Box<Expression>>> {
        let mut items = vec![];
        while !self.curr_in(&[stop_ttype.clone()]) {
            let expr = self.build_expression();
            if expr.type_equals(&NOTANEXPR) { return None; }
            items.push(Box::new(expr));
            if self.curr_in(&[COMMA]) {
                self.cursor.step_fwd();
            }
        }
        Some(items)
    }
    fn build_association_declaration(&mut self, list: bool) -> Expression {
        let mut input_state = InputState::ITEMS;
        let expr = self.build_expression();
        if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
        let mut items = vec![Box::new(expr)];
        if self.curr_in(&[RANGE]) {
            input_state = InputState::RANGE;
            self.cursor.step_fwd();
            let expr = self.build_expression();
            if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
            items.push(Box::new(expr));
        } else {
            self.assert_curr_is(COMMA);
            self.cursor.step_fwd();
            if let Some(acc) = self.accumulate(RBRACKET) {
                items = acc;
            } else { return NOTANEXPR; }
        }
        self.assert_curr_is(RBRACKET);
        self.cursor.step_fwd();
        if list { LIST_DECLARATION_EXPR { range: input_state, items } } else { SET_DECLARATION_EXPR { range: input_state, items } }
    }
}