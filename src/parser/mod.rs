use std::collections::VecDeque;
use std::vec;

use crate::{Cursor, WalksCollection};
use crate::boilerplate::ZERO_COORD;
use crate::environment::value::Value;
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::evaluator::Evaluator;
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
    APPLICABLE_EXPR { params: Box<Expression>, body: Box<Expression> },
    APPLIED_EXPR {
        it_arg: Box<Expression>,
        op: Token,
        body: Box<Expression>,
        contour_args: Option<Vec<Box<Expression>>>,
    },
    RETURN_EXPR(Box<Expression>),
    ASSOCIATION_EXPR(Vec<(Box<Expression>, Box<Expression>)>, bool),
    LIST_DECLARATION_EXPR { input_type: InputType, items: Vec<Box<Expression>>, is_lazy: bool },
    SET_DECLARATION_EXPR { input_type: InputType, items: Vec<Box<Expression>>, is_lazy: bool },
    PULL_EXPR { source: Box<Expression>, op: Token, key: Box<Expression> },
    PUSH_EXPR { obj: Box<Expression>, args: Box<Expression> },
    ARGS(Vec<Box<Expression>>),
    OPTION_EXPR(Box<Expression>),
    UNDERSCORE_EXPR(Coord),
    PRINT_EXPR(Box<Expression>, Option<String>),

    NOTANEXPR,
    // when an expr is desugared into a bigger one this is a way to evaluate once, and carry around,
    // something that appears multiple times in the desugar
    VALUE_WRAPPER(Box<Value>),
}

impl Expression {
    pub fn into_applicable(self) -> Expression {
        match &self {
            VAR_RAW(_, _) | APPLICABLE_EXPR {..} => self,
            _=> {APPLICABLE_EXPR { params: Box::new(ARGS(vec![])), body: Box::new(self) }}
        }
    }

    pub fn ok_or_var_with_applicable(&self, eval: &mut Evaluator) -> Box<Expression> {
        if let VAR_RAW(_, varname) = &self {
            if let Value::LAMBDAVAL { params, body }
                = eval.read_var(varname) {
                return Box::from(APPLICABLE_EXPR { params, body });
            }
        }
        Box::from(self.clone())
    }
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
            BLOCK(exprs) => exprs.first().map(|ex| ex.coord()).unwrap_or(&ZERO_COORD),

            PRINT_EXPR(expr, _) |
            APPLICABLE_EXPR { params: expr, .. } |
            PUSH_EXPR { obj: expr, .. } |
            GROUPING(expr) |
            OPTION_EXPR(expr) |
            RETURN_EXPR(expr) => expr.coord(),

            ASSOCIATION_EXPR(v, _) => v.first().map(|(ex, _)| ex.coord()).unwrap_or(&ZERO_COORD),

            LIST_DECLARATION_EXPR { .. } |
            SET_DECLARATION_EXPR { .. } |
            VALUE_WRAPPER(_) |
            NOTANEXPR => &ZERO_COORD,
        }
    }

    pub fn last_instruction(&self) -> &Expression {
        match self {
            RETURN_EXPR(e) => e.last_instruction(),
            BLOCK(exprs) => exprs.last().map(|e| e.last_instruction()).unwrap_or(self),
            _ => self
        }
    }

    pub fn is_tail_call_optimizable(&self) -> bool {
        if let APPLIED_EXPR { .. } = self.last_instruction() {
            return true;
        }
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
            (VAR_RAW(_, _), VAR_RAW(_, _)) |
            (APPLIED_EXPR { .. }, APPLIED_EXPR { .. }) |
            (ASSOCIATION_EXPR(_, _), ASSOCIATION_EXPR(_, _)) |
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
pub enum InputType {
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
const UNARY_TOKENS: [TokenType; 2] = [NOT, MINUS];
const POSTFIX_UNARY_TOKENS: [TokenType; 2] = [ASBOOL, EXTRACT];
const LOGIC_TOKENS: [TokenType; 3] = [AND, OR, XOR];

pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    pub cursor: Cursor,
    pub scribe: &'a mut ErrorScribe,
    pub exprs: VecDeque<Expression>,
}

impl Parser<'_> {
    pub fn into_expressions(self) -> VecDeque<Expression> { self.exprs }
    pub fn from_tokens(tokens: Vec<Token>, scribe: &mut ErrorScribe) -> Parser {
        Parser { tokens, cursor: Cursor::new(), scribe, exprs: VecDeque::new() }
    }

    pub fn parse(&mut self) {
        if self.tokens.is_empty() { return; }
        dbg!(&self.tokens);
        while self.can_consume() {
            let expr = self.build_expression();
            self.exprs.push_front(expr);
        }
    }

    ///cursor will be after the built expression.
    fn build_expression(&mut self) -> Expression { self.applicable() }

    fn applicable(&mut self) -> Expression {
        // if args expr has bubbled up without being captured by anything else, it means that it's defining an applicable expr.
        let expr = self.pull();
        if let ARGS(_) = expr {
            APPLICABLE_EXPR {
                params: Box::new(expr),
                body: Box::new(self.build_expression()),
            }
        } else { expr }
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
            self.cursor.step_fwd();
            return UNARY { op: self.read_prev().clone(), expr: Box::new(self.unary()) };
        }
        let mut expr = self.print();
        while self.curr_in(&POSTFIX_UNARY_TOKENS) {
            self.cursor.step_fwd();
            expr = UNARY { op: self.read_prev().clone(), expr: Box::new(expr) }
        }
        expr
    }

    fn print(&mut self) -> Expression {
        if self.curr_in(&[DOLLAR]) {
            self.cursor.step_fwd();
            // tagged print: $<a>3  // prints a: 3
            return if self.curr_is_seq(&[LT, IDENTIFIER(String::new()), GT]) {
                self.cursor.step_fwd();
                let tag = if let IDENTIFIER(tag) = &self.read_curr().ttype
                { tag.to_owned() } else { return NOTANEXPR; };
                self.cursor.mov(2);
                PRINT_EXPR(Box::new(self.application()), Some(tag))
                // normal print
            } else { PRINT_EXPR(Box::new(self.application()), None) };
        }
        self.application()
    }

    fn application(&mut self) -> Expression {
        let mut expr = self.primary();
        while self.curr_in(&APPLICATION_TOKENS) {
            self.cursor.step_fwd();
            let op = self.read_prev().clone();
            let body = self.primary();
            let mut contour_args = None;
            if self.curr_in(&[LPAREN]) {
                self.cursor.step_fwd();
                contour_args = self.accumulate(RPAREN);
                self.cursor.step_fwd();
            }
            expr = APPLIED_EXPR {
                it_arg: Box::new(expr),
                op,
                body: Box::new(body),
                contour_args,
            }
        }
        expr
    }


    fn primary(&mut self) -> Expression {
        if !self.can_consume() {
            self.scribe.annotate_error(Error::on_coord(
                if self.cursor.get() == 0 { &self.read_curr().coord } else { &self.read_prev().coord },
                ErrorType::PARSER_EXPECTED_LITERAL(EOF)));
            return NOTANEXPR;
        }
        // check for composite tokens first
        if self.curr_is_seq(&[BANGBANG, LIST]) {
            self.cursor.step_fwd();
            return self.build_association_declaration(AssociationState::LIST, false);
        } else if self.curr_is_seq(&[BANGBANG, SET]) {
            self.cursor.step_fwd();
            return self.build_association_declaration(AssociationState::SET, false);
        } else if self.curr_is_seq(&[BANGBANG, LBRACKET]) {
            self.cursor.step_fwd();
            return self.process_association(false);
        }

        let tok = &self.tokens.get(self.cursor.get()).unwrap().clone();
        return match &tok.ttype {
            LIST => {
                self.build_association_declaration(AssociationState::LIST, true)
            }
            SET => {
                self.build_association_declaration(AssociationState::SET, true)
            }
            QUESTIONMARK => {
                self.cursor.step_fwd();
                OPTION_EXPR(Box::new(self.primary()))
            }
            LBRACKET => { self.process_association(true) }
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
                ARGS(exprs)
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

    fn process_association(&mut self, is_lazy: bool) -> Expression {
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
        ASSOCIATION_EXPR(keys.iter().map(|k| k.to_owned()).zip(vals).collect(), is_lazy)
    }

    fn process_code_block(&mut self) -> Expression {
        self.cursor.step_fwd();

        let mut exprs = vec![];
        while self.can_consume() && !self.curr_in(&[RBRACE]) {
            let expr = self.build_expression();
            if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
            exprs.push(Box::new(expr));
        }
        self.assert_curr_is(RBRACE);
        self.cursor.step_fwd();
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
        VAR_RAW(self.read_prev().coord.clone(), str.clone())
    }


    fn assert_curr_is(&mut self, ttype: TokenType) -> bool {
        if !self.can_consume() || !self.read_curr().type_equals(&ttype) {
            self.scribe.annotate_error(Error::on_coord(
                self.try_read_curr().map(|tok| &tok.coord).unwrap_or(
                    &self.read_prev().coord
                ),
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
    fn build_association_declaration(&mut self, state: AssociationState, is_lazy: bool) -> Expression {
        self.cursor.step_fwd();
        let mut input_state = InputType::ITEMS;
        let expr = self.build_expression();
        if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
        let mut items = vec![Box::new(expr)];
        if self.curr_in(&[RANGE]) {
            input_state = InputType::RANGE;
            self.cursor.step_fwd();
            let expr = self.build_expression();
            if expr.type_equals(&NOTANEXPR) { return NOTANEXPR; }
            items.push(Box::new(expr));
        } else {
            if self.curr_in(&[COMMA]) { self.cursor.step_fwd(); }
            if let Some(mut acc) = self.accumulate(RBRACKET) {
                items.append(&mut acc);
            } else { return NOTANEXPR; }
        }
        self.assert_curr_is(RBRACKET);
        self.cursor.step_fwd();
        if state == AssociationState::LIST {
            LIST_DECLARATION_EXPR { input_type: input_state, items, is_lazy }
        } else { SET_DECLARATION_EXPR { input_type: input_state, items, is_lazy } }
    }
}