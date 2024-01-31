use std::clone::Clone;
use std::collections::HashMap;
use std::fmt::{Debug};
use std::iter::Iterator;
use std::str::FromStr;

use lazy_static::lazy_static;

use crate::{Cursor, WalksCollection};
use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::TokenType::*;

mod tests;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    //comparison
    GT,
    LT,
    GTE,
    LTE,
    EQ,
    UNEQ,
    //grouping
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    // binary ops
    MINUS,
    PLUS,
    DIV,
    MUL,
    MODULO,
    POW,
    AND,
    OR,
    XOR,
    // unary ops
    ASBOOL,
    DOLLAR,
    NOT,
    BANGBANG,
    // primitive types
    IDENTIFIER(String),
    STRING(String),
    INTEGER(i64),
    FLOAT(f64),
    // primitive literals and values
    IT,
    TI,
    IDX,
    TRUE,
    FALSE,
    EOLPRINT,
    RETURN,
    // assign
    ASSIGN,
    MAXASSIGN,
    MINASSIGN,
    MULASSIGN,
    DIVASSIGN,
    PLUSASSIGN,
    MINUSASSIGN,
    MODULOASSIGN,
    POWASSIGN,
    // others
    AT,
    ATAT,
    BAR,
    // associations
    PULLEXTRACT,
    PULL,
    PUSH,
    COLON,
    COMMA,
    UNDERSCORE,
    SET,
    LIST,
    RANGE,
    // options
    QUESTIONMARK,
    EXTRACT,
    //
    NOTATOKEN,
    EOF,
    //tbdeleted
    DOT,
}
lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("it", IT),
        ("ti", TI),
        ("idx", IDX),
        ("return", RETURN),
        ("true", TRUE),
        ("false", FALSE),
        ("and", AND),
        ("xor", XOR),
        ("or", OR),
        ("not", NOT)
]);
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Coord {
    pub row: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub ttype: TokenType,
    pub coord: Coord,
}


impl Token {
    pub fn new(ttype: TokenType, row: usize, column: usize) -> Token { Token { ttype, coord: Coord { row, column } } }

    pub fn type_equals(&self, other: &TokenType) -> bool {
        return match (&self.ttype, other) {
            (IDENTIFIER(_), IDENTIFIER(_)) |
            (STRING(_), STRING(_)) |
            (INTEGER(_), INTEGER(_)) |
            (FLOAT(_), FLOAT(_)) => { true }
            (_, _) => { self.ttype.eq(other) }
        };
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub(crate) source: Vec<char>,
    pub(crate) tokens: Vec<Token>,
    pub(crate) current_row: usize,
    current_column: usize,
    pub(crate) cursor: Cursor,
    scribe: &'a mut ErrorScribe,
}

impl<'a> Lexer<'_> {
    fn fwd(&mut self) {
        self.cursor.step_fwd();
        self.current_column += 1;
    }

    fn fwd_consume(&'a mut self) -> &'a char {
        self.current_column += 1;
        self.consume()
    }

    pub fn from_string(s: String, scribe: &mut ErrorScribe) -> Lexer {
        Lexer {
            source: s.chars().collect(),
            cursor: Cursor::new(),
            tokens: vec![],
            current_row: 1,
            current_column: 1,
            scribe,
        }
    }

    fn consume_seq_if_eq(&mut self, seq: &str) -> bool {
        if !self.can_peek(seq.len()) { return false; }
        for (ch, i) in seq.chars().zip(0..) {
            if self.peek(i) != &ch { return false; }
        }
        self.cursor.mov(seq.len() as i32);
        self.current_row += seq.len();
        true
    }

    fn consume_next_if_eq(&mut self, other: char) -> bool {
        if self.can_consume() && *self.read_curr() == other {
            self.fwd();
            return true;
        }
        false
    }

    fn consume_num(&mut self, starting_digit: char) -> TokenType {
        let mut str = String::from(starting_digit);
        let mut is_float = false;
        while self.can_consume() {
            let peeked = self.read_curr();
            match peeked {
                '0'..='9' => { str.push(self.fwd_consume().clone()); }
                '.' => {
                    if !is_float && ('0'..='9').contains(&self.read_next()) {
                        str.push(self.fwd_consume().clone());
                        is_float = true;
                    } else { break; }
                }
                '_' => { self.fwd_consume(); }
                _ => { break; }
            }
        }
        if is_float { FLOAT(f64::from_str(str.as_str()).unwrap()) } else { INTEGER(i64::from_str(str.as_str()).unwrap()) }
    }

    fn skip_comment(&mut self) {
        while self.can_consume() {
            match self.read_curr() {
                '\n' => { break; }
                _ => { self.fwd_consume(); }
            }
        }
    }

    fn consume_alphabet(&mut self, starting_symbol: char) -> TokenType {
        let mut str = String::from(starting_symbol);
        while self.can_consume() {
            let peeked = self.read_curr();
            match peeked {
                'a'..='z' | 'A'..='Z' | '_' => { str.push(self.fwd_consume().clone()); }
                _ => { break; }
            }
        }
        match RESERVED_KEYWORDS.get(str.as_str()) {
            Some(tok) => { (*tok).clone() }
            None => { IDENTIFIER(str) }
        }
    }

    fn consume_str(&mut self, starting_symbol: char) -> TokenType {
        let mut str = String::new();
        let mut symbol = self.fwd_consume();
        while symbol != &starting_symbol {
            str.push(symbol.clone());
            match symbol {
                '\\' => {
                    str.pop();
                    if self.can_consume() {
                        let next = self.fwd_consume();
                        match next {
                            't' => { str.push('\t'); }
                            'n' => { str.push('\n'); }
                            _ => { str.push(next.clone()); }
                        }
                    }
                }
                '\n' => {
                    self.scribe.annotate_error(
                        Error::on_coord(&Coord::new(self.current_row, self.current_column), ErrorType::LEXER_BAD_STR_FMT));
                    self.current_row += 1;
                    self.current_column = 1;
                    return STRING(str);
                }
                _ => {
                    if !self.can_consume() {
                        self.scribe.annotate_error(
                            Error::on_coord(&Coord::new(self.current_row, self.current_column), ErrorType::LEXER_BAD_STR_FMT));
                    }
                }
            }
            symbol = self.fwd_consume();
        }
        STRING(str)
    }

    pub fn produce_tokens(&mut self) -> &Vec<Token> {
        while self.can_consume() {
            let symbol = self.fwd_consume().clone();
            let ttyp = match symbol {
                ' ' | '\r' | '\t' => continue,
                '\n' => {
                    self.current_row += 1;
                    self.current_column = 1;
                    continue;
                }
                ':' => if self.consume_next_if_eq('[') { LIST } else { COLON },
                '[' => if self.consume_next_if_eq(':') { SET } else { LBRACKET },
                ']' => RBRACKET,
                '(' => LPAREN,
                ')' => RPAREN,
                '{' => LBRACE,
                '}' => RBRACE,
                ',' => COMMA,
                '.' => if self.consume_next_if_eq('.') { RANGE } else { DOT },
                '?' => if self.consume_next_if_eq('!') { ASBOOL } else { QUESTIONMARK },
                '_' => UNDERSCORE,
                '%' => MODULO,
                '^' => POW,
                '|' => if self.consume_seq_if_eq(">>") {
                    PULLEXTRACT
                } else if self.consume_next_if_eq('>') {
                    EXTRACT
                } else { BAR },
                '@' => if self.consume_next_if_eq('@') { ATAT } else { AT },
                '-' => if self.consume_next_if_eq('=') { MINUSASSIGN } else { MINUS },
                '+' => if self.consume_next_if_eq('=') { PLUSASSIGN } else { PLUS },
                '*' => if self.consume_next_if_eq('=') { MULASSIGN } else { MUL },
                '/' => if self.consume_next_if_eq('/') {
                    self.skip_comment();
                    continue;
                } else if self.consume_next_if_eq('=') { DIVASSIGN } else { DIV },
                '$' => if self.consume_next_if_eq('$') { EOLPRINT } else { DOLLAR }
                '!' => if self.consume_next_if_eq('=') {
                    UNEQ
                } else if self.consume_next_if_eq('!') {
                    BANGBANG
                } else { NOTATOKEN }
                '=' => if self.consume_next_if_eq('=') {
                    EQ
                } else if self.consume_next_if_eq('>') {
                    MAXASSIGN
                } else if self.consume_next_if_eq('<') {
                    MINASSIGN
                } else if self.consume_next_if_eq('%') {
                    MODULOASSIGN
                } else if self.consume_next_if_eq('^') {
                    POWASSIGN
                } else { ASSIGN }
                '<' => if self.consume_next_if_eq('=') {
                    LTE
                } else if self.consume_next_if_eq('<') {
                    PUSH
                } else { LT }
                '>' => if self.consume_next_if_eq('=') {
                    GTE
                } else if self.consume_next_if_eq('>') {
                    PULL
                } else { GT }
                '\'' | '"' => { self.consume_str(symbol) }
                '0'..='9' => { self.consume_num(symbol) }
                'a'..='z' | 'A'..='Z' => { self.consume_alphabet(symbol) }
                _ => {
                    NOTATOKEN
                }
            };
            if ttyp == NOTATOKEN {
                self.scribe.annotate_error(
                    Error::on_coord(&Coord::new(self.current_row, self.current_column), ErrorType::LEXER_UNEXPECTED_SYMBOL(symbol)));
            }
            self.tokens.push(Token::new(ttyp, self.current_row, self.current_column));
        }
        self.scribe.enact_termination_policy();
        if self.tokens.iter()
            .map(|tok| &tok.ttype)
            .any(|ttype| ttype.eq(&NOTATOKEN)) {
            self.tokens = vec![];
        }
        &self.tokens
    }
}