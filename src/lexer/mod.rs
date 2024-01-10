use std::clone::Clone;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Iterator;
use std::str::FromStr;

use lazy_static::lazy_static;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::TokenType::*;
use crate::shared::{Cursor, WalksCollection};

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
    LBRACE,
    RBRACE,
    // binary ops
    MINUS,
    PLUS,
    DIV,
    MUL,
    // unary ops
    DOLLAR,
    BANG,
    // primitive types
    IDENTIFIER(String),
    STRING(String),
    INTEGER(i32),
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
    INTO,
    // others
    AT,
    COMMA,
    DOT,
    NOTATOKEN,
    QUESTIONMARK,
    EOF,
}
lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("it", IT),
        ("ti", TI),
        ("idx", IDX),
        ("return", RETURN),
        ("true", TRUE),
        ("false", FALSE),
        ("into", INTO)
]);
}


#[derive(PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub(crate) line: usize,
}


impl Token {
    #[allow(dead_code)]
    pub fn debug(ttype: TokenType) -> Token {
        Token { ttype, line: 0 }
    }

    fn new(ttype: TokenType, line: usize) -> Token {
        Token {
            ttype,
            line,
        }
    }
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Ok(f.write_str(&*format!("{:?}", self.ttype))?)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { f.write_str(self.to_string().as_str()) }
}

pub struct Lexer<'a> {
    source: Vec<char>,
    tokens: Vec<Token>,
    line_number: usize,
    cursor: Cursor,
    scribe: &'a mut ErrorScribe,
}

impl WalksCollection<'_, Vec<char>, char> for Lexer<'_> {
    fn cnt(&self) -> &Cursor { &self.cursor }
    fn mut_cnt(&mut self) -> &mut Cursor { &mut self.cursor }
    fn arr(&self) -> &Vec<char> { &self.source }
}

impl Debug for Lexer<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&*format!("source: {:?}\nc: {}, line: {}\ntoks: {:?}",
                              self.source, self.cursor, self.line_number, self.tokens))
    }
}

impl<'a> Lexer<'_> {
    pub fn from_string(s: String, scribe: &mut ErrorScribe) -> Lexer {
        Lexer {
            source: s.chars().collect(),
            cursor: Cursor::new(),
            tokens: vec![],
            line_number: 1,
            scribe,
        }
    }

    fn consume_next_if_eq(&mut self, other: char) -> bool {
        if self.can_consume() && *self.read_curr() == other {
            self.mut_cnt().step_fwd();
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
                '0'..='9' => { str.push(self.consume().clone()); }
                '.' => {
                    if !is_float && ('0'..='9').contains(&self.read_next()) {
                        str.push(self.consume().clone());
                        is_float = true;
                    } else { break; }
                }
                '_' => { self.consume(); }
                'a'..='z' | 'A'..='z' => {
                    self.scribe.annotate_error(Error::on_line(
                        self.line_number,
                        ErrorType::LEXER_UNEXPECTED_SYMBOL { symbol: peeked.clone() },
                    ));
                    return NOTATOKEN;
                }
                _ => { break; }
            }
        }
        if is_float {
            FLOAT(f64::from_str(str.as_str()).unwrap())
        } else {
            INTEGER(i32::from_str(str.as_str()).unwrap())
        }
    }

    fn skip_comment(&mut self) {
        while self.can_consume() {
            match self.read_curr() {
                '\n' => { break; }
                _ => { self.consume(); }
            }
        }
    }

    fn consume_alphabet(&mut self, starting_symbol: char) -> TokenType {
        let mut str = String::from(starting_symbol);
        while self.can_consume() {
            let peeked = self.read_curr();
            match peeked {
                'a'..='z' | 'A'..='Z' | '_' => { str.push(self.consume().clone()); }
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
        let mut symbol = self.consume();
        while symbol != &starting_symbol {
            str.push(symbol.clone());
            match symbol {
                '\\' => {
                    str.pop();
                    str.push(self.consume().clone());
                }
                '\n' => {
                    self.scribe.annotate_error(
                        Error::on_line(self.line_number, ErrorType::LEXER_BAD_STR_FMT));
                    self.line_number += 1;
                    return STRING(str);
                }
                _ => {
                    if !self.can_consume() {
                        self.scribe.annotate_error(
                            Error::on_line(self.line_number, ErrorType::LEXER_BAD_STR_FMT));
                    }
                }
            }
            symbol = self.consume();
        }
        STRING(str)
    }

    pub fn produce_tokens(&mut self) -> &Vec<Token> {
        while self.can_consume() {
            let symbol = self.consume().clone();
            let ttyp = match symbol {
                ' ' | '\r' | '\t' => continue,
                '\n' => {
                    self.line_number += 1;
                    continue;
                }
                ')' => RPAREN,
                '(' => LPAREN,
                '{' => LBRACE,
                '}' => RBRACE,
                ',' => COMMA,
                '.' => DOT,
                '?' => QUESTIONMARK,
                '@' => AT,
                '-' => if self.consume_next_if_eq('=') { MINUSASSIGN } else { MINUS },
                '+' => if self.consume_next_if_eq('=') { PLUSASSIGN } else { PLUS },
                '*' => if self.consume_next_if_eq('=') { MULASSIGN } else { MUL },
                '/' => if self.consume_next_if_eq('/') {
                    self.skip_comment();
                    continue;
                } else if self.consume_next_if_eq('=') { DIVASSIGN } else { DIV },
                '$' => if self.consume_next_if_eq('$') { EOLPRINT } else { DOLLAR }
                '!' => if self.consume_next_if_eq('=') { UNEQ } else { BANG }
                '=' => if self.consume_next_if_eq('=') {
                    EQ
                } else if self.consume_next_if_eq('>') {
                    MAXASSIGN
                } else if self.consume_next_if_eq('<') {
                    MINASSIGN
                } else {
                    ASSIGN
                },
                '<' => if self.consume_next_if_eq('=') { LTE } else { LT }
                '>' => if self.consume_next_if_eq('=') { GTE } else { GT }
                '\'' | '"' => { self.consume_str(symbol) }
                '0'..='9' => { self.consume_num(symbol) }
                'a'..='z' | 'A'..='Z' | '_' => { self.consume_alphabet(symbol) }
                _ => {
                    self.scribe.annotate_error(
                        Error::on_line(self.line_number, ErrorType::LEXER_UNEXPECTED_SYMBOL { symbol }));
                    NOTATOKEN
                }
            };
            self.tokens.push(Token::new(ttyp, self.line_number));
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