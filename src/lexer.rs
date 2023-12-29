use std::clone::Clone;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Iterator;
use std::str::FromStr;
use lazy_static::lazy_static;

use crate::errors::{Error, ErrorScribe};
use crate::lexer::TokenType::{ASSIGN, BANG, COMMA, DIV, DOT, EQ, GT, GTE, LBRACE, LPAREN, LT, LTE, MINUS, MUL, NOTATOKEN, PLUS, RBRACE, RPAREN, UNEQ};

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    GT,
    LT,
    GTE,
    LTE,
    EQ,
    UNEQ,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    MINUS,
    PLUS,
    DIV,
    MUL,
    IDENTIFIER(String),
    STRING(String),
    INTEGER(i32),
    STRUCT,
    IF,
    ELSE,
    ITER,
    FN,
    IT,
    IDX,
    COMMA,
    DOT,
    BANG,
    EOF,
    ASSIGN,
    NOTATOKEN,
    RETURN,
    QUESTIONMARK,
    ENUM,
}

lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
    ("if", TokenType::IF),
    ("else", TokenType::ELSE),
    ("iter", TokenType::ITER),
    ("it", TokenType::IT),
    ("idx", TokenType::IDX),
    ("fn", TokenType::FN),
    ("return", TokenType::RETURN),
    ("struct", TokenType::STRUCT),
    ("enum", TokenType::ENUM),
]);
}

pub struct Token {
    ttype: TokenType,
    line: usize,
    line_offset: usize,
}

impl Token {
    fn new(ttype: TokenType, line: usize, line_offset: usize) -> Token {
        Token {
            ttype,
            line,
            line_offset,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("[{}, {}] {:?}", self.line, self.line_offset, self.ttype))?;
        Ok(())
    }
}


pub struct Lexer<'a> {
    source: String,
    tokens: Vec<Token>,
    n_line: usize,
    n_pos: usize,
    n_offset: usize,
    scribe: &'a mut ErrorScribe,
}

impl<'a> Lexer<'_> {
    pub fn from_string(s: String, scribe: &mut ErrorScribe) -> Lexer {
        Lexer {
            source: s,
            tokens: vec![],
            n_line: 0,
            n_pos: 0,
            n_offset: 0,
            scribe,
        }
    }

    fn can_peek(&self, amount: usize) -> bool {
        self.n_pos + amount < self.source.len()
    }
    fn get_current(&self) -> char { self.peek(0) }
    fn peek(&self, amount: usize) -> char {
        self.source.chars().nth(amount + self.n_pos - 1)
            .expect(&*format!("index error during source parsing: {}[{}]", self.source, self.n_pos))
    }
    fn can_consume(&self) -> bool { self.can_peek(0) }
    fn advance_pos(&mut self, amount: usize) {
        self.n_pos += amount;
        self.n_offset += amount;
    }
    fn consume(&mut self) -> char {
        self.advance_pos(1);
        self.get_current()
    }
    fn next_eq(&mut self, other: &str) -> bool {
        if self.n_pos + other.len() <= self.source.len()
            && self.source[self.n_pos..self.n_pos + other.len()].eq(other) {
            self.advance_pos(other.len());
            return true;
        }
        false
    }

    fn consume_int(&mut self, starting_digit: char) -> TokenType {
        let mut str = String::from(starting_digit);
        while self.can_consume() {
            let peeked = self.peek(1);
            match peeked {
                '0'..='9' => { str.push(self.consume()); }
                '_' => { self.consume(); }
                _ => break
            }
        }
        TokenType::INTEGER(i32::from_str(str.as_str()).unwrap())
    }

    fn consume_alphabet(&mut self, starting_symbol: char) -> TokenType {
        let mut str = String::from(starting_symbol);
        while self.can_consume() {
            let peeked = self.peek(1);
            match peeked {
                'a'..='z' | 'A'..='Z' | '_' => { str.push(self.consume()); }
                _ => { break; }
            }
        }
        match RESERVED_KEYWORDS.get(str.as_str()) {
            Some(tok) => { (*tok).clone() }
            None => { TokenType::IDENTIFIER(str) }
        }
    }

    fn consume_str(&mut self, starting_symbol: char) -> TokenType {
        let mut starting_symbol_pos = self.n_pos - 1;
        let mut str = String::new();
        let mut symbol = self.consume();
        while symbol != starting_symbol {
            str.push(symbol);
            match symbol {
                '\\' => {
                    str.pop();
                    str.push(self.consume());
                }
                '\n' => {
                    self.scribe.annotate_error(
                        Error::BADSTRFMT {
                            line: self.n_line,
                            line_offset: starting_symbol_pos,
                        });
                    return TokenType::STRING(str);
                }
                _ => {}
            }
            symbol = self.consume();
        }
        TokenType::STRING(str)
    }

    pub fn produce_tokens(&mut self) -> &Vec<Token> {
        while self.can_consume() {
            let symbol = self.consume();
            let ttyp = match symbol {
                ' ' | '\r' | '\t' => continue,
                '\n' => {
                    self.n_line += 1;
                    self.n_offset = 0;
                    continue;
                }
                ')' => RPAREN,
                '(' => LPAREN,
                '{' => LBRACE,
                '}' => RBRACE,
                ',' => COMMA,
                '.' => DOT,
                '-' => MINUS,
                '+' => PLUS,
                '*' => MUL,
                '/' => DIV,
                '!' => if self.next_eq("=") { UNEQ } else { BANG }
                '=' => if self.next_eq("=") { EQ } else { ASSIGN }
                '<' => if self.next_eq("=") { LTE } else { LT }
                '>' => if self.next_eq("=") { GTE } else { GT }
                '\'' | '"' => { self.consume_str(symbol) }
                '0'..='9' => { self.consume_int(symbol) }
                'a'..='z' | 'A'..='Z' | '_' => { self.consume_alphabet(symbol) }
                _ => {
                    self.scribe.annotate_error(
                        Error::UNKNOWNSYMBOL {
                            line: self.n_line,
                            line_offset: self.n_offset,
                            symbol,
                        }
                    );
                    NOTATOKEN
                }
            };
            self.tokens.push(Token::new(ttyp, self.n_line, self.n_offset - 1));
        }
        &self.tokens
    }
}


#[cfg(test)]
mod tests {
    use crate::errors::ErrorScribe;
    use crate::lexer::{Lexer, TokenType};

    #[test]
    fn consume() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("="), &mut es);
        assert_eq!(l.can_consume(), true);
        l.advance_pos(1);
        assert_eq!(l.can_consume(), false);
    }

    #[test]
    fn next_eq() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("12345"), &mut es);
        assert_eq!(l.next_eq("12345678"), false);
        assert_eq!(l.next_eq("0"), false);
        assert_eq!(l.next_eq("12"), true);
        assert_eq!(l.consume(), '3');
        assert_eq!(l.next_eq("45"), true);
        assert_eq!(l.next_eq("?"), false);
    }

    #[test]
    fn consume_str() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from(r#""hello, \"dude\"!""#), &mut es);
        l.advance_pos(1);
        let tt = l.consume_str('"');
        let str = match tt {
            TokenType::STRING(str) => str,
            _ => { panic!("test failed") }
        };
        dbg!(&str);
        assert_eq!(str.eq("hello, \"dude\"!"), true);
    }

    #[test]
    fn consume_int() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("000_123_456"), &mut es);
        let tt = l.consume_int('0');
        let int = match tt {
            TokenType::INTEGER(int) => int,
            _ => { panic!("test failed") }
        };
        dbg!(int);
        assert_eq!(int, 123456);
    }

    #[test]
    fn consume_alphabet() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("f bob"), &mut es);
        let tt = l.consume_alphabet('i');
        assert_eq!(tt, TokenType::IF);
        l.advance_pos(2);
        let tt = l.consume_alphabet('b');
        assert_eq!(tt, TokenType::IDENTIFIER("bob".parse().unwrap()));
    }
}