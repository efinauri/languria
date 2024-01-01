use std::clone::Clone;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Iterator;
use std::str::FromStr;

use lazy_static::lazy_static;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::TokenType::{ASSIGN, BANG, COMMA, DIV, DOT, ELSE, ENUM, EQ, FALSE, FLOAT, FN, GT, GTE, IDENTIFIER, IDX, IF, INTEGER, IT, ITER, LBRACE, LPAREN, LT, LTE, MINUS, MUL, NOTATOKEN, PLUS, RBRACE, RETURN, RPAREN, STRING, STRUCT, TRUE, UNEQ};
use crate::shared::{Counter, WalksCollection};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
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
    FLOAT(f64),
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
    ASSIGN,
    NOTATOKEN,
    RETURN,
    QUESTIONMARK,
    ENUM,
    TRUE,
    FALSE,
}
lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("if", IF),
        ("else", ELSE),
        ("iter", ITER),
        ("it", IT),
        ("idx", IDX),
        ("fn", FN),
        ("return", RETURN),
        ("struct", STRUCT),
        ("enum", ENUM),
        ("true", TRUE),
        ("false", FALSE)
]);
}


#[derive(PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    line: usize,
    line_offset: usize,
}


impl Token {
    pub fn from_debug(ttype: TokenType) -> Token {
        Token {
            ttype,
            line: 0,
            line_offset: 0,
        }
    }
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
        f.write_str(&*format!("{:?}", self.ttype))?;
        Ok(())
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { f.write_str(self.to_string().as_str()) }
}


pub struct Lexer<'a> {
    source: Vec<char>,
    tokens: Vec<Token>,
    n_line: usize,
    pub counter: Counter,
    pub n_offset: usize,
    scribe: &'a mut ErrorScribe,
}

impl WalksCollection<'_, Vec<char>, char> for Lexer<'_> {
    fn cnt(&self) -> &Counter { &self.counter }
    fn mut_cnt(&mut self) -> &mut Counter { &mut self.counter }
    fn arr(&self) -> &Vec<char> { &self.source }
}

impl<'a> Lexer<'_> {
    pub fn from_string(s: String, scribe: &mut ErrorScribe) -> Lexer {
        Lexer {
            source: s.chars().collect(),
            counter: Counter::new(),
            tokens: vec![],
            n_line: 0,
            n_offset: 0,
            scribe,
        }
    }

    fn peek_or_err(&mut self, amount: usize) -> char {
        let result = self.peek(amount).clone();
        if !result.is_ascii() {
            self.advance_pos(result.len_utf8());
            self.scribe.annotate_error(
                Error::from_lexer_fault(&self, ErrorType::NONASCIICHARACTER {
                    symbol: result
                }))
        }
        result
    }
    fn advance_pos(&mut self, amount: usize) {
        self.counter.mov(amount as i32);
        self.n_offset += amount;
    }

    fn consume_next_if_eq(&mut self, other: char) -> bool {
        if self.can_peek(1) && *self.peek(1) == other {
            self.mut_cnt().step_fwd();
            return true;
        }
        false
    }

    fn consume_num(&mut self, starting_digit: char) -> TokenType {
        let mut str = String::from(starting_digit);
        let mut is_float = false;
        while self.can_consume() {
            let peeked = self.peek_or_err(1);
            match peeked {
                '0'..='9' => { str.push(self.consume().clone()); }
                '.' => {
                    if !is_float && ('0'..='9').contains(&self.peek_or_err(2)) {
                        is_float = true;
                    } else { break; }
                }
                '_' => { self.consume(); }
                _ => break
            }
        }
        if is_float {
            FLOAT(f64::from_str(str.as_str()).unwrap())
        } else {
            INTEGER(i32::from_str(str.as_str()).unwrap())
        }
    }

    fn consume_alphabet(&mut self, starting_symbol: char) -> TokenType {
        let mut str = String::from(starting_symbol);
        while self.can_consume() {
            let peeked = self.peek_or_err(1);
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
                        Error::from_lexer_fault(&self, ErrorType::BADSTRFMT));
                    return STRING(str);
                }
                _ => {
                    if !self.can_consume() {
                        self.scribe.annotate_error(
                            Error::from_lexer_fault(&self, ErrorType::BADSTRFMT));
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
                '!' => if self.consume_next_if_eq('=') { UNEQ } else { BANG }
                '=' => if self.consume_next_if_eq('=') { EQ } else { ASSIGN }
                '<' => if self.consume_next_if_eq('=') { LTE } else { LT }
                '>' => if self.consume_next_if_eq('=') { GTE } else { GT }
                '\'' | '"' => { self.consume_str(symbol) }
                '0'..='9' => { self.consume_num(symbol) }
                'a'..='z' | 'A'..='Z' | '_' => { self.consume_alphabet(symbol) }
                _ => {
                    self.scribe.annotate_error(
                        Error::from_lexer_fault(&self, ErrorType::UNEXPECTEDTOKEN { symbol }));
                    NOTATOKEN
                }
            };
            self.tokens.push(Token::new(ttyp, self.n_line, self.n_offset));
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


#[cfg(test)]
mod tests {
    //TODO more comprehensive test cases
    // - non unicode chars
    use crate::errors::ErrorScribe;
    use crate::lexer::Lexer;
    use crate::lexer::TokenType::{FLOAT, IDENTIFIER, IF, INTEGER, STRING};
    use crate::shared::WalksCollection;

    #[test]
    fn consume() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("="), &mut es);
        assert_eq!(l.can_consume(), true);
        l.advance_pos(1);
        assert_eq!(l.can_consume(), false);
    }

    #[test]
    fn consume_next_if_eq() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("12"), &mut es);
        assert_eq!(l.consume_next_if_eq('6'), false);
        assert_eq!(l.consume_next_if_eq('1'), true);
        assert_eq!(*l.consume(), '2');
        assert_eq!(l.consume_next_if_eq('?'), false);
    }

    #[test]
    fn consume_str() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from(r#""hello, \"dude\"!""#), &mut es);
        l.advance_pos(1);
        let tt = l.consume_str('"');
        let str = match tt {
            STRING(str) => str,
            _ => { panic!("test failed") }
        };
        dbg!(&str);
        assert_eq!(str.eq("hello, \"dude\"!"), true);
    }

    #[test]
    fn consume_int() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("000_123_456"), &mut es);
        let tt = l.consume_num('0');
        let int = match tt {
            INTEGER(int) => int,
            _ => { panic!("test failed") }
        };
        dbg!(int);
        assert_eq!(int, 123456);
    }

    #[test]
    fn consume_float() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from(".28"), &mut es);
        let tt = l.consume_num('0');
        let flt = match tt {
            FLOAT(flt) => flt,
            _ => { panic!("test failed") }
        };
        dbg!(flt);
        assert_eq!(flt - 0.28 < 0.001, true);

        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("345.double()"), &mut es);
        let tt = l.consume_num('2');
        let int = match tt {
            INTEGER(int) => int,
            _ => { panic!("test failed") }
        };
        dbg!(int);
        assert_eq!(2345, int);
    }

    #[test]
    fn consume_alphabet() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("f bob"), &mut es);
        let tt = l.consume_alphabet('i');
        assert_eq!(tt, IF);
        l.advance_pos(2);
        let tt = l.consume_alphabet('b');
        assert_eq!(tt, IDENTIFIER("bob".parse().unwrap()));
    }
}