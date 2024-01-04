use std::clone::Clone;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Iterator;
use std::str::FromStr;

use lazy_static::lazy_static;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::TokenType::*;
use crate::shared::{Counter, WalksCollection};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    DOLLAR,
    AT,
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
    IT,
    TI,
    IDX,
    COMMA,
    DOT,
    BANG,
    ASSIGN,
    NOTATOKEN,
    RETURN,
    QUESTIONMARK,
    TRUE,
    FALSE,
    EOF,
}
lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("it", IT),
        ("ti", TI),
        ("idx", IDX),
        ("return", RETURN),
        ("true", TRUE),
        ("false", FALSE)
]);
}


#[derive(PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub(crate) line: usize,
}


impl Token {
    pub fn from_debug(ttype: TokenType) -> Token {
        Token {
            ttype,
            line: 0,
        }
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
        f.write_str(&*format!("{:?}", self.ttype))?;
        Ok(())
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { f.write_str(self.to_string().as_str()) }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: Vec<char>,
    tokens: Vec<Token>,
    cursor: usize,
    counter: Counter,
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
            cursor: 1,
            scribe,
        }
    }

    fn peek_or_err(&mut self, amount: usize) -> char {
        let result = self.peek(amount).clone();
        if !result.is_ascii() {
            self.counter.mov(result.len_utf8() as i32);
            self.scribe.annotate_error(
                Error::on_line(self.cursor, ErrorType::NONASCIICHARACTER {
                    symbol: result
                }))
        }
        result
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
            let peeked = self.peek_or_err(0);
            match peeked {
                '0'..='9' => { str.push(self.consume().clone()); }
                '.' => {
                    if !is_float && ('0'..='9').contains(&self.peek_or_err(1)) {
                        str.push(self.consume().clone());
                        is_float = true;
                    } else { break; }
                }
                '_' => { self.consume(); }
                _ => { break; }
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
            let peeked = self.peek_or_err(0);
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
                        Error::on_line(self.cursor, ErrorType::BADSTRFMT));
                    self.cursor += 1;
                    return STRING(str);
                }
                _ => {
                    if !self.can_consume() {
                        self.scribe.annotate_error(
                            Error::on_line(self.cursor, ErrorType::BADSTRFMT));
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
                    self.cursor += 1;
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
                '?' => QUESTIONMARK,
                '$' => DOLLAR,
                '@' => AT,
                '!' => if self.consume_next_if_eq('=') { UNEQ } else { BANG }
                '=' => if self.consume_next_if_eq('=') { EQ } else { ASSIGN }
                '<' => if self.consume_next_if_eq('=') { LTE } else { LT }
                '>' => if self.consume_next_if_eq('=') { GTE } else { GT }
                '\'' | '"' => { self.consume_str(symbol) }
                '0'..='9' => { self.consume_num(symbol) }
                'a'..='z' | 'A'..='Z' | '_' => { self.consume_alphabet(symbol) }
                _ => {
                    self.scribe.annotate_error(
                        Error::on_line(self.cursor, ErrorType::UNEXPECTEDCHAR { symbol }));
                    NOTATOKEN
                }
            };
            self.tokens.push(Token::new(ttyp, self.cursor));
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
    use crate::lexer::TokenType::*;
    use crate::shared::WalksCollection;

    #[test]
    fn consume() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("="), &mut es);
        assert_eq!(l.can_consume(), true);
        l.counter.step_fwd();
        assert_eq!(l.can_consume(), false);
    }

    #[test]
    fn consume_next_if_eq() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from("12"), &mut es);
        assert_eq!(l.consume_next_if_eq('6'), false);
        assert_eq!(l.consume_next_if_eq('1'), true);
        dbg!(&l);
        assert_eq!(*l.consume(), '2');
        assert_eq!(l.consume_next_if_eq('?'), false);
    }

    #[test]
    fn consume_str() {
        let mut es = ErrorScribe::new();
        let mut l = Lexer::from_string(String::from(r#""hello, \"dude\"!""#), &mut es);
        l.counter.step_fwd();
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
        let mut l = Lexer::from_string(String::from("t bob"), &mut es);
        let tt = l.consume_alphabet('i');
        assert_eq!(tt, IT);
        l.counter.mov(2);
        let tt = l.consume_alphabet('b');
        assert_eq!(tt, IDENTIFIER("bob".parse().unwrap()));
    }
}