#[cfg(test)]
mod tests {
    use std::iter::zip;
    use crate::errors::ErrorScribe;
    use crate::lexer::{Lexer, Token};
    use crate::lexer::TokenType::*;
    use crate::shared::WalksCollection;

    #[test]
    fn type_equals() {
        let identifier = Token::debug(IDENTIFIER("ID".parse().unwrap()));
        let identifier_2 = Token::debug(IDENTIFIER("IDDD".parse().unwrap()));
        let return_tok = Token::debug(RETURN);
        assert_eq!(identifier, identifier);
        assert_ne!(identifier, identifier_2);
        assert_ne!(identifier, return_tok);
        assert_eq!(return_tok, return_tok);
    }

    #[test]
    fn consume_nothing() {
        let mut es = ErrorScribe::debug();
        let l = Lexer::from_string(String::from(""), &mut es);
        assert!(!l.can_consume());
    }

    #[test]
    fn test_bounds() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("="), &mut es);
        assert!(l.can_consume());
        l.cursor.step_fwd();
        dbg!(&l);
        assert!(!l.can_consume());
        l.cursor.mov(1000);
        assert!(!l.can_consume());
    }

    #[test]
    fn consume_next_if_eq() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("12"), &mut es);
        dbg!(&l);
        assert!(!l.consume_next_if_eq('6'));
        assert!(l.consume_next_if_eq('1'));
        dbg!(&l);
        assert_eq!(*l.consume(), '2');
        assert!(l.consume_next_if_eq('?'));
    }

    #[test]
    fn consume_int() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("000_123_456"), &mut es);
        let tt = l.consume_num('0');
        let int = match tt {
            INTEGER(int) => int,
            _ => { panic!("test failed") }
        };
        assert_eq!(int, 123456);
    }

    #[test]
    fn consume_float() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from(".28"), &mut es);
        let tt = l.consume_num('0');
        let flt = match tt {
            FLOAT(flt) => flt,
            _ => { panic!("test failed") }
        };
        assert!(flt - 0.28 < 0.00001);
    }

    #[test]
    fn false_float() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("345.double()"), &mut es);
        let tt = l.consume_num('2');
        let int = match tt {
            INTEGER(int) => int,
            _ => { panic!("test failed") }
        };
        assert_eq!(2345, int);
    }

    #[test]
    fn consume_str() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from(r#""hello, \"dude\"!""#), &mut es);
        l.cursor.step_fwd();
        let tt = l.consume_str('"');
        let str = match tt {
            STRING(str) => str,
            _ => { panic!("test failed") }
        };
        dbg!(&str);
        assert!(str.eq("hello, \"dude\"!"));
    }

    #[test]
    fn strange_str() {
        let mut es = ErrorScribe::debug();
        let strange_str = "→→↓đ”€€ſæð”€ſ\"";
        let mut l = Lexer::from_string(String::from(strange_str), &mut es);
        let ttype = l.consume_str('"');
        let str = match ttype {
            STRING(str) => str,
            _ => { panic!("test failed") }
        };
        dbg!(&str);
        assert!(str.eq("→→↓đ”€€ſæð”€ſ"));
    }

    #[test]
    fn consume_identifiers() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("t bob"), &mut es);
        let tt = l.consume_alphabet('i');
        assert_eq!(tt, IT);
        l.cursor.mov(2);
        let tt = l.consume_alphabet('b');
        assert_eq!(tt, IDENTIFIER("bob".parse().unwrap()));
    }

    #[test]
    fn refute_bad_identifiers() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("2no idént also-bad _this_too"), &mut es);
        let toks = l.produce_tokens();
        dbg!(toks);
        assert_eq!(toks.len(), 0);
    }

    #[test]
    fn produce_tokens_on_good_syntax() {
        let mut es = ErrorScribe::debug();
        let mut l = Lexer::from_string(String::from("x=2\n3+2\n'hello'>it?\n\n"), &mut es);
        let expected_types = [
            IDENTIFIER("x".parse().unwrap()),
            ASSIGN,
            INTEGER(2),
            INTEGER(3),
            PLUS,
            INTEGER(2),
            STRING("hello".parse().unwrap()),
            GT,
            IT,
            QUESTIONMARK
        ];
        let toks = l.produce_tokens();
        dbg!(toks);
        assert_eq!(toks.len(), expected_types.len());
        for (tt1, tt2) in zip(expected_types, toks) {
            assert_eq!(tt1, tt2.ttype);
        }

    }
}