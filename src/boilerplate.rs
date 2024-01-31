use std::fmt;
use std::fmt::{Display, format, Formatter};
use log::{error, info};

use crate::{Cursor, WalksCollection};
use crate::evaluator::Evaluator;
use crate::lexer::{Coord, Lexer, Token, TokenType};
use crate::lexer::TokenType::*;
use crate::parser::{Expression, Parser};
use crate::parser::Expression::*;

/// display, debug and similar impls for the various structs.

pub(crate) const ZERO_COORD: Coord = Coord { row: 0, column: 0 };

impl Coord {
    pub fn new(row: usize, column: usize) -> Coord { Coord { row, column } }
}

impl WalksCollection<'_, Vec<char>, char> for Lexer<'_> {
    fn cnt(&self) -> &Cursor { &self.cursor }
    fn mut_cnt(&mut self) -> &mut Cursor { &mut self.cursor }
    fn arr(&self) -> &Vec<char> { &self.source }
}

impl WalksCollection<'_, Vec<Token>, Token> for Parser<'_> {
    fn cnt(&self) -> &Cursor { &self.cursor }
    fn mut_cnt(&mut self) -> &mut Cursor { &mut self.cursor }
    fn arr(&self) -> &Vec<Token> { &self.tokens }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let string = match self {
            GT => ">".to_string(),
            LT => "<".to_string(),
            GTE => ">=".to_string(),
            LTE => "<=".to_string(),
            EQ => "==".to_string(),
            UNEQ => "!=".to_string(),
            LPAREN => "(".to_string(),
            RPAREN => ")".to_string(),
            LBRACKET => "[".to_string(),
            RBRACKET => "]".to_string(),
            LBRACE => "{".to_string(),
            RBRACE => "}".to_string(),
            MINUS => "-".to_string(),
            PLUS => "+".to_string(),
            DIV => "/".to_string(),
            MUL => "*".to_string(),
            MODULO => "%".to_string(),
            POW => "**".to_string(),
            AND => "&&".to_string(),
            OR => "||".to_string(),
            XOR => "^".to_string(),
            ASBOOL => "?!".to_string(),
            DOLLAR => "$".to_string(),
            NOT => "!".to_string(),
            BANGBANG => "!!".to_string(),
            IDENTIFIER(str) => format!("id({str})"),
            STRING(str) => format!("str({str})"),
            INTEGER(int) => format!("int({int})"),
            FLOAT(flt) => format!("flt({flt})"),
            IT => "IT".to_string(),
            TI => "TI".to_string(),
            IDX => "IDX".to_string(),
            TRUE => "TRUE".to_string(),
            FALSE => "FALSE".to_string(),
            EOLPRINT => "$$".to_string(),
            RETURN => "RETURN".to_string(),
            ASSIGN => "=".to_string(),
            MAXASSIGN => "MAX=".to_string(),
            MINASSIGN => "MIN=".to_string(),
            MULASSIGN => "*=".to_string(),
            DIVASSIGN => "/=".to_string(),
            PLUSASSIGN => "+=".to_string(),
            MINUSASSIGN => "-=".to_string(),
            MODULOASSIGN => "%=".to_string(),
            POWASSIGN => "**=".to_string(),
            AT => "@".to_string(),
            ATAT => "@@".to_string(),
            BAR => "|".to_string(),
            PULLEXTRACT => "|>>".to_string(),
            PULL => ">>".to_string(),
            PUSH => "<<".to_string(),
            COLON => ":".to_string(),
            COMMA => ",".to_string(),
            UNDERSCORE => "_".to_string(),
            SET => "SET[".to_string(),
            LIST => "LIST[".to_string(),
            RANGE => "RANGE".to_string(),
            QUESTIONMARK => "?".to_string(),
            EXTRACT => "|>".to_string(),
            NOTATOKEN => "NAT".to_string(),
            EOF => "EOF".to_string(),
            DOT => ".".to_string(),
        };
        f.write_str(string.as_str())
    }
}

impl Display for Coord {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&*format!("[{}:{}]", self.row, self.column))
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(format!("{} {}", self.coord, self.ttype).as_str())
    }
}

impl Expression {
    fn indented_string(&self, idt: usize) -> String {
        let content = match &self {
            LITERAL(tok) => format!("LITL({tok})"),
            UNARY { op, expr } =>
                format!("UNRY({op}, {})", expr.indented_string(idt)),
            BINARY { lhs, op, rhs } |
            LOGIC { lhs, op, rhs } =>
                format!("BNRY({op},\n{},\n{})", lhs.indented_string(idt + 1), rhs.indented_string(idt + 1)),
            GROUPING(expr) => format!("({})", expr.indented_string(idt)),
            VAR_ASSIGN { varname, op, varval } =>
                format!("ASGN({varname}, {op}, {})", varval.indented_string(idt)),
            VAR_RAW(_, str) => format!("VRAW({str})"),
            BLOCK(exprs) =>
                format!("BLOK(\n{})", exprs.iter()
                    .map(|e| e.indented_string(idt + 1) + "\n")
                    .collect::<Vec<_>>()
                    .join("\n")),
            APPLIED_EXPR { arg, op, body } =>
                format!("APLD({op},\n{},\n{})", arg.indented_string(idt + 1), body.indented_string(idt + 1)),
            RETURN_EXPR(expr) => format!("RTRN({})", expr.indented_string(idt)),

            ASSOCIATION_EXPR(pairs, lazy) =>
                format!("ASSC({},\n{})",
                        if *lazy { "lazy" } else { "eager" },
                        pairs.iter()
                            .map(|p| format!(
                                "({})->({})",
                                p.0.indented_string(idt + 1),
                                p.1.indented_string(idt + 1))
                            ).collect::<Vec<_>>()
                            .join(", ")
                ),
            LIST_DECLARATION_EXPR { .. } => "LISTDECL".to_string(),
            SET_DECLARATION_EXPR { .. } => "SETDECL".to_string(),
            PULL_EXPR { source, op, key } =>
                format!("PULL({op}, {}\n{})",
                    key.indented_string(idt), source.indented_string(idt+1)
                ),
            PUSH_EXPR { obj, args } =>
                format!("PUSH(\n{}\n{})", args.indented_string(idt+1), obj.indented_string(idt+1)),
            ARGS(exprs) =>
                format!("ARGS(\n{})", exprs.iter()
                    .map(|e|e.indented_string(idt+1))
                    .collect::<Vec<_>>()
                    .join("\n")
                ),
            APPLICABLE_EXPR { params, body: _ignored } =>
                format!("APBL({})", params.indented_string(idt)),
            OPTION_EXPR(expr) => format!("OPTN({})", expr.indented_string(idt)),
            UNDERSCORE_EXPR(_) => "_".to_string(),
            PRINT_EXPR(expr, tag) =>
                format!("PRNT({}{})",
                        if let Some(t) = tag {format!("<{t}>")} else {"".to_string()},
                        expr.indented_string(idt)),
            NOTANEXPR => "NEXPR".to_string(),
            VALUE_WRAPPER(val) => format!("_VAL({val})"),
        };
        return format!("{}{}", "  ".repeat(idt), content);
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { f.write_str(&*self.indented_string(0)) }
}


impl<'a> Evaluator<'a> {
    pub fn dbg(&self) {
        println!("{} EVALUATOR SNAPSHOT:", self.env.coord);
        println!("\nOPS:\n\t ** {}", self.op_queue.iter()
            .map(|o| o.otype.to_string())
            .collect::<Vec<_>>()
            .join("\n\t ** "));
        println!("VALS:\n\t ** {}", self.val_queue.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join("\n\t ** "));
        println!("EXPRS:\n\t ** {}", self.exp_queue.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n\t ** "));
        println!();
    }

    pub fn was_evaluation_consistent(&self) -> bool {
        if !self.scribe.has_errors() &&
            self.op_queue.len() + self.val_queue.len() + self.exp_queue.len() == 0 {
            info!("*** EVERYTHING WAS REGULARLY CONSUMED ***");
            return true;
        }
        if self.scribe.has_errors() {
            error!("*** SCRIBE HAS ERRORS! ***\n");
        }
        if self.env.scopes.len() != 1 {
            error!("*** UNCLOSED SCOPES! ***\n{:?}\n\n", &self.env.scopes);
        }
        if !self.op_queue.is_empty() {
            error!("*** UNCONSUMED OPS! ***\n{:?}\n\n", &self.op_queue);
        }
        if !self.exp_queue.is_empty() {
            error!("*** UNCONSUMED EXPS! ***\n{:?}\n\n", &self.exp_queue);
        }
        if !self.val_queue.is_empty() {
            error!("*** UNCONSUMED VALS! ***\n{:?}\n\n", &self.val_queue);
            for val in &self.val_queue {
                error!("\t{:?}", val);
            }
        }
        false
    }
}