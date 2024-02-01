use std::fmt;
use std::fmt::{Display, Formatter};

use log::{error, info};

use crate::{Cursor, WalksCollection};
use crate::evaluator::Evaluator;
use crate::evaluator::operation::OperationType::*;
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
    fn indented_string(&self, prefix: &str, idt: usize) -> String {
        let content = match &self {
            LITERAL(tok) => format!("LITERAL ({tok}"),
            UNARY { op, expr } =>
                format!("UNARY ({op}\n{}", expr.indented_string("unary to: ", idt + 1)),
            BINARY { lhs, op, rhs } |
            LOGIC { lhs, op, rhs } =>
                format!("BINARY ({op}\n{}\n{}",
                        lhs.indented_string("binary lhs: ", idt + 1),
                        rhs.indented_string("binary rhs: ", idt + 1)),
            GROUPING(expr) => format!("GROUP (\n{}", expr.indented_string("group: ", idt + 1)),
            VAR_ASSIGN { varname, op, varval } =>
                format!("ASSIGN (var: {varname}, op: {op}\n{}", varval.indented_string("var assign what: ", idt + 1)),
            VAR_RAW(_, str) => format!("VAR RAW ({str}"),
            BLOCK(exprs) =>
                format!("BLOCK (\n{}", exprs.iter().zip(0..)
                    .map(|(e, i)| e.indented_string(format!("block[{i}]: ").as_str(), idt + 1))
                    .collect::<Vec<_>>()
                    .join("\n")),
            APPLIED_EXPR { arg, op, body } =>
                format!("APPLIED ({op},\n{}\n{}",
                        arg.indented_string("applied arg: ", idt + 1),
                        body.indented_string("applied to: ", idt + 1)),
            RETURN_EXPR(expr) => format!("RETURN (\n{}", expr.indented_string("returned: ", idt + 1)),

            ASSOCIATION_EXPR(pairs, lazy) =>
                format!("ASSOCIATION ({},\n{}",
                        if *lazy { "lazy" } else { "eager" },
                        pairs.iter().zip(0..)
                            .map(|(p, i)| format!(
                                "{}\n{}\n",
                                p.0.indented_string(format!("key[{i}]: ").as_str(), idt + 1),
                                p.1.indented_string(format!("val[{i}]: ").as_str(), idt + 1))
                            ).collect::<Vec<_>>()
                            .join(", ")
                ),
            LIST_DECLARATION_EXPR { .. } => "LISTDECL".to_string(),
            SET_DECLARATION_EXPR { .. } => "SETDECL".to_string(),
            PULL_EXPR { source, op, key } =>
                format!("PULL ({op}\n{}\n{}",
                        key.indented_string("pull key: ", idt + 1),
                        source.indented_string("pull from: ", idt + 1)
                ),
            PUSH_EXPR { obj, args } =>
                format!("PUSH (\n{}\n{}",
                        args.indented_string("push what: ", idt + 1),
                        obj.indented_string("push into: ", idt + 1)),
            ARGS(exprs) =>
                format!("ARGS (\n{}", exprs.iter().zip(0..)
                    .map(|(e, i)| e.indented_string(format!("arg[{i}]: ").as_str(), idt + 1))
                    .collect::<Vec<_>>()
                    .join("\n")
                ),
            APPLICABLE_EXPR { params, body: _ignored } =>
                format!("APPLICABLE (\n{}", params.indented_string("params: ", idt + 1)),
            OPTION_EXPR(expr) => format!("OPTION (\n{}", expr.indented_string("option of: ", idt + 1)),
            UNDERSCORE_EXPR(_) => "_".to_string(),
            PRINT_EXPR(expr, tag) =>
                format!("PRINT ({}\n{}",
                        if let Some(t) = tag { format!("<{t}>") } else { "".to_string() },
                        expr.indented_string("print what: ", idt + 1)),
            NOTANEXPR => "NEXPR".to_string(),
            VALUE_WRAPPER(val) => format!("_VAL ({val}"),
        };
        return format!("{}{prefix}\t{}\n{})", "    ".repeat(idt), content, "    ".repeat(idt));
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { f.write_str(&*self.indented_string("",0)) }
}


impl<'a> Evaluator<'a> {
    pub fn dbg(&self) {
        println!("{}", "-".repeat(24));
        println!("{} EVALUATOR SNAPSHOT:", self.env.coord);
        println!("\nOPS:\n-- {}", self.op_queue.iter()
            .map(|o| o.otype.to_string())
            .collect::<Vec<_>>()
            .join("\n-- "));
        println!("\nVALS:\n** {}", self.val_queue.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join("\n** "));
        println!("\nEXPRS:\n>> {}", self.exp_queue.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n>> "));
        println!("{}", "-".repeat(24));
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

impl Display for crate::evaluator::operation::OperationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match &self {
                BINARY_OP(_) => "BINARY",
                OPTIONAL_OP => "OPTIONAL",
                LAZY_LOGIC_OP(_) => "LOGIC (LAZY)",
                LOGIC_OP(_) => "LOGIC",
                UNARY_OP(_) => "UNARY",
                VARASSIGN_OP(_, _) => "VAR ASSIGN",
                SCOPE_CLOSURE_OP(_) => "CLOSURE",
                RETURN_CLEANUP => "RETURN",
                ASSOC_GROWER_SETUPPER_OP(_, _, _) => "ASSOC GROW",
                ASSOC_GROWER_RESOLVER_OP(_, _, _) => "ASSOC GROW (RES)",
                PULL_OP(_) => "PULL",
                BIND_APPLICATION_ARGS_TO_PARAMS_OP(_, _) => "ARGBIND",
                AT_APPLICABLE_RESOLVER_OP => "APPLICATION",
                ASSOC_PUSHER_OP => "PUSH",
                ITERATIVE_PARAM_BINDER(_, _, _) => "@@ITER",
                TI_REBINDER_OP => "TIREBIND",
                PRINT_OP(_) => "PRINT"
            }
        )
    }
}