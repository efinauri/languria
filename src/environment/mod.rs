use std::collections::HashMap;

use value::Value;
use value::Value::*;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::Token;
use crate::lexer::TokenType::*;

pub(crate) mod value;

pub struct Environment {
    scopes: Vec<Scope>,
    pub curr_line: usize,
    last_print_line: usize,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![Scope::new()],
            curr_line: 0,
            last_print_line: 0,
        }
    }

    pub fn create_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.push(scope);
    }
    pub fn destroy_scope(&mut self) { if self.scopes.len() > 1 { self.scopes.pop(); } }
    pub fn curr_scope(&mut self) -> &Scope { self.scopes.last().unwrap() }


    pub fn try_read(&self, varname: &String) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(varname) { return Some(val); }
        }
        None
    }

    pub fn read(&self, varname: &String, scribe: &mut ErrorScribe) -> &Value {
        match self.try_read(varname) {
            Some(value) => {
                value }
            None => {
                scribe.annotate_error(Error::on_line(self.curr_line,
                                                     ErrorType::EVAL_UNASSIGNED_VAR(varname.clone())));
                &ERRVAL
            }
        }
    }

    pub fn write(&mut self, varname: &String, varval: &Value, op: &Token) -> Value {
        let limit = self.scopes.len() - 1;
        for (scope, i) in &mut self.scopes.iter_mut().zip(0..) {
            if scope.variables.contains_key(varname) || i == limit {
                return scope.write(varname, varval, op).clone();
            }
        }
        NOTAVAL
    }

    pub fn write_binding(&mut self, varname: &String, varval: &Value) -> Value {
        self.write(varname, varval, &Token::new(ASSIGN, self.curr_line))
    }
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Value>,
    entry_point: String,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: Default::default(),
            entry_point: String::from("REPL"),
        }
    }

    fn read(&self, varname: &String) -> Option<&Value> { self.variables.get(varname) }

    fn write(&mut self, varname: &String, varval: &Value, op: &Token) -> Value {
        let old_val = &self.read(varname);
        let val_to_write = match old_val {
            None => { varval.clone() }
            Some(ov) => {
                match op.ttype {
                    ASSIGN => { varval.clone() }
                    MINASSIGN => { varval.min_them(ov) }
                    MAXASSIGN => { varval.max_them(ov) }
                    PLUSASSIGN => { varval.plus_them(ov) }
                    MINUSASSIGN => { ov.minus_them(&varval) }
                    MULASSIGN => { varval.mul_them(ov) }
                    DIVASSIGN => { ov.div_them(&varval) }
                    POWASSIGN => { ov.pow_them(&varval) }
                    MODULOASSIGN => { ov.modulo_them(&varval) }
                    _ => { ERRVAL }
                }
            }
        };
        if val_to_write.type_equals(&ERRVAL) { return ERRVAL; }
        self.variables.insert(varname.clone(), val_to_write.clone());
        val_to_write.clone()
    }
}

pub fn print_eol(env: &mut Environment, line: &usize) -> Value {
    let start_new_line = env.last_print_line != *line;
    env.last_print_line = *line;
    let to_print = format!("[{}:{}]", env.curr_scope().entry_point, line);
    if start_new_line { print!("\n{}", to_print); } else { print!(" {} ", to_print); }
    NOTAVAL
}
