use std::collections::HashMap;

use value::Value;
use value::Value::*;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::{Coord, Token};
use crate::lexer::TokenType::*;

pub(crate) mod value;

pub struct Environment {
    pub(crate) scopes: Vec<Scope>,
    pub(crate) last_print_line: usize,
    pub(crate) coord: Coord,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![Scope::new()],
            last_print_line: 0,
            coord: Coord { row: 0, column: 0 },
        }
    }

    pub fn create_or_reuse_scope(&mut self, reuse: bool) {
        if reuse {
            assert!(self.scopes.len() > 1);
            self.scopes.last_mut().unwrap().times_recycled += 1;
        }
        else {
            println!("create {} {}", self.scopes.len() + 1, &self.coord);
            let mut scope = Scope::new();
            scope.coord = self.coord.clone();
            self.scopes.push(scope);
        }
    }

    pub fn destroy_scope(&mut self) {
        assert!(self.scopes.len() > 1);
        println!("destroy {} {}", self.scopes.len() - 1, &self.coord);
        if self.scopes.len() > 1 { self.scopes.pop(); }
    }

    pub fn curr_scope(&self) -> &Scope { self.scopes.last().unwrap() }

    pub fn curr_scope_times_recycled(&self) -> usize { self.curr_scope().times_recycled }


    pub fn try_read(&self, varname: &String) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(varname) { return Some(val); }
        }
        None
    }

    pub fn read(&self, varname: &String, scribe: &mut ErrorScribe) -> &Value {
        match self.try_read(varname) {
            Some(value) => {
                value
            }
            None => {
                scribe.annotate_error(Error::on_coord(&self.coord,
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
        self.write(varname, varval, &Token::new(ASSIGN, self.coord.row, self.coord.column))
    }
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Value>,
    pub(crate) entry_point: String,
    coord: Coord,
    times_recycled: usize,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: Default::default(),
            entry_point: String::from("REPL"),
            coord: Default::default(),
            times_recycled: 0,
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