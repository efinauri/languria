use std::collections::HashMap;

use value::Value;
use value::Value::*;

use crate::errors::{Error, ErrorScribe, ErrorType};
use crate::lexer::{Coord, Token};
use crate::lexer::TokenType::*;
use crate::stdlib_modules::Module;
use crate::stdlib_modules::Module::AUTOINCL;
use crate::user_io::interpret_internal_files;

pub mod value;

pub struct Environment {
    pub imported_modules: Vec<Module>,
    pub stdlib: HashMap<String, Value>,
    pub scopes: Vec<Scope>,
    pub last_print_line: usize,
    pub coord: Coord,
}

impl Environment {
    pub fn module_having(&self, fn_name: &String) -> Option<&Module> {
        self.imported_modules.iter()
            .find(|m|m.has_func_name(fn_name))
    }
}

impl Environment {
    fn load_native_stdlib(&mut self) { interpret_internal_files("stdlib", self); }

    pub fn new() -> Environment {
        let mut res = Environment {
            imported_modules: vec![AUTOINCL],
            stdlib: Default::default(),
            scopes: vec![Scope::new()],
            last_print_line: 0,
            coord: Coord { row: 0, column: 0 },
        };
        res.load_native_stdlib();
        res
    }

    pub fn create_scope(&mut self) {
        // println!("{}create {} {}", " ".repeat(self.scopes.len()), self.scopes.len() + 1, &self.coord);
        let mut scope = Scope::new();
        scope.coord = self.coord.clone();
        self.scopes.push(scope);
    }

    pub fn destroy_scope(&mut self) {
        assert!(self.scopes.len() > 1);
        // println!("{}destroy {} {}", " ".repeat(self.scopes.len()-1), self.scopes.len() - 1, &self.coord);
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn curr_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn curr_scope_times_recycled(&self) -> usize {
        self.curr_scope().times_recycled
    }

    pub fn try_read(&self, varname: &String) -> Option<&Value> {
        if self.stdlib.contains_key(varname) { return self.stdlib.get(varname); }
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(varname) {
                return Some(val);
            }
        }
        None
    }

    pub fn read(&self, varname: &String, scribe: &mut ErrorScribe) -> &Value {
        match self.try_read(varname) {
            Some(value) => value,
            None => {
                scribe.annotate_error(Error::on_coord(
                    &self.coord,
                    ErrorType::EVAL_UNASSIGNED_VAR(varname.clone()),
                ));
                &ERRVAL
            }
        }
    }

    pub fn write(&mut self, varname: &String, varval: &Value, op: &Token) -> Value {
        if self.stdlib.contains_key(varname) { return ERRVAL; }
        let limit = self.scopes.len() - 1;
        for (scope, i) in &mut self.scopes.iter_mut().zip(0..) {
            if scope.variables.contains_key(varname) || i == limit {
                return scope.write(varname, varval, op).clone();
            }
        }
        NOTAVAL
    }

    pub fn write_binding(&mut self, varname: &String, varval: &Value) -> Value {
        self.write(
            varname,
            varval,
            &Token::new(ASSIGN, self.coord.row, self.coord.column),
        )
    }
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Value>,
    pub entry_point: String,
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

    fn read(&self, varname: &String) -> Option<&Value> {
        self.variables.get(varname)
    }

    /// NOTE: particular assigns such as =% implicitly default to = if the literal isn't containing a value already.
    /// should this be considered an error?
    fn write(&mut self, varname: &String, varval: &Value, op: &Token) -> Value {
        let old_val = &self.read(varname);
        let val_to_write = match old_val {
            None => varval.clone(),
            Some(ov) => match op.ttype {
                ASSIGN => varval.clone(),
                MINASSIGN => varval.min_them(ov),
                MAXASSIGN => varval.max_them(ov),
                PLUSASSIGN => ov.plus_them(varval),
                MINUSASSIGN => ov.minus_them(&varval),
                MULASSIGN => varval.mul_them(ov),
                DIVASSIGN => ov.div_them(&varval),
                MODULOASSIGN => ov.modulo_them(&varval),
                UNIONASSIGN => ov.union_them(&varval),
                INTERSECTIONASSIGN => ov.intersection_them(&varval),
                _ => ERRVAL,
            },
        };
        if val_to_write.type_equals(&ERRVAL) {
            return ERRVAL;
        }
        self.variables.insert(varname.clone(), val_to_write.clone());
        val_to_write.clone()
    }
}
