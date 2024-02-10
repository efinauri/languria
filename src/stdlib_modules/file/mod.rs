use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use lazy_static::lazy_static;

use crate::environment::value::Value;
use crate::environment::value::Value::*;
use crate::evaluator::Evaluator;
use crate::stdlib_modules::file::Func::READFILE;

lazy_static! {
    pub static ref FILE_FUNCS: HashMap<&'static str, Func> = HashMap::from([
        ("read_file", READFILE),
    ]);
}

pub enum Func {
    READFILE,
}

pub fn call(eval: &mut Evaluator, fn_name: &String) -> Value {
    let f = if let Some(f) = FILE_FUNCS.get(fn_name.as_str()) { f } else { return ERRVAL; };
    return match f {
        READFILE => {
            let path_val = eval.val_queue.pop_back().unwrap();
            let path = if let STRINGVAL(str) = path_val { str } else { "".to_string() };
            let mut contents = String::new();
            File::open(path).unwrap().read_to_string(&mut contents).expect("TODO: panic message");
            STRINGVAL(contents)
        }
    };
}