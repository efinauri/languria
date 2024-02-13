use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::environment::value::Value;
use crate::environment::value::Value::*;
use crate::evaluator::Evaluator;
use crate::stdlib_modules::autoincluded::Func::EMPTY;

lazy_static! {
    pub static ref AUTOINCL_FUNCS: HashMap<&'static str, Func> = HashMap::from([
        ("is_empty", EMPTY),
    ]);
}

pub enum Func {
    EMPTY,
}

pub fn call(eval: &mut Evaluator, fn_name: &String) -> Value {
    let f = if let Some(f) = AUTOINCL_FUNCS.get(fn_name.as_str()) { f } else { return ERRVAL; };
    return match f {
        EMPTY => {
            let empty = match eval.val_queue.pop_back().unwrap() {
                STRINGVAL(str) => str.is_empty(),
                ASSOCIATIONVAL(map) => map.map.is_empty(),
                _ => false
            };
            BOOLEANVAL(empty)
        }
    };
}