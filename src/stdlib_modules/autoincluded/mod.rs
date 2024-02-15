use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::environment::value::Value;
use crate::environment::value::Value::*;
use crate::evaluator::Evaluator;
use crate::stdlib_modules::autoincluded::Func::*;

lazy_static! {
    pub static ref AUTOINCL_FUNCS: HashMap<&'static str, Func> = HashMap::from([
        ("is_empty", EMPTY),
        ("to_int", TO_INT)
    ]);
}

#[allow(non_camel_case_types)]
pub enum Func {
    EMPTY,
    TO_INT,
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
        TO_INT => {
            let val = eval.val_queue.pop_back().unwrap();
            let opt = match val {
                INTEGERVAL(_) => Some(Box::from(val)),
                FLOATVAL(flt) => Some(Box::from(INTEGERVAL(flt as i64))),
                BOOLEANVAL(bool) => Some(Box::from(INTEGERVAL(bool as i64))),
                STRINGVAL(str) => {
                    if let Ok(n) = str.parse::<i64>() {
                        Some(Box::from(INTEGERVAL(n)))
                    } else { None }
                }
                _ => None
            };
            OPTIONVAL(opt)
        }
    };
}