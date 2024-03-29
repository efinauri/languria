use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::environment::value::Value;
use crate::evaluator::Evaluator;
use crate::stdlib_modules::Module::*;

mod file;
mod autoincluded;

lazy_static! {
    pub static ref MODULE_NAMES: HashMap<&'static str, Module> = HashMap::from([
        ("autoincluded", AUTOINCL),
        ("file", FILE),
    ]);
}
#[derive(Debug, Clone)]
pub enum Module {
    FILE,
    AUTOINCL,
}

impl Module {
    pub fn invoke(&self, eval: &mut Evaluator, fn_name: &String) -> Value {
        match &self {
            FILE => { file::call(eval, fn_name) }
            AUTOINCL => { autoincluded::call(eval, fn_name) }
        }
    }

    pub fn has_func_name(&self, name: &String) -> bool {
        match &self {
            FILE => { file::FILE_FUNCS.contains_key(name.as_str()) }
            AUTOINCL => { autoincluded::AUTOINCL_FUNCS.contains_key(name.as_str()) }
        }
    }
}