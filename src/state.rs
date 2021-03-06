/*
* state.rs
* contains worm state, which includes the state during execution for variable values
* but also every value in the function, the state is built in parser.rs, analyzed in static_analysis and eveulated in evaluate.rs
* it is THE worm object :)
*/
use crate::ast::*;
use crate::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct AstMap {
    pub func_map: HashMap<String, Function>,
    pub struct_map: HashMap<String, Vec<(String, VarType)>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ExecutionState {
    pub var_map: HashMap<String, Literal>,
    pub var_stack: Vec<(String, u32)>,
    pub stack_lvl: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StaticAnalyzerState {
    pub var_map: HashMap<String, VarType>,
    pub var_stack: Vec<(String, u32)>,
    pub stack_lvl: u32,
}

impl StaticAnalyzerState {
    // increase stack level
    pub fn increment_stack_level(&mut self) {
        self.stack_lvl += 1;
    }

    /* pops all variables off the stack that are on a lower level of the stack */
    pub fn pop_stack(&mut self) {
        self.stack_lvl -= 1;
        if self.var_stack.is_empty() {
            return;
        }
        let mut last_pos = self.var_stack.len() - 1;
        // loop, deleting the variables that are out of scope of the current level
        loop {
            if self.var_stack[last_pos].1 > self.stack_lvl {
                self.var_map.remove(&self.var_stack[last_pos].0);
                self.var_stack.remove(last_pos);
                if last_pos == 0 {
                    break;
                }
                last_pos -= 1;
            } else {
                break;
            }
        }
    }

    // save variable to stack
    pub fn save_variable(&mut self, var_name: String, value: VarType) {
        match self.var_map.get(&var_name) {
            Some(_) => (), // variable was already inserted
            None => self.var_stack.push((var_name.clone(), self.stack_lvl)),
        }
        self.var_map.insert(var_name, value);
    }

    pub fn get_variable(&self, var_name: &str) -> Option<VarType> {
        self.var_map.get(var_name).cloned()
    }
}

impl ExecutionState {
    // increase stack level
    pub fn increment_stack_level(&mut self) {
        self.stack_lvl += 1;
    }

    /* pops all variables off the stack that are on a lower level of the stack */
    pub fn pop_stack(&mut self) {
        self.stack_lvl -= 1;
        if self.var_stack.is_empty() {
            return;
        }
        let mut last_pos = self.var_stack.len() - 1;
        // loop, deleting the variables that are out of scope of the current level
        loop {
            if self.var_stack[last_pos].1 > self.stack_lvl {
                self.var_map.remove(&self.var_stack[last_pos].0);
                self.var_stack.remove(last_pos);
                if last_pos == 0 {
                    break;
                }
                last_pos -= 1;
            } else {
                break;
            }
        }
    }

    // save variable to stack
    pub fn save_variable(&mut self, var_name: String, value: Literal) {
        match self.var_map.get(&var_name) {
            Some(_) => (), // variable was already inserted
            None => self.var_stack.push((var_name.clone(), self.stack_lvl)),
        }
        self.var_map.insert(var_name, value);
    }

    pub fn _print_stack(&mut self) {
        println!(
            "lvl: {} & variable stack: {:?}",
            self.stack_lvl, self.var_stack
        );
    }
}
