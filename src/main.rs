extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod ast;
mod evaluate;
mod parser;
use crate::ast::{Constant, Function, State};
use crate::evaluate::*;
use crate::parser::*;
use pest::Parser;
use std::collections::HashMap;
use std::string::String;

// builds default for state
fn build_default_state() -> State {
    let var_map: HashMap<String, Constant> = HashMap::new();
    let func_map: HashMap<String, Function> = HashMap::new();
    State {
        var_map: var_map,
        func_map: func_map,
    }
}

/*
* Main function for worm interpretter
*/
fn main() {
    let expression = std::fs::read_to_string("worm/easy.c").expect("cannot read file"); //from file
    let pairs = WormParser::parse(Rule::program, &expression).unwrap_or_else(|e| panic!("{}", e));
    let mut state = build_default_state();
    // parses the program into an AST, saves the functions AST in the state to be called upon later
    match parse_program(pairs, &mut state) {
        Ok(()) => (),
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    }
    // calls the main function, return the constant result
    let result = match run_program(&mut state) {
        Ok(res) => Ok(res),
        Err(e) => Err(e),
    };
    println!("{:?}", result);
}
