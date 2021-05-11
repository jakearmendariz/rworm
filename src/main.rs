extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod ast;
mod evaluate;
mod parser;
use crate::ast::{Constant, Function, State};
use crate::parser::*;
use pest::Parser;
use std::collections::HashMap;
use crate::evaluate::*;


// builds default for state
fn build_default_state() -> State {
    State {
        var_map: HashMap::new(),
        func_map: HashMap::new(),
        var_stack:Vec::new(),
        stack_lvl:0,
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

    // match type_evaluation(&mut state) {

    // }

    let result = match run_program(&mut state) {
        Ok(res) => Ok(res),
        Err(e) => Err(e),
    };
    println!("{:?}", result);
    
}
