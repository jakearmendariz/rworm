extern crate pest;
extern crate pretty_env_logger;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;
extern crate log;

mod ast;
mod evaluate;
mod parser;
mod static_analysis;
use crate::ast::{Constant, Function, State};
use crate::parser::*;
use pest::Parser;
use std::collections::HashMap;
use crate::evaluate::*;
use crate::static_analysis::check_program;

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
    pretty_env_logger::init();
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

    match check_program(&mut state) {
        Ok(()) => (),
        Err(e) => {
            println!("static_error: {:?}", e);
            return;
        }
    }

    let result = match run_program(&mut state) {
        Ok(res) => Ok(res),
        Err(e) => Err(e),
    };
    println!("{:?}", result);
    
}
