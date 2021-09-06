/*
* start of worm execution
* opens the file, passes into worm strucutres to evaluate
* allows for flags for just compilation, or for a complete run
* NOTE the compilation compiles to binary, but the binary is not for a machine, but can be evaulated by evaluate.rs
*/
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate colored;
extern crate lazy_static;

mod ast;
mod evaluate;
mod parser;
// mod static_analysis;
// mod intern;
mod seperate;
mod display;
mod state;
mod ordering;
use crate::state::{State, ExecutionState};
use crate::evaluate::*;
use crate::parser::*;
// use crate::static_analysis::*;
// use crate::intern::*;
use crate::seperate::*;
use colored::*;
use pest::Parser;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

// builds default for state
fn build_default_state() -> State {
    State {
        var_map: HashMap::new(),
        func_map: HashMap::new(),
        var_stack: Vec::new(),
        fn_list: Vec::new(),
        stack_lvl: 0,
    }
}


fn build_default_execution_state() -> ExecutionState {
    ExecutionState {
        var_map: HashMap::new(),
        var_stack: Vec::new(),
        stack_lvl: 0,
    }
}

/*
* Main function for worm interpretter
*/
fn main() {
    let first_arg = std::env::args().nth(1).expect("expected a filename");
    // compile and execute
    let filename = &format!("{}", first_arg)[..];
    let expression = std::fs::read_to_string(filename).expect("cannot read file"); //from file
    let pairs =
        WormParser::parse(Rule::program, &expression).unwrap_or_else(|e| panic!("{}", e));
    let mut state = build_default_state();
    // parses the program into an AST, saves the functions AST in the state to be called upon later
    match parse_program(pairs, &mut state) {
        Ok(()) => (),
        Err(e) => {
            println!("{} {:?}", "PARSE ERROR:".red().bold(), e);
            return;
        }
    }

    let mut exec = build_default_execution_state();
    match exec.check_program(&state) {
        Ok(()) => (),
        Err(e) => {
            println!("{} {}", "STATIC ERROR:".red().bold(), e);
            return;
        }
    }
    match run_program(&mut state) {
        Ok(result) => {
            println!("{}", result);
        }
        Err(e) => {
            println!("{} {:?}", "EXECUTION ERROR:".red().bold(), e);
        }
    };
}
