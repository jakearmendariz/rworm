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
mod static_analysis;

mod display;
mod state;
use crate::evaluate::run_program;
use crate::parser::*;
use crate::state::{ExecutionState, State};
use crate::static_analysis::{log_errors, StaticAnalyzer};
use colored::*;
use pest::Parser;
use std::collections::HashMap;

/*
* Main function for worm interpretter
*/
fn main() {
    let filename = std::env::args().nth(1).expect("expected a filename");
    let file_content = std::fs::read_to_string(filename).expect("cannot read file");
    let pairs = WormParser::parse(Rule::program, &file_content).unwrap_or_else(|e| panic!("{}", e));
    // println!("{:?}", get_position(file_content.clone(), 27));
    let mut state = State::default();
    // parses the program into an AST, saves the functions AST in the state to be called upon later
    match parse_program(pairs, &mut state) {
        Ok(()) => (),
        Err(e) => {
            println!("{} {:?}", "PARSE ERROR:".red().bold(), e);
            return;
        }
    }

    let mut static_analyzer = StaticAnalyzer::default();
    match static_analyzer.check_program(&state) {
        Ok(()) => (),
        Err(e) => {
            log_errors(e, file_content);
            return;
        }
    }
    let mut execution_state = ExecutionState::default();
    match run_program(&mut execution_state, &state) {
        Ok(result) => {
            println!("{}", result);
        }
        Err(e) => {
            println!("{} {:?}", "EXECUTION ERROR:".red().bold(), e);
        }
    };
}
