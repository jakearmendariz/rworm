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
mod error_handling;
mod evaluate;
mod parser;
mod static_analysis;

mod display;
mod state;
use crate::error_handling::log_errors;
use crate::evaluate::run_program;
use crate::parser::*;
use crate::state::{AstMap, ExecutionState, StaticAnalyzerState};
use colored::*;
use pest::Parser;
use std::collections::HashMap;

/*
* Main function for worm interpretter
*/
fn main() {
    let filename = std::env::args().nth(1).expect("expected a filename");
    let file_content = std::fs::read_to_string(filename).expect("cannot read file");
    let parsed_program =
        WormParser::parse(Rule::program, &file_content).unwrap_or_else(|e| panic!("{}", e));
    let mut ast = AstMap::default();
    // parses the program into an AST, saves the functions AST in the state to be called upon later
    match parse_program(parsed_program, &mut ast) {
        Ok(()) => (),
        Err(e) => {
            println!("{} {:?}", "PARSE ERROR:".red().bold(), e);
            return;
        }
    }

    let mut static_analyzer = StaticAnalyzerState::default();
    let errors = static_analyzer.check_program(&ast);
    if !errors.is_empty() {
        log_errors(errors, file_content);
        return;
    }

    let mut execution_state = ExecutionState::default();
    match run_program(&mut execution_state, &ast) {
        Ok(result) => {
            println!("{}", result);
        }
        Err(e) => {
            println!("{} {:?}", "EXECUTION ERROR:".red().bold(), e);
        }
    };
}
