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
mod ordering;
use crate::state::State;
use crate::evaluate::*;
use crate::parser::*;
use crate::static_analysis::*;
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
        stack_lvl: 0,
    }
}

/*
* Main function for worm interpretter
*/
fn main() {
    let first_arg = std::env::args().nth(1).expect("expected a filename");
    let second_arg;
    if first_arg == "-c" {
        // compile only
        second_arg = std::env::args().nth(2).expect("expected a filename");
        let expression =
            std::fs::read_to_string(&format!("{}", second_arg)[..]).expect("cannot read file"); //from file
        let pairs =
            WormParser::parse(Rule::program, &expression).unwrap_or_else(|e| panic!("{}", e));
        let mut state = build_default_state();
        // parses the program into an AST, saves the functions AST in the state to be called upon later
        match parse_program(pairs, &mut state) {
            Ok(()) => (),
            Err(e) => {
                println!("{} {:?}", "PARSE ERROR:".yellow().bold(), e);
                return;
            }
        }

        match check_program(&mut state) {
            Ok(()) => (),
            Err(e) => {
                println!("{} {}", "STATIC ERROR:".red().bold(), e);
                return;
            }
        }

        let encoded: Vec<u8> = bincode::serialize(&state).unwrap();
        let filename = &format!("{}.o", second_arg)[..];
        let mut file = File::create(filename).unwrap();
        match file.write(&encoded) {
            Ok(_) => (),
            Err(_) => println!("{} could not write to file", "FILE ERROR:".red().bold()),
        }
        println!("{}", "Compile Successfully".green());
        //write to file
    } else if first_arg == "-e" {
        // execute only
        second_arg = std::env::args().nth(2).expect("expected a filename");
        let contents =
            std::fs::read_to_string(second_arg).expect("Something went wrong reading the file");
        let mut decoded: State = bincode::deserialize(&contents.as_bytes()).unwrap();
        match run_program(&mut decoded) {
            Ok(result) => {
                println!("{}: {}", "Execution Result".green(), result);
            }
            Err(e) => {
                println!("{} {:?}", "EXECUTION ERROR:".red().bold(), e);
            }
        };
    } else {
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

        match check_program(&mut state) {
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
}
