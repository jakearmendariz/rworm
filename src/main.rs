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
use crate::ast::{State};
use crate::parser::*;
use pest::Parser;
use std::collections::HashMap;
use crate::evaluate::*;
use crate::static_analysis::check_program;
use log::{info, trace, error};
use std::fs::File;
use std::io::prelude::*;

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
    let first_arg = std::env::args().nth(1).expect("expected a filename");
    let second_arg;
    if first_arg == "-c" {
        // compile only
        second_arg = std::env::args().nth(2).expect("expected a filename");
        let expression = std::fs::read_to_string(&format!("{}", second_arg)[..]).expect("cannot read file"); //from file
        let pairs = WormParser::parse(Rule::program, &expression).unwrap_or_else(|e| panic!("{}", e));
        let mut state = build_default_state();
        // parses the program into an AST, saves the functions AST in the state to be called upon later
        match parse_program(pairs, &mut state) {
            Ok(()) => (),
            Err(e) => {
                trace!("{:?}", e);
                return;
            }
        }

        match check_program(&mut state) {
            Ok(()) => (),
            Err(e) => {
                error!("{}", e);
                return;
            }
        }

        let encoded: Vec<u8> = bincode::serialize(&state).unwrap();
        let filename = &format!("{}.o", second_arg)[..];
        let mut file = File::create(filename).unwrap();
        match file.write(&encoded) {
            Ok(_) => (),
            Err(_) => error!("error writing to file")
        }
        info!("compiled");
        //write to file

    } else if first_arg == "-e" {
        // execute only
        second_arg = std::env::args().nth(2).expect("expected a filename");
        let contents = std::fs::read_to_string(second_arg)
            .expect("Something went wrong reading the file");
        let mut decoded:State = bincode::deserialize(&contents.as_bytes()).unwrap();
        let result = match run_program(&mut decoded) {
            Ok(res) => Ok(res),
            Err(e) => Err(e),
        };
        println!("exec result: {:?}", result);

    } else {
        // compile and execute
        let filename = &format!("{}", first_arg)[..];
        let expression = std::fs::read_to_string(filename).expect("cannot read file"); //from file
        let pairs = WormParser::parse(Rule::program, &expression).unwrap_or_else(|e| panic!("{}", e));
        let mut state = build_default_state();
        // parses the program into an AST, saves the functions AST in the state to be called upon later
        match parse_program(pairs, &mut state) {
            Ok(()) => (),
            Err(e) => {
                trace!("parse error {:?}", e);
                return;
            }
        }

        match check_program(&mut state) {
            Ok(()) => (),
            Err(e) => {
                error!("{}", e);
                return;
            }
        }

        match run_program(&mut state) {
            Ok(res) => info!("{}", res),
            Err(e) => error!("{:?}", e),
        };
    }
}
