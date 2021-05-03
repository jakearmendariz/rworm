extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod ast;
mod parser;
mod evaluate;
use crate::ast::{AstNode, State, Function, Constant};
use crate::parser::{*};
use crate::evaluate::{*};
use pest::Parser;
use std::collections::{HashMap};
use std::vec::Vec;
use std::string::String;

#[derive(Debug, Clone)]
enum WormError{ParseError, ExecutionError}

// builds default for state
fn build_default_state() -> State {
    let var_map:HashMap<String, Constant> = HashMap::new();
    let func_map:HashMap<String, Function> = HashMap::new();
    State{var_map:var_map, func_map:func_map}
}


fn main() {
    let expression;
    expression = std::fs::read_to_string("worm/easy.c").expect("cannot read file"); //from file
    let pairs = WormParser::parse(Rule::program,&expression).unwrap_or_else(|e| panic!("{}", e));
    let mut state = build_default_state();
    for pair in pairs {
        match pair.as_rule() {
            Rule::EOI => continue,
            _ => { match parse_function(pair, &mut state) {
                    Ok(stm) => (),
                    Err(e) => {
                        println!("{:?}", e);
                    }
                }
            }
        }
        
    }  
    let main_function = match state.func_map.get("main") {
        Some(func) => func,
        None => {
            println!("Error parsing main function");
            return;
        },
    };
    let result = match eval_func(main_function.clone(), &mut state) {
        Ok(res) => Ok(res),
        Err(e) => Err(e)
    };
    println!("result: {:?}", result);
}
