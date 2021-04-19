extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod ast;
mod parser;
mod evaluate;
use crate::ast::AstNode;
use crate::parser::{*};
use crate::evaluate::{*};
use pest::Parser;
use std::collections::HashMap;
use std::string::String;

fn main() {
    let expression;
    expression = std::fs::read_to_string("worm/easy.worm").expect("cannot read file"); //from file
    let pairs = WormParser::parse(Rule::program,&expression).unwrap_or_else(|e| panic!("{}", e));
    // let mut map:HashMap<String, Variable> = HashMap::new();
    let mut state = State::default();
    // let mut func_map:HashMap<String, AstNode> = HashMap::new();
    for pair in pairs {
        let ast = parse_ast(pair);
        println!("ast: {:?}", ast);
        match ast {
            Ok(stm) => {
                let result = execute_ast(stm, &mut state);
                match result {
                    Ok(_) => (),
                    Err(e) => println!("error:{:?}", e)
                }
            },
            Err(_) => ()
        }
    }   
}
