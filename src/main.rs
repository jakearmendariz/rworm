extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod ast;
mod parser;
mod evaluate;
use crate::parser::{*};
use crate::evaluate::{*};
use pest::Parser;
use std::collections::HashMap;
use std::string::String;

fn main() {
    let expression;// = String::new();
    expression = std::fs::read_to_string("worm/easy.worm").expect("cannot read file"); //from file
    // std::io::stdin().read_line(&mut expression).unwrap(); //from user sinput
    println!("inputted:{}", expression);
    let pairs = WormParser::parse(Rule::program,&expression[..expression.len()]).unwrap_or_else(|e| panic!("{}", e));
    let mut map:HashMap<String, f64> = HashMap::new();
    for pair in pairs {
        let ast = parse_ast(pair);
        println!("ast: {:?}", ast);
        match ast {
            Ok(stm) => {
                let result = execute_ast(stm, &mut map);
                match result {
                    Ok(_) => (),
                    Err(e) => println!("error:{:?}", e)
                }
            },
            Err(_) => ()
        }
    }   
}
