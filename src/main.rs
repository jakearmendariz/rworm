extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod ast;
mod parser;
mod evaluate;
use crate::ast::{*};
use crate::parser::{*};
use crate::evaluate::{*};
use pest::Parser;

fn main() {
    let mut expression = String::new();
    std::io::stdin().read_line(&mut expression).unwrap();
    println!("inputted:{}", expression);
    let pairs = WormParser::parse(Rule::program,&expression[..expression.len()-1]).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
        let ast = parse_ast(pair);
        println!("ast: {:?}", ast);
    }   
}
