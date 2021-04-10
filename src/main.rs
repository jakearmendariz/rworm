extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;
use pest::Parser;
#[allow(unused_imports)]
use pest::iterators::Pair;
use pest::prec_climber::PrecClimber;
use pest::prec_climber::Operator;
use pest::prec_climber::Assoc;
#[allow(unused_imports)]
use pest::iterators::Pairs;
mod ast;
use crate::ast::{*};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct WormParser;

lazy_static::lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right)
        ])
    };
}


fn parse_into_expr(expression: Pairs<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::num => Expr::ExpVal(Value::Number(pair.as_str().parse::<f64>().unwrap())),
            Rule::var_name => Expr::ExpVal(Value::Variable(pair.as_str().to_string())),
            Rule::expr => parse_into_expr(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr | 
        match op.as_rule() {
            Rule::add      => Expr::ExpOp(Box::new(lhs), OpType::Add, Box::new(rhs)),
            Rule::subtract => Expr::ExpOp(Box::new(lhs), OpType::Sub, Box::new(rhs)),
            Rule::multiply => Expr::ExpOp(Box::new(lhs), OpType::Mult, Box::new(rhs)),
            Rule::divide   => Expr::ExpOp(Box::new(lhs), OpType::Div, Box::new(rhs)),
            Rule::power    => Expr::ExpOp(Box::new(lhs), OpType::Pow, Box::new(rhs)),
            _ => unreachable!(),
        },
    )
}


fn evaluate(exp:Expr) -> f64 {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Value::Variable(_) => 0.0,
                Value::Number(number) => number
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            match op {
                OpType::Add => evaluate(*lhs) + evaluate(*rhs),
                OpType::Sub => evaluate(*lhs) - evaluate(*rhs),
                OpType::Mult => evaluate(*lhs) * evaluate(*rhs),
                OpType::Div => evaluate(*lhs) / evaluate(*rhs),
                OpType::Pow => evaluate(*lhs).powf(evaluate(*rhs)),
            }
        }
    }
}

fn parse_ast(pair: Pair<Rule>) -> Result<AstNode, ParseError>{
    match pair.as_rule() {
        Rule::assignment => {
            let mut inner_rules = pair.into_inner();
            let first_pos = inner_rules.next().unwrap();
            let (var_type, var_name) = match first_pos.as_rule() {
                Rule::var_type => {
                    let vartype = match first_pos.into_inner().next().unwrap().as_rule() {
                        Rule::vint => VarType::Int,
                        Rule::vstring => VarType::String,
                        _ => return Err(ParseError::FormatError)
                    };
                    (Some(vartype), inner_rules.next().unwrap().as_str())
                },
                Rule::var_name => {
                    (None, first_pos.as_str())
                }
                _ => return Err(ParseError::FormatError)
            };
            // println!("variable_name: {:?}\n",variable_name);
            let expression = parse_into_expr(inner_rules.next().unwrap().into_inner());
            // println!("expression: {:?}\n", expression);
            Ok(AstNode::Assignment(var_type, var_name.to_string(), expression))
        },
        Rule::EOI => return Err(ParseError::EndOfInput),
        _ => unreachable!(),
    }
    
}

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
