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

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct WormParser;


#[derive(Debug)]
enum AST {
    Print(String),
    Assign(String, Expr)
}

#[derive(Debug)]
enum Expr {
    ExpVal(Value),
    ExpOp(Box<Expr>, OpType, Box<Expr>)
}

#[derive(Debug)]
enum Value {
  Variable(String),
  Number(f64)  
}

#[derive(Debug)]
struct Operation {
    op:OpType,
    left:Box<Expr>,
    right:Box<Expr>
}

#[derive(Debug)]
enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow
}

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
        |lhs: Expr, op: Pair<Rule>, rhs: Expr | match op.as_rule() {
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


fn main() {
    let mut expression = String::new();
    std::io::stdin().read_line(&mut expression).unwrap();
    println!("inputted:{}", expression);
    let pairs = WormParser::parse(Rule::program,&expression[..expression.len()-1]).unwrap_or_else(|e| panic!("{}", e));
    use std::rc::Rc;
    use std::cell::{RefCell, RefMut};
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("\nPair");
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());
        let node = match pair.as_rule() {
            Rule::assignment => {
                let mut variable_name;//:Rc<RefCell<String>> = Rc::new(RefCell::new(String::new()));
                let mut expression;// Rc::new(RefCell::new(Expr::new()));
                for token in pair.into_inner() {
                    match token.as_rule() {
                        Rule::var_name => {
                            variable_name = token.as_str();
                        },
                        Rule::expr => {
                            expression = parse_into_expr(token.into_inner());
                        },
                        Rule::var_type => {
                            println!("var type:{}", token.as_str());
                        },
                        _ => unreachable!(),
                    };
                }
                AST::Assign(variable_name.to_string(), expression)
                // let statement:Vec<Pair<'_,Rule>> = pair.into_inner().filter(x| x=x).collect();
                // let variable_name = match statement[1] {
                //     Rule::var_name => statement[1].as_str().to_string(),
                //     _ => unreachable!(),
                // };
                // let expression = match statement[2] {
                //     Rule::expr => parse_into_expr(statement[2].into_inner()),
                //     _ => unreachable!(),
                // };
                // AST::Assign(variable_name, expression)
            },
            _ => unreachable!(),
        };
        println!("node:{:?}", node);
        // let infix = |lhs: i32, op: Pair<Rule>, rhs: i32| {
        //     match op.as_rule() {
        //         Rule::add => lhs + rhs,
        //         Rule::mult => lhs * rhs,
        //         _ => unreachable!()
        //     }
        // };
        // println!("infix:{:?}", infix);


        // A pair can be converted to an iterator of the tokens which make it up:
        // for inner_pair in pair.into_inner() {
        //     println!("{:?}:{}", inner_pair.as_rule(), inner_pair.as_str());
            // for in_inner_pair in inner_pair.into_inner() {
            //     println!("in_inner {:?}:{}", in_inner_pair.as_rule(), in_inner_pair.as_str());
            //     // match in_inner_pair.as_rule() {
            //     //     Rule::expr => {
            //     //         let result = parse_into_expr(in_inner_pair.into_inner());
            //     //         println!("expression:{:?}", result);
            //     //         println!("result:{}", evaluate(result));
            //     //     },
            //     //     _ => println!()
            //     // };
            // }
            // match inner_pair.as_rule() {
            //     Rule::num => println!("Letter:  {}", inner_pair.as_str()),
            //     Rule::var_type => println!("var:  {}", inner_pair.as_str()),
            //     Rule::assignment => println!("assignment:  {}", inner_pair.as_str()),
            //     _ => 
            // };
        // }
    }
}
