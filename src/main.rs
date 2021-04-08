extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;
use pest::Parser;
use pest::iterators::Pair;
use pest::prec_climber::PrecClimber;
use pest::prec_climber::Operator;
use pest::prec_climber::Assoc;
use pest::iterators::Pairs;

#[derive(Debug)]
enum Expr {
    ExpInt(f64),
    ExpOp(Box<Expr>, OpType, Box<Expr>)
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

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct ExprParser;

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
            Rule::num => Expr::ExpInt(pair.as_str().parse::<f64>().unwrap()),
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
        Expr::ExpInt(num) => num,
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
    let pairs = ExprParser::parse(Rule::calculation,&expression[..expression.len()-1]).unwrap_or_else(|e| panic!("{}", e));
    // let result:Expr = parse_into_expr(pairs);
    // println!("result:{}", evaluate(result));
    // Because ident_list is silent, the iterator will contain idents
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());
        
        // let infix = |lhs: i32, op: Pair<Rule>, rhs: i32| {
        //     match op.as_rule() {
        //         Rule::add => lhs + rhs,
        //         Rule::mult => lhs * rhs,
        //         _ => unreachable!()
        //     }
        // };
        // println!("infix:{:?}", infix);


        // A pair can be converted to an iterator of the tokens which make it up:
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::num => println!("Letter:  {}", inner_pair.as_str()),
                Rule::var_type => println!("var:  {}", inner_pair.as_str()),
                Rule::assignment => println!("assignment:  {}", inner_pair.as_str()),
                _ => println!("unknown:{}", inner_pair.as_str())
            };
        }
    }
}
