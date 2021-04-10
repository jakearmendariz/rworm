use pest::iterators::{Pairs, Pair};
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use crate::ast::{*};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct WormParser;

#[derive(Debug)]
pub enum ParseError {
    EndOfInput,
    FormatError
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

fn remove_whitespace(s: &str) -> String {
    s.chars().filter(|c| !c.is_whitespace()).collect()
}

pub fn parse_ast(pair: Pair<Rule>) -> Result<AstNode, ParseError>{
    match pair.as_rule() {
        Rule::assignment => {
            let mut inner_rules = pair.into_inner();
            let first_pos = inner_rules.next().unwrap();
            let (var_type, var_name) = match first_pos.as_rule() {
                Rule::var_type => {
                    let vartype = match first_pos.into_inner().next().unwrap().as_rule() {
                        Rule::vint => VarType::Int,
                        Rule::vfloat => VarType::Float,
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
            let expression = parse_into_expr(inner_rules.next().unwrap().into_inner());
            Ok(AstNode::Assignment(var_type, remove_whitespace(var_name), expression))
        },
        Rule::print => {
            let var_name = pair.into_inner().next().unwrap().as_str();
            Ok(AstNode::Print(var_name.to_string()))
        },
        Rule::ifstm => {
            let mut inner_rules = pair.into_inner();
            let mut bool_exp = inner_rules.next().unwrap().into_inner();
            let exp_left = parse_into_expr(bool_exp.next().unwrap().into_inner());
            let bool_op = match bool_exp.next().unwrap().as_rule() {
                Rule::eq => BoolOp::Eq,
                Rule::neq => BoolOp::Neq,
                Rule::geq => BoolOp::Geq,
                Rule::leq => BoolOp::Leq,
                rule => {
                    println!("{:?}", rule);
                    unreachable!();
                }
            };
            let exp_right = parse_into_expr(bool_exp.next().unwrap().into_inner());
            let mut stms = std::vec::Vec::new();
            let body = inner_rules.next().unwrap().into_inner();

            for stm in body {
                let ast = match parse_ast(stm) {
                    Ok(ast) => ast,
                    Err(e) => return Err(e),
                };
                stms.push(Box::new(ast));
            }
            Ok(AstNode::If(BoolExp(exp_left, bool_op, exp_right), stms))
        }
        Rule::EOI => return Err(ParseError::EndOfInput),
        _ => {
            println!("{:?} rule is unreachable while parsing", pair.as_rule());
            unreachable!();
        }
    }
}