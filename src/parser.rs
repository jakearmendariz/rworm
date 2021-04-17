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

/*
precendence climber, helping with the ordering rules
*/
lazy_static::lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;
        PrecClimber::new(vec![
            Operator::new(or, Left) | Operator::new(and, Left), 
            Operator::new(eq, Left) | Operator::new(gt, Left) | Operator::new(lt, Left) 
                | Operator::new(leq, Left) | Operator::new(neq, Left) | Operator::new(leq, Left) | Operator::new(geq, Left),            
            Operator::new(not, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right)
        ])
    };
}

/* parse a boolean expression, for a == b, return (a, ==, b) */
fn parse_bool_exp(bool_exp:&mut Pairs<Rule>) -> BoolExp {
    BoolExp(
    parse_into_expr(bool_exp.next().unwrap().into_inner()),
    match bool_exp.next().unwrap().as_rule() {
        Rule::eq => BoolOp::Eq,
        Rule::neq => BoolOp::Neq,
        Rule::geq => BoolOp::Geq,
        Rule::leq => BoolOp::Leq,
        Rule::lt => BoolOp::Lt,
        Rule::gt => BoolOp::Gt,
        rule => {
            println!("{:?}", rule);
            unreachable!();
        }
    },
    parse_into_expr(bool_exp.next().unwrap().into_inner())
    )
}

/* builds a bool abstract syntax tree */
fn parse_bool_ast(conditional:&mut Pairs<Rule>) -> BoolAst {
    PREC_CLIMBER.climb(
        conditional,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::tru => BoolAst::Const(true),
            Rule::fal => BoolAst::Const(false),
            Rule::boolnot => BoolAst::Not(Box::new(parse_bool_ast(&mut pair.into_inner()))),
            Rule::boolterm => parse_bool_ast(&mut pair.into_inner()),
            Rule::boolexp => BoolAst::Exp(parse_bool_exp(&mut pair.into_inner())),
            Rule::boolexpr => parse_bool_ast(&mut pair.into_inner()),
            _ => {
                println!("{:?} rule is unreachable while parsing", pair.as_rule());
                unreachable!();
            }
        },
        |lhs: BoolAst, op: Pair<Rule>, rhs: BoolAst | 
        match op.as_rule() {
            Rule::and => BoolAst::And(Box::new(lhs), Box::new(rhs)),
            Rule::or => BoolAst::Or(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        },
    )
}

/* parses pairs of rules from peg parser into a expression */
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

/* parses ast into nodes, only handles one clause at a time. */
pub fn parse_ast(pair: Pair<Rule>) -> Result<AstNode, ParseError>{
    let rule = pair.as_rule();
    match rule {
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
            Ok(AstNode::Assignment(var_type, var_name.to_string(), expression))
        },
        Rule::print => {
            let var_name = pair.into_inner().next().unwrap().as_str();
            Ok(AstNode::Print(var_name.to_string()))
        },
        Rule::ifstm | Rule::whilestm => {
            let mut inner_rules = pair.into_inner();
            let mut bool_exp = inner_rules.next().unwrap().into_inner();
            let bool_ast = parse_bool_ast(&mut bool_exp);
            let mut stms = std::vec::Vec::new();

            for stm in inner_rules { // two in forloop, one in while loop
                let ast = match parse_ast(stm) {
                    Ok(ast) => ast,
                    Err(e) => return Err(e),
                };
                stms.push(Box::new(ast));
            }

            match rule {
                Rule::ifstm => Ok(AstNode::If(bool_ast, stms)),
                Rule::whilestm => Ok(AstNode::While(bool_ast, stms)),
                _ => unreachable!()
            }
        },
        Rule::skip => return Ok(AstNode::Skip()),
        Rule::builtin => {
            let builtin = pair.into_inner().next().unwrap();
            match builtin.as_rule() {
                Rule::del => Ok(AstNode::BuiltIn(BuiltIn::Delete(builtin.into_inner().next().unwrap().as_str().to_string()))),
                Rule::sum => Ok(AstNode::BuiltIn(BuiltIn::Sum())),
                _ => unreachable!()
            }
        },
        Rule::parse_error => return Err(ParseError::FormatError),
        Rule::EOI => return Err(ParseError::EndOfInput),
        _ => {
            println!("{:?} rule is unreachable while parsing", pair.as_rule());
            unreachable!();
        }
    }
}

