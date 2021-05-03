use pest::iterators::{Pairs, Pair};
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use crate::ast::{*};
use std::vec::Vec;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct WormParser;

#[derive(Debug, Clone)]
pub enum ParseError {
    EndOfInput,
    FormatError,
    NoReturnType,
    UnencampslatedStatement,
    GeneralParseError
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
            Rule::float => Expr::ExpVal(Object::Constant(Constant::Float(pair.as_str().parse::<f64>().unwrap()))),
            Rule::int => Expr::ExpVal(Object::Constant(Constant::Int(pair.as_str().parse::<i32>().unwrap()))),
            Rule::var_name => Expr::ExpVal(Object::Variable(pair.as_str().to_string())),
            Rule::func_call => {
                let mut inner = pair.into_inner();
                let func_name = inner.next().unwrap().as_str().to_string();
                let list = inner.next().unwrap().into_inner();
                let mut params = Vec::new();
                for item in list {
                    params.push(parse_into_expr(item.into_inner()));
                }
                Expr::ExpVal(Object::FuncCall(FuncCall {name:func_name, params:params}))
            },
            Rule::string => Expr::ExpVal(Object::Constant(Constant::String(pair.as_str().to_string()))),
            Rule::expr => parse_into_expr(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr | {
            // match lhs {
            //     Object::Constant(Constant::String)(_) => return Expr::ExpErr("lhs is string, cannot eval operation\n".to_string()),
            //     _ => (),
            // };
            // match rhs {
            //     Object::Constant(Constant::String)(_) => return Expr::ExpErr("rhs is string, cannot eval operation\n".to_string()),
            //     _ => (),
            // };
            match op.as_rule() {
                Rule::add      => Expr::ExpOp(Box::new(lhs), OpType::Add, Box::new(rhs)),
                Rule::subtract => Expr::ExpOp(Box::new(lhs), OpType::Sub, Box::new(rhs)),
                Rule::multiply => Expr::ExpOp(Box::new(lhs), OpType::Mult, Box::new(rhs)),
                Rule::divide   => Expr::ExpOp(Box::new(lhs), OpType::Div, Box::new(rhs)),
                Rule::power    => Expr::ExpOp(Box::new(lhs), OpType::Pow, Box::new(rhs)),
                _ => unreachable!(),
            }
        },
    )
}

fn parse_parameters(params_rules:Pairs<Rule>) -> Result<Vec<(VarType, String)>, ParseError> {
    let mut params:Vec<(VarType, String)> = Vec::new();
    for param in params_rules {
        //each param is in form { var_type var_name }
        let mut pair = param.into_inner();

        let vt = match pair.next() {
            Some(vt) => vt.into_inner().next().unwrap(),
            None => break,
        };
        let var_type = match vt.as_rule() {
            Rule::vint => VarType::Int,
            Rule::vfloat => VarType::Float,
            Rule::vstring => VarType::String,
            _ => return {
                println!("could not read parameters from fn on {}", vt.as_str());
                Err(ParseError::FormatError)
            }
        };
        let var_name = pair.next().unwrap().as_str();
        params.push((var_type, var_name.to_string()))
    }
    Ok(params)

}

fn parse_return_stm(return_rule:Pair<Rule>)  -> Result<VarType, ParseError>{
    Ok(match return_rule.as_rule() {
        Rule::vint => VarType::Int,
        Rule::vfloat => VarType::Float,
        Rule::vstring => VarType::String,
        _ => return {
            println!("parse_function(): could not format fn return type on: {}", return_rule.as_str());
            Err(ParseError::FormatError)
        }
    })
}

pub fn parse_function(pair:Pair<Rule>, state:&mut State) -> Result<(), ParseError> {
    match pair.as_rule(){
        Rule::func_def => (),
        _ => {
            println!("parse_function() on not a function: {}", pair.as_str());
            return Err(ParseError::UnencampslatedStatement)
        },
    }
    let mut inner_rules = pair.into_inner();
    let fn_name = inner_rules.next().unwrap().as_str().to_string();

    let next_rule = inner_rules.next().unwrap();
    let (params, return_type) = match next_rule.as_rule() {
        Rule::params => {
            (
                parse_parameters(next_rule.into_inner())?,
                parse_return_stm(inner_rules.next().unwrap().into_inner().next().unwrap())?
            )
        },
        Rule::var_type => {
            (
                Vec::new(),
                parse_return_stm(next_rule.into_inner().next().unwrap())?
            )
        },
        _ => return Err(ParseError::NoReturnType)
    };
    
    let mut stms = std::vec::Vec::new();
    for stm in inner_rules { // two in forloop, one in while loop
        let ast = parse_ast(stm, state)?;
        println!("{:?}", ast);
        stms.push(Box::new(ast));
    }
    let function = Function {name:fn_name.clone(), return_type:return_type, params:params, statements:stms};
    state.func_map.insert(fn_name, function);
    Ok(())
}

/* parses ast into nodes, only handles one clause at a time. */
pub fn parse_ast(pair: Pair<Rule>, state:&mut State) -> Result<AstNode, ParseError> {
    let rule = pair.as_rule();
    let statement = pair.as_str();
    // matches the rule depending on the type of statments
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
                        _ => return {
                            println!("error parsing var type on {}\n", statement);
                            Err(ParseError::FormatError)
                        }
                    };
                    (Some(vartype), inner_rules.next().unwrap().as_str())
                },
                Rule::var_name => {
                    (None, first_pos.as_str())
                }
                _ => return {
                    println!("error parsing statement {}\n", statement);
                    Err(ParseError::FormatError)
                }
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
            let mut stms = Vec::new();

            for stm in inner_rules { // two in forloop, one in while loop
                let ast = match parse_ast(stm, state) {
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
        Rule::func_def => {
            let mut inner_rules = pair.into_inner();
            let fn_name = inner_rules.next().unwrap().as_str().to_string();

            let up_params = inner_rules.next().unwrap().into_inner();
            let mut params:Vec<(VarType, String)> = Vec::new();
            for param in up_params {
                //each param is in form { var_type var_name }
                let mut pair = param.into_inner();
                let vt = pair.next().unwrap().into_inner().next().unwrap();
                let var_type = match vt.as_rule() {
                    Rule::vint => VarType::Int,
                    Rule::vfloat => VarType::Float,
                    Rule::vstring => VarType::String,
                    _ => return {
                        println!("could not read parameters from fn on {}", statement);
                        Err(ParseError::FormatError)
                    }
                };
                let var_name = pair.next().unwrap().as_str();
                params.push((var_type, var_name.to_string()))
            }
            let fn_return = match inner_rules.next().unwrap().into_inner().next().unwrap().as_rule() {
                Rule::vint => VarType::Int,
                Rule::vfloat => VarType::Float,
                Rule::vstring => VarType::String,
                _ => return {
                    println!("could not format fn return type on: {}", statement);
                    Err(ParseError::FormatError)
                }
            };
            let mut stms = std::vec::Vec::new();
            for stm in inner_rules { // two in forloop, one in while loop
                let ast = match parse_ast(stm, state) {
                    Ok(ast) => ast,
                    Err(e) => return Err(e),
                };
                stms.push(Box::new(ast));
            }
            let function = Function {name:fn_name.clone(), return_type:fn_return, params:params, statements:stms};
            state.func_map.insert(fn_name, function.clone());
            Ok(AstNode::FuncDef(function))
        },
        Rule::return_stm => {
            let return_expr = pair.into_inner().next().unwrap();
            Ok(AstNode::ReturnStm(parse_into_expr(return_expr.into_inner())))
        }
        Rule::parse_error => return Err(ParseError::GeneralParseError),
        Rule::EOI => return Err(ParseError::EndOfInput),
        _ => {
            println!("{:?} rule is unreachable while parsing", pair.as_rule());
            unreachable!();
        }
    }
}

