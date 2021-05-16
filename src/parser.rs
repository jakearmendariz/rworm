use crate::ast::*;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::vec::Vec;
use log::{warn};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct WormParser;

#[derive(Debug, Clone)]
pub enum ParseError {
    EndOfInput,
    FormatError(String),
    NoReturnType,
    UnencampslatedStatement,
    GeneralParseError(String),
}

/*
* parse the program
* starting from the top
* currently only only parsing functions, no global variables or code outside functions
*/
pub fn parse_program(pairs: Pairs<Rule>, state: &mut State) -> Result<(), ParseError> {
    for pair in pairs {
        match pair.as_rule() {
            Rule::EOI => continue,
            Rule::import_stm => { 
                parse_ast(pair, state)?; 
                continue
            },
            _ => parse_function(pair, state)?,
        }
    }
    Ok(())
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
fn parse_bool_exp(bool_exp: &mut Pairs<Rule>) -> BoolExp {
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
                warn!("{:?}", rule);
                unreachable!();
            }
        },
        parse_into_expr(bool_exp.next().unwrap().into_inner()),
    )
}

/* builds a bool abstract syntax tree */
fn parse_bool_ast(conditional: &mut Pairs<Rule>) -> BoolAst {
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
                warn!("{:?} rule is unreachable while parsing", pair.as_rule());
                unreachable!();
            }
        },
        |lhs: BoolAst, op: Pair<Rule>, rhs: BoolAst| match op.as_rule() {
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
            Rule::float => Expr::ExpVal(Object::Constant(Constant::Float(
                pair.as_str().parse::<f64>().unwrap(),
            ))),
            Rule::int => Expr::ExpVal(Object::Constant(Constant::Int(
                pair.as_str().parse::<i32>().unwrap(),
            ))),
            Rule::char => Expr::ExpVal(Object::Constant(Constant::Char(
                {
                    let character = pair.into_inner().next().unwrap().as_str();
                    println!("character : {}", character);
                    character.chars().next().unwrap()
                }
            ))),
            Rule::var_name => Expr::ExpVal(Object::Variable(pair.as_str().to_string())),
            Rule::func_call => {
                let mut inner = pair.into_inner();
                let func_name = inner.next().unwrap().as_str().to_string();
                let mut params = Vec::new();
                for item in inner.next().unwrap().into_inner() {
                    // warn!("item in funccall() : {:?}", item);
                    params.push(parse_into_expr(item.into_inner()));
                }
                Expr::ExpVal(Object::FuncCall(FuncCall {
                    name: func_name,
                    params: params,
                }))
            }
            Rule::string => Expr::ExpVal(Object::Constant(Constant::String(
                pair.into_inner().next().unwrap().as_str().to_string(),
            ))),
            Rule::expr => parse_into_expr(pair.into_inner()),
            Rule::array_index => {
                let mut arrary_index_rules = pair.into_inner();
                let array_name = arrary_index_rules.next().unwrap().as_str().to_string();
                let index = parse_into_expr(arrary_index_rules.next().unwrap().into_inner());
                Expr::ExpVal(Object::Constant(Constant::ArrayIndex(
                    array_name,
                    Box::new(index),
                )))
            }
            _ => unreachable!(),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| match op.as_rule() {
            Rule::add => Expr::ExpOp(Box::new(lhs), OpType::Add, Box::new(rhs)),
            Rule::subtract => Expr::ExpOp(Box::new(lhs), OpType::Sub, Box::new(rhs)),
            Rule::multiply => Expr::ExpOp(Box::new(lhs), OpType::Mult, Box::new(rhs)),
            Rule::divide => Expr::ExpOp(Box::new(lhs), OpType::Div, Box::new(rhs)),
            Rule::power => Expr::ExpOp(Box::new(lhs), OpType::Pow, Box::new(rhs)),
            _ => unreachable!(),
        },
    )
}


fn parse_parameters(params_rules: Pairs<Rule>) -> Result<Vec<(VarType, String)>, ParseError> {
    let mut params: Vec<(VarType, String)> = Vec::new();
    for param in params_rules {
        //each param is in form { var_type var_name }
        let mut pair = param.into_inner();

        let var_type = match pair.next() {
            Some(var_type) => {
                let first = var_type.into_inner().next().unwrap();
                match first.as_rule() {
                    Rule::vint => VarType::Int,
                    Rule::vfloat => VarType::Float,
                    Rule::vchar => VarType::Char,
                    Rule::vstring => VarType::String,
                    Rule::array_inst => match first.into_inner().next().unwrap().as_rule() {
                        Rule::vint => VarType::Int,
                        Rule::vfloat => VarType::Float,
                        Rule::vstring => VarType::String,
                        Rule::vchar => VarType::Char,
                        _ => {
                            return Err(ParseError::FormatError(format!(
                                "parse_parameters() array_inst"
                            )))
                        }
                    },
                    _ => {
                        return {
                            Err(ParseError::FormatError(format!(
                                "parse_parameters() parameters"
                            )))
                        }
                    }
                }
            }
            None => break,
        };
        let var_name = pair.next().unwrap().as_str();
        params.push((var_type, var_name.to_string()))
    }
    Ok(params)
}

fn parse_return_stm(return_rule: Pair<Rule>) -> Result<VarType, ParseError> {
    Ok(match return_rule.as_rule() {
        Rule::vint => VarType::Int,
        Rule::vfloat => VarType::Float,
        Rule::vstring => VarType::String,
        Rule::vchar => VarType::Char,
        Rule::array_inst => parse_return_stm(return_rule.into_inner().next().unwrap())?,
        _ => {
            return {
                Err(ParseError::FormatError(format!(
                    "parse_return_stm() error on {}",
                    return_rule.as_str()
                )))
            }
        }
    })
}

/*
* parse the function from the pair provided.
*/
pub fn parse_function(pair: Pair<Rule>, state: &mut State) -> Result<(), ParseError> {
    match pair.as_rule() {
        Rule::func_def => (),
        _ => {
            warn!("parse_function() on not a function: {}", pair.as_str());
            return Err(ParseError::UnencampslatedStatement);
        }
    }
    let mut inner_rules = pair.into_inner();
    let fn_name = inner_rules.next().unwrap().as_str().to_string();

    let next_rule = inner_rules.next().unwrap();
    let (params, return_type) = match next_rule.as_rule() {
        Rule::params => (
            parse_parameters(next_rule.into_inner())?,
            parse_return_stm(inner_rules.next().unwrap().into_inner().next().unwrap())?,
        ),
        Rule::var_type => (
            Vec::new(),
            parse_return_stm(next_rule.into_inner().next().unwrap())?,
        ),
        _ => return Err(ParseError::NoReturnType),
    };

    let mut stms = std::vec::Vec::new();
    for stm in inner_rules {
        // two in forloop, one in while loop
        let ast = parse_ast(stm, state)?;
        stms.push(Box::new(ast));
    }
    let function = Function {
        name: fn_name.clone(),
        return_type: return_type,
        params: params,
        statements: stms,
    };
    state.func_map.insert(fn_name, function);
    Ok(())
}

/*
* parses ast into nodes, only handles one clause at a time.
*/
pub fn parse_ast(pair: Pair<Rule>, state: &mut State) -> Result<AstNode, ParseError> {
    let rule = pair.as_rule();
    let statement = pair.as_str();
    // matches the rule depending on the type of statments
    match rule {
        Rule::assignment => {
            let mut inner_rules = pair.into_inner();
            let first_pos = inner_rules.next().unwrap();
            let (var_type, var_name) = match first_pos.as_rule() {
                Rule::var_type => {
                    let start_of_assign = first_pos.into_inner().next().unwrap();
                    let vartype = match start_of_assign.as_rule() {
                        Rule::vint => VarType::Int,
                        Rule::vfloat => VarType::Float,
                        Rule::vstring => VarType::String,
                        Rule::vchar => VarType::Char,
                        _ => {
                            return {
                                Err(ParseError::FormatError(format!(
                                    "error parsing var type on {}\n",
                                    statement
                                )))
                            }
                        }
                    };
                    (Some(vartype), inner_rules.next().unwrap().as_str())
                }
                Rule::var_name => (None, first_pos.as_str()),
                Rule::array_index => {
                    let mut array_index_rule = first_pos.into_inner();
                    let var_name = array_index_rule.next().unwrap().as_str().to_string();
                    let index_exp = parse_into_expr(array_index_rule.next().unwrap().into_inner());
                    let value_exp = parse_into_expr(inner_rules.next().unwrap().into_inner());
                    return Ok(AstNode::ArrayIndexAssignment(
                        var_name, index_exp, value_exp,
                    ));
                }
                _ => {
                    return {
                        Err(ParseError::FormatError(format!(
                            "error parsing statement {}\n",
                            statement
                        )))
                    }
                }
            };
            let expression = parse_into_expr(inner_rules.next().unwrap().into_inner());
            Ok(AstNode::Assignment(
                var_type,
                var_name.to_string(),
                expression,
            ))
        }
        Rule::array_definition => {
            // format of `int[] a = [expression; size];`
            let mut array_rules = pair.into_inner();
            // to get the type inside of array, we first have to pass the outer loop of array_inst
            let array_type = match array_rules
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .as_rule()
            {
                Rule::vint => VarType::Int,
                Rule::vfloat => VarType::Float,
                Rule::vstring => VarType::String,
                Rule::vchar => VarType::Char,
                _ => unreachable!(),
            };
            let array_name = array_rules.next().unwrap().as_str().to_string();

            let init_or_call = array_rules.next().unwrap();
            let mut array_def = match init_or_call.as_rule() {
                Rule::array_initial => init_or_call.into_inner(),
                Rule::func_call => {
                    let mut inner = init_or_call.into_inner();
                    let func_name = inner.next().unwrap().as_str().to_string();
                    let mut params = Vec::new();
                    for item in inner.next().unwrap().into_inner() {
                        params.push(parse_into_expr(item.into_inner()));
                    }
                    return Ok(AstNode::ArrayFromExp(
                        array_type,
                        array_name,
                        Expr::ExpVal(Object::FuncCall(FuncCall {
                            name: func_name,
                            params: params,
                        })),
                    ));
                }
                _ => {
                    return Err(ParseError::GeneralParseError(String::from(
                        "non array type in front of array dec",
                    )))
                }
            };
            // expression can be a constant or a value to be evaulated to, var i represents the index of an array
            let first = array_def.next().unwrap();
            // catch the piped variable, optional, but if there it's the index of the program
            let (piped, expression) = match first.as_rule() {
                Rule::piped => (
                    Some(first.into_inner().next().unwrap().as_str().to_string()),
                    parse_into_expr(array_def.next().unwrap().into_inner()),
                ),
                Rule::expr => (None, parse_into_expr(first.into_inner())),
                _ => {
                    return Err(ParseError::GeneralParseError(String::from(
                        "Unmatched rule in piped slot of array def",
                    )))
                }
            };
            // let expression = parse_into_expr(array_def.next().unwrap().into_inner());
            // size must be an uinteger, but parse into expression anyways in case a variable is passed in
            let size_expr = parse_into_expr(array_def.next().unwrap().into_inner());
            Ok(AstNode::ArrayDef(
                array_type, array_name, piped, expression, size_expr,
            ))
        }
        Rule::whilestm => {
            let mut inner_rules = pair.into_inner();
            let mut bool_exp = inner_rules.next().unwrap().into_inner();
            let bool_ast = parse_bool_ast(&mut bool_exp);
            let mut stms = Vec::new();

            for stm in inner_rules {
                let ast = parse_ast(stm, state)?;
                stms.push(Box::new(ast));
            }

            Ok(AstNode::While(bool_ast, stms))
        }
        Rule::ifstm => {
            let mut inner_rules = pair.into_inner();
            let mut if_set = Vec::new();
            loop {
                //this will break once all if else have been parsed
                let (ifstm, boolexp) = match inner_rules.next() {
                    Some(stm) => {
                        match stm.as_rule() {
                            Rule::else_stm => {
                                let inner = stm.into_inner();
                                (inner, BoolAst::Const(true))
                            }
                            _ => {
                                //only if or if else statements get to this point, they are handled the same
                                let mut inner = stm.into_inner();
                                let mut boolast = inner.next().unwrap().into_inner();
                                (inner, parse_bool_ast(&mut boolast))
                            }
                        }
                    }
                    None => break,
                };
                //get the body of statements
                let mut stms: Vec<Box<AstNode>> = Vec::new();
                for stm in ifstm {
                    stms.push(Box::new(parse_ast(stm, state)?));
                }
                if_set.push((boolexp, stms));
            }
            Ok(AstNode::If(if_set))
        }
        Rule::skip => return Ok(AstNode::Skip()),
        Rule::builtin => {
            let builtin = pair.into_inner().next().unwrap();
            match builtin.as_rule() {
                Rule::print => {
                    let expression = parse_into_expr(builtin.into_inner());
                    Ok(AstNode::BuiltIn(BuiltIn::Print(expression)))
                }
                Rule::assert => Ok(AstNode::BuiltIn(BuiltIn::Assert(parse_bool_ast(
                    &mut builtin.into_inner(),
                )))),
                _ => unreachable!(),
            }
        }
        Rule::return_stm => {
            let return_expr = pair.into_inner().next().unwrap();
            Ok(AstNode::ReturnStm(parse_into_expr(
                return_expr.into_inner(),
            )))
        }
        Rule::parse_error => {
            return Err(ParseError::GeneralParseError(format!(
                "unmatched rule while parsing ast {:?}",
                rule
            )))
        }
        Rule::import_stm => {
            use pest::Parser;
            let filename = pair.into_inner().next().unwrap().into_inner().next().unwrap().as_str();
            let expression = std::fs::read_to_string(filename).expect("cannot read file"); //from file
            let pairs = WormParser::parse(Rule::program, &expression).unwrap_or_else(|e| panic!("{}", e));
            // parses the program into an AST, saves the functions AST in the state to be called upon later
            parse_program(pairs, state)?;
            Ok(AstNode::Skip())
        }
        Rule::EOI => return Err(ParseError::EndOfInput),
        _ => {
            warn!("{:?} rule is unreachable while parsing", pair.as_rule());
            unreachable!();
        }
    }
}
