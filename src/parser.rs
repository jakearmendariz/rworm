/*
* parser.rs
* parses the program into an ast
*/
use crate::ast::*;
use crate::state::AstMap;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::collections::BTreeMap;
use std::vec::Vec;

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
pub fn parse_program(pairs: Pairs<Rule>, ast: &mut AstMap) -> Result<(), ParseError> {
    for pair in pairs {
        match pair.as_rule() {
            Rule::EOI => continue,
            Rule::import_stm => {
                parse_ast_node(pair, ast)?;
                continue;
            }
            _ => match parse_ast_node(pair, ast)? {
                AstNode::Skip() => (),
                _ => return Err(ParseError::UnencampslatedStatement),
            },
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

/* parses pairs of rules from peg parser into a expression */
fn parse_expr(expression: Pairs<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| {
            let position = pair.as_span().start();
            match pair.as_rule() {
                Rule::int => {
                    // parse int from pair, remove whitespace first
                    let int_lit = pair.as_str().trim().parse::<i32>().unwrap();
                    Expr::Literal(Literal::Int(int_lit), position)
                }
                Rule::char => {
                    let character_lit = pair
                        .into_inner()
                        .next()
                        .unwrap()
                        .as_str()
                        .chars()
                        .next()
                        .unwrap();
                    Expr::Literal(Literal::Char(character_lit), position)
                }
                Rule::tru => Expr::Literal(Literal::Bool(true), position),
                Rule::fal => Expr::Literal(Literal::Bool(false), position),
                // Rule::var_name => Expr::ExpVal(Object::Variable(pair.as_str().to_string())),
                Rule::identifier => Expr::Identifier(parse_identifier(pair)),
                Rule::func_call => {
                    let mut func_call_pairs = pair.into_inner();
                    let func_name = func_call_pairs.next().unwrap().as_str().to_string();
                    let params = match func_call_pairs.next() {
                        Some(parameters_pair) => parameters_pair
                            .into_inner()
                            .map(|item| parse_expr(item.into_inner()))
                            .collect(),
                        None => Vec::with_capacity(0),
                    };
                    Expr::FnCall {
                        name: func_name,
                        params,
                        position,
                    }
                }
                Rule::array_empty => Expr::Literal(
                    Literal::Array(
                        parse_type_from_rule(pair.into_inner().next().unwrap()).unwrap(),
                        Vec::new(),
                    ),
                    position,
                ),
                Rule::string => Expr::Literal(
                    Literal::String(pair.into_inner().next().unwrap().as_str().to_string()),
                    position,
                ),
                Rule::array_value => {
                    // format of `int[] a = [expression; size];`
                    let init_or_call = pair.into_inner().next().unwrap();
                    let mut array_def = match init_or_call.as_rule() {
                        Rule::array_initial => init_or_call.into_inner(),
                        Rule::array_empty => {
                            return Expr::Literal(
                                Literal::Array(
                                    parse_type_from_rule(init_or_call.into_inner().next().unwrap())
                                        .unwrap(),
                                    Vec::new(),
                                ),
                                position,
                            );
                        }
                        _ => {
                            unreachable!("")
                        }
                    };
                    // expression can be a Literal or a value to be evaulated to, var i represents the index of an array
                    let first = array_def.next().unwrap();
                    // catch the piped variable, optional, but if there it's the index of the program
                    let (piped, expression) = match first.as_rule() {
                        Rule::piped => (
                            Some(first.into_inner().next().unwrap().as_str().to_string()),
                            parse_expr(array_def.next().unwrap().into_inner()),
                        ),
                        Rule::expr => (None, parse_expr(first.into_inner())),
                        _ => {
                            panic!("")
                        }
                    };
                    // let expression = parse_expr(array_def.next().unwrap().into_inner());
                    // size must be an uinteger, but parse into expression anyways in case a variable is passed in
                    let size_expr = parse_expr(array_def.next().unwrap().into_inner());
                    Expr::ListComprehension {
                        piped_var: piped,
                        value_expr: Box::new(expression),
                        in_expr: Box::new(size_expr),
                    }
                }
                Rule::expr => parse_expr(pair.into_inner()),
                Rule::unary_expr => {
                    let mut unary_expr_pairs = pair.into_inner();
                    match unary_expr_pairs.next().unwrap().as_rule() {
                        Rule::not => Expr::UnaryExpr(
                            UnaryOp::Not,
                            Box::new(parse_expr(unary_expr_pairs.next().unwrap().into_inner())),
                        ),
                        _ => unreachable!("Unexpected unary rules"),
                    }
                }
                Rule::hash_obj => {
                    let mut inner_types = pair.into_inner();
                    let key_type = parse_type_from_rule(match inner_types.next() {
                        Some(a_rule) => match a_rule.as_rule() {
                            Rule::var_type => a_rule.into_inner().next().unwrap(),
                            _ => a_rule,
                        },
                        None => panic!("missing key type from Map"),
                    })
                    .unwrap();
                    let value_type = parse_type_from_rule(match inner_types.next() {
                        Some(a_rule) => match a_rule.as_rule() {
                            Rule::var_type => a_rule.into_inner().next().unwrap(),
                            _ => a_rule,
                        },
                        None => panic!("missing key type from Map"),
                    })
                    .unwrap();
                    Expr::Literal(
                        Literal::Map(key_type, value_type, BTreeMap::default()),
                        position,
                    )
                }
                _ => unreachable!(),
            }
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| match op.as_rule() {
            Rule::add => Expr::BinaryExpr(Box::new(lhs), OpType::Add, Box::new(rhs)),
            Rule::subtract => Expr::BinaryExpr(Box::new(lhs), OpType::Sub, Box::new(rhs)),
            Rule::multiply => Expr::BinaryExpr(Box::new(lhs), OpType::Mult, Box::new(rhs)),
            Rule::divide => Expr::BinaryExpr(Box::new(lhs), OpType::Div, Box::new(rhs)),
            Rule::power => Expr::BinaryExpr(Box::new(lhs), OpType::Pow, Box::new(rhs)),
            Rule::modulus => Expr::BinaryExpr(Box::new(lhs), OpType::Modulus, Box::new(rhs)),
            Rule::lt => Expr::BinaryExpr(Box::new(lhs), OpType::Lt, Box::new(rhs)),
            Rule::gt => Expr::BinaryExpr(Box::new(lhs), OpType::Gt, Box::new(rhs)),
            Rule::and => Expr::BinaryExpr(Box::new(lhs), OpType::And, Box::new(rhs)),
            Rule::or => Expr::BinaryExpr(Box::new(lhs), OpType::Or, Box::new(rhs)),
            Rule::geq => Expr::BinaryExpr(Box::new(lhs), OpType::Geq, Box::new(rhs)),
            Rule::leq => Expr::BinaryExpr(Box::new(lhs), OpType::Leq, Box::new(rhs)),
            Rule::eq => Expr::BinaryExpr(Box::new(lhs), OpType::Eq, Box::new(rhs)),
            Rule::neq => Expr::BinaryExpr(Box::new(lhs), OpType::Neq, Box::new(rhs)),
            _ => unreachable!(),
        },
    )
}

/* parse the parameters from a function */
fn parse_parameters(params_rules: Pairs<Rule>) -> Result<Vec<(VarType, String)>, ParseError> {
    let mut params: Vec<(VarType, String)> = Vec::new();
    for param in params_rules {
        //each param is in form { var_type var_name }
        let mut pair = param.into_inner();

        let var_type = match pair.next() {
            Some(var_type) => parse_type_from_rule(var_type.into_inner().next().unwrap())?,
            None => break,
        };
        let var_name = pair.next().unwrap().as_str();
        params.push((var_type, var_name.to_string()))
    }
    Ok(params)
}

/* parse the parameters from a function */
fn parse_structure(mut attribute_rules: Pairs<Rule>) -> Result<Vec<(String, VarType)>, ParseError> {
    let mut attributes: Vec<(String, VarType)> = Vec::new();
    loop {
        //each param is in form { var_type var_name }
        let (var_name_rule, vtype_rule) = match attribute_rules.next() {
            Some(var_name_rule) => (var_name_rule, attribute_rules.next().unwrap()),
            None => break,
        };
        match (var_name_rule.as_rule(), vtype_rule.as_rule()) {
            (Rule::var_name, Rule::var_type) => {
                let attribute_name = var_name_rule.as_str().to_string();
                let attribute_type = parse_type_from_rule(vtype_rule)?;
                attributes.push((attribute_name, attribute_type));
            }
            _ => panic!("structures should be var_name, var_type"),
        }
    }
    Ok(attributes)
}

fn parse_type_from_rule(rule: Pair<Rule>) -> Result<VarType, ParseError> {
    Ok(match rule.as_rule() {
        Rule::vint => VarType::Int,
        Rule::vstring => VarType::String,
        Rule::vchar => VarType::Char,
        Rule::vbool => VarType::Bool,
        Rule::var_type => parse_type_from_rule(rule.into_inner().next().unwrap())?,
        Rule::hmap => {
            let mut inner_types = rule.into_inner();
            let key_type = parse_type_from_rule(match inner_types.next() {
                Some(type_rule) => match type_rule.as_rule() {
                    Rule::var_type => type_rule.into_inner().next().unwrap(),
                    _ => type_rule,
                },
                None => panic!("missing key type from Map"),
            })
            .expect("couldn't parse type from key type of map");
            let value_type = parse_type_from_rule(match inner_types.next() {
                Some(a_rule) => match a_rule.as_rule() {
                    Rule::var_type => a_rule.into_inner().next().unwrap(),
                    _ => a_rule,
                },
                None => panic!("missing key type from Map"),
            })
            .unwrap();
            VarType::Map(Box::new(key_type), Box::new(value_type))
        }
        Rule::array_inst => VarType::Array(Box::new(parse_type_from_rule(
            rule.into_inner().next().unwrap(),
        )?)),
        Rule::structure => {
            let structure_name = rule
                .into_inner()
                .next()
                .expect("Error trying to parse structure name")
                .as_str()
                .to_string();
            VarType::Struct(structure_name)
        }
        _ => panic!("parse_type_from_rule() error on {:?}", rule),
    })
}

/*
* parse the function from the pair provided.
*/
pub fn parse_function(pairs: &mut Pairs<Rule>, ast: &mut AstMap) -> Result<(), ParseError> {
    let fn_name_pair = pairs.next().unwrap();
    let position = fn_name_pair.as_span().start();
    let fn_name = fn_name_pair.as_str().to_string();

    let next_rule = pairs.next().unwrap();
    let (params, return_type) = match next_rule.as_rule() {
        Rule::params => (
            parse_parameters(next_rule.into_inner())?,
            parse_type_from_rule(pairs.next().unwrap().into_inner().next().unwrap())?,
        ),
        Rule::var_type => (
            Vec::new(),
            parse_type_from_rule(next_rule.into_inner().next().unwrap())?,
        ),
        _ => return Err(ParseError::NoReturnType),
    };

    let mut stms = Vec::new();
    for pair in pairs {
        stms.push(Box::new(parse_ast_node(pair, ast)?));
    }
    let function = Function {
        name: fn_name.clone(),
        return_type: return_type,
        params: params,
        statements: stms,
        position: position,
    };
    ast.func_map.insert(fn_name, function);
    Ok(())
}

pub fn parse_identifier(pair: Pair<Rule>) -> Identifier {
    let position = pair.as_span().start();
    let mut structure_index_rules = pair.into_inner();
    let var_name = structure_index_rules.next().unwrap().as_str().to_string();
    let tail = structure_index_rules
        .map(|rule| match rule.as_rule() {
            Rule::expr => IdentifierHelper::ArrayIndex(parse_expr(rule.into_inner())),
            Rule::var_name => IdentifierHelper::StructIndex(rule.as_str().to_string()),
            _ => panic!(),
        })
        .collect::<Vec<IdentifierHelper>>();
    return Identifier {
        var_name,
        tail,
        position,
    };
}

/*
* parses ast into nodes, only handles one clause at a time.
*/
pub fn parse_ast_node(pair: Pair<Rule>, ast: &mut AstMap) -> Result<AstNode, ParseError> {
    let rule = pair.as_rule();
    let statement = pair.as_str();
    match rule {
        Rule::func_def => {
            parse_function(&mut pair.into_inner(), ast)?;
            Ok(AstNode::Skip())
        }
        Rule::assignment => {
            let position = pair.as_span().start();
            let mut assignment_rules = pair.into_inner();
            let vtype_or_name = assignment_rules.next().unwrap();
            let (var_type, identifier) = match vtype_or_name.as_rule() {
                Rule::var_type => {
                    let vartype = parse_type_from_rule(vtype_or_name.into_inner().next().unwrap())?;
                    let identifier = Identifier {
                        var_name: assignment_rules.next().unwrap().as_str().to_string(),
                        tail: Vec::new(),
                        position,
                    };
                    (Some(vartype), identifier)
                }
                Rule::identifier => {
                    let identifier = parse_identifier(vtype_or_name);
                    (None, identifier)
                }
                _ => {
                    return {
                        Err(ParseError::FormatError(format!(
                            "error parsing statement: {} rule: {:?}",
                            statement,
                            vtype_or_name.as_rule()
                        )))
                    }
                }
            };
            let expression = parse_expr(assignment_rules.next().unwrap().into_inner());
            Ok(AstNode::Assignment {
                var_type: var_type,
                identifier: identifier,
                expr: expression,
            })
        }
        Rule::whilestm => {
            let mut inner_rules = pair.into_inner();
            let bool_exp = inner_rules.next().unwrap().into_inner();
            let bool_ast = parse_expr(bool_exp);

            let mut stms = Vec::new();
            for stm in inner_rules {
                stms.push(Box::new(parse_ast_node(stm, ast)?));
            }

            Ok(AstNode::While(bool_ast, stms))
        }
        Rule::ifstm => {
            let inner_rules = pair.into_inner();
            let mut if_else_list = Vec::new();
            for stm in inner_rules {
                let (ifstm, boolexp) = match stm.as_rule() {
                    Rule::else_stm => {
                        let inner = stm.into_inner();
                        (inner, Expr::Literal(Literal::Bool(true), 0))
                    }
                    _ => {
                        //only if or if else statements get to this point, they are handled the same
                        let mut inner = stm.into_inner();
                        let boolast = inner.next().unwrap().into_inner();
                        (inner, parse_expr(boolast))
                    }
                };
                //get the body of statements
                let mut stms: Vec<Box<AstNode>> = Vec::new();
                for stm in ifstm {
                    stms.push(Box::new(parse_ast_node(stm, ast)?));
                }
                if_else_list.push((boolexp, stms));
            }
            Ok(AstNode::If(if_else_list))
        }
        Rule::skip => return Ok(AstNode::Skip()),
        Rule::builtin => {
            let builtin = pair.into_inner().next().unwrap();
            match builtin.as_rule() {
                Rule::print => {
                    let expression = parse_expr(builtin.into_inner());
                    Ok(AstNode::BuiltIn(BuiltIn::Print(expression)))
                }
                Rule::static_print => {
                    let expression = parse_expr(builtin.into_inner());
                    Ok(AstNode::BuiltIn(BuiltIn::StaticPrint(expression)))
                }
                Rule::assert => Ok(AstNode::BuiltIn(BuiltIn::Assert(parse_expr(
                    builtin.into_inner(),
                )))),
                _ => unreachable!(),
            }
        }
        Rule::return_stm => {
            let return_expr = pair.into_inner().next().unwrap();
            Ok(AstNode::ReturnStm(parse_expr(return_expr.into_inner())))
        }
        Rule::structure_def => {
            let pairs = &mut pair.into_inner();
            let struct_name = pairs.next().unwrap().as_str().to_string();
            let next_rule = pairs.next().unwrap();
            let params = match next_rule.as_rule() {
                Rule::structure_items => (parse_structure(next_rule.into_inner())?),
                _ => return Err(ParseError::NoReturnType),
            };
            ast.struct_map.insert(struct_name, params);
            Ok(AstNode::Skip())
        }
        Rule::parse_error => {
            return Err(ParseError::GeneralParseError(format!(
                "unmatched rule while parsing ast {:?}",
                rule
            )))
        }
        Rule::import_stm => {
            use pest::Parser;
            let filename = pair
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .as_str();
            let file_contents = std::fs::read_to_string(filename)
                .expect(&format!("Error cannot read file {}", filename)[..]); //from file
            let pairs = WormParser::parse(Rule::program, &file_contents)
                .unwrap_or_else(|e| panic!("Error {} while reading file {}", e, filename));
            // parses the program into an AST, saves the functions AST in the state to be called upon later
            parse_program(pairs, ast)?;
            Ok(AstNode::Skip())
        }
        Rule::EOI => return Err(ParseError::EndOfInput),
        _ => {
            println!("UNREACHABLE");
            unreachable!();
        }
    }
}
