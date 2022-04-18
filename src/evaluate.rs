/*
* evaluates the ast, panics on missed type static errors, but catches execution errors and returns result
*/
use crate::ast::*;
use crate::state::{AstMap, ExecutionState};
use colored::*;
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone)]
pub enum ExecutionError {
    DivideByZero,
    AssertionError(Expr),
}

static MAIN: &str = "main";

/* run program calls the main function to run the program */
pub fn run_program(
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Literal, ExecutionError> {
    eval_func(MAIN.to_string(), execution_state, ast)
}

/// eval_func expects parameters and scope to already be handeled. Only calls statements
pub fn eval_func(
    name: String,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Literal, ExecutionError> {
    let function = ast.func_map.get(&name).unwrap();
    execution_state.increment_stack_level();
    for ast_node in &function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        if let Some(val) = eval_ast(ast_node, execution_state, ast)? {
            execution_state.pop_stack();
            return Ok(val);
        }
    }
    panic!("no return statement from function")
}

/// Saving a value requires us to explore the identifier's "tail"
/// ## Example
/// `object[i].attri = value;`
/// The tail in this case is [ArrayIndex(i), StructIndex(attri)]
///
/// We need to update the attri value, but then also the value in the array
/// and finally the object inside of var_map.
///
/// @param curr_value maintains the current value being accessed, so in
/// ArrayIndex(i) it contains the object and in StructIndex(attri) it contains
/// the array value.
///
/// @param final_value is the value that we should set at the tail of the tail.
fn recurse_identifier_tail(
    execution_state: &mut ExecutionState,
    ast: &AstMap,
    identifier_tail: &mut core::slice::Iter<IdentifierHelper>,
    curr_value: Literal,
    final_value: Literal,
) -> Literal {
    match identifier_tail.next() {
        Some(val) => match val {
            IdentifierHelper::ArrayIndex(expr) => match curr_value {
                Literal::Array(vtype, mut list) => {
                    match eval_expr(expr, execution_state, ast).unwrap() {
                        Literal::Int(i) => {
                            if (i as usize) >= list.len() {
                                panic!("Index out of bounds for an array")
                            }
                            list[i as usize] = recurse_identifier_tail(
                                execution_state,
                                ast,
                                identifier_tail,
                                list[i as usize].clone(),
                                final_value,
                            );
                            Literal::Array(vtype, list)
                        }
                        _ => panic!("Tried to index an array with a non int"),
                    }
                }
                Literal::Map(ktype, vtype, mut wmap) => {
                    let key = eval_expr(expr, execution_state, ast).unwrap();
                    match wmap.clone().get(&key) {
                        Some(literal) => {
                            let value = recurse_identifier_tail(
                                execution_state,
                                ast,
                                identifier_tail,
                                literal.clone(),
                                final_value,
                            );
                            wmap.insert(key, value);
                        }
                        None => {
                            wmap.insert(key, final_value);
                        }
                    };
                    Literal::Map(ktype, vtype, wmap)
                }
                Literal::String(s) => {
                    let index = eval_expr(expr, execution_state, ast).unwrap();
                    match index {
                        Literal::Int(i) => {
                            return {
                                if (i as usize) >= s.len() {
                                    panic!("Index out of bounds for {}", s)
                                }
                                Literal::Char(s.chars().nth(i as usize).unwrap())
                            }
                        }
                        _ => panic!("Only ints can index strings"),
                    }
                }
                _ => panic!("Tried to index a non string or non array"),
            },
            IdentifierHelper::StructIndex(attribute) => match curr_value {
                Literal::Struct { name, mut pairs } => {
                    let updated_val = pairs.get(&attribute.clone()).unwrap().clone();
                    pairs.insert(
                        attribute.clone(),
                        recurse_identifier_tail(
                            execution_state,
                            ast,
                            identifier_tail,
                            updated_val,
                            final_value,
                        ),
                    );
                    Literal::Struct { name, pairs }
                }
                _ => panic!("struct index on a non struct"),
            },
        },
        None => final_value,
    }
}

fn save_value(
    execution_state: &mut ExecutionState,
    ast: &AstMap,
    identifier: &Identifier,
    value: Literal,
) {
    let curr_value = execution_state
        .var_map
        .get(&identifier.var_name)
        .unwrap()
        .clone();
    let value = recurse_identifier_tail(
        execution_state,
        ast,
        &mut identifier.tail.iter(),
        curr_value,
        value,
    );
    execution_state.var_map.insert(identifier.var_name.to_owned(), value);
}

/*
* evaluate an ast, one line or one if/while stm
*/
pub fn eval_ast(
    ast_node: &AstNode,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Option<Literal>, ExecutionError> {
    match ast_node {
        AstNode::Assignment {
            var_type,
            identifier,
            expr,
        } => {
            let value = eval_expr(&expr, execution_state, ast)?;
            let actual_val = match (var_type.clone(), value) {
                (Some(VarType::Int), Literal::Char(c)) => Literal::Int(c as i32),
                (_, val) => val,
            };
            match var_type {
                Some(_) => execution_state.save_variable(identifier.var_name.to_owned(), actual_val),
                None => {
                    save_value(execution_state, ast, identifier, actual_val);
                }
            }
        }
        AstNode::If(if_pairs) => {
            execution_state.increment_stack_level();
            for (conditional, stms) in if_pairs {
                if eval_bool_expr(&conditional, execution_state, ast)? {
                    for stm in stms.iter() {
                        if let Some(eval) = eval_ast(stm, execution_state, ast)? { 
                            return Ok(Some(eval)) 
                        }
                    }
                    break;
                }
            }
            execution_state.pop_stack();
        }
        AstNode::While(conditional, stms) => {
            while eval_bool_expr(&conditional, execution_state, ast)? {
                execution_state.increment_stack_level();
                for stm in stms.iter() {
                    if let Some(eval) = eval_ast(stm, execution_state, ast)? { 
                        return Ok(Some(eval)) 
                    }
                }
                execution_state.pop_stack();
            }
        }
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    println!("\"{}\" => {}", exp, eval_expr(&exp, execution_state, ast)?);
                }
                BuiltIn::StaticPrint(_) => (),
                BuiltIn::Assert(boolexp) => {
                    if !eval_bool_expr(&boolexp, execution_state, ast)? {
                        return Err(ExecutionError::AssertionError(boolexp.clone()));
                    } else {
                        println!("{} {}", "ASSERTION PASS:".blue(), boolexp);
                    }
                }
            }
        }
        AstNode::ReturnStm(expr) => {
            return Ok(Some(eval_expr(&expr, execution_state, ast)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/*
* evalulates booleans based on their conjunction
*/
fn eval_bool_expr(
    bool_expr: &Expr,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<bool, ExecutionError> {
    match eval_expr(bool_expr, execution_state, ast)? {
        Literal::Bool(a) => Ok(a),
        _ => panic!("expected boolean result in conditional"),
    }
}

/*
* eval_expr evaluates inline expressions
*/
fn eval_expr(
    exp: &Expr,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Literal, ExecutionError> {
    match exp.clone() {
        Expr::Identifier(identifier) => eval_identifier(identifier, execution_state, ast),
        Expr::Literal(literal, _) => match literal {
            Literal::Array(var_type, elements) => Ok(Literal::Array(var_type, elements)),
            _ => Ok(literal),
        },
        Expr::UnaryExpr(op, expr) => {
            let value = eval_expr(&expr, execution_state, ast)?;
            match (op, value) {
                (UnaryOp::Not, Literal::Bool(value)) => Ok(Literal::Bool(!value)),
                _ => panic!("Type error on unaryexpr"),
            }
        }
        Expr::FnCall {
            name,
            params,
            position,
        } => eval_fn_call(
            FnCall {
                name,
                params,
                position,
            },
            execution_state,
            ast,
        ),
        Expr::ListComprehension {
            piped_var,
            value_expr,
            in_expr,
        } => {
            let len = match eval_expr(&in_expr, execution_state, ast)? {
                Literal::Int(i) => i as usize,
                _ => panic!("type mismatch found during execution"),
            };
            // elements of the array
            let mut elements: Vec<Literal> = Vec::new();
            let (variable, pipe) = match piped_var {
                Some(piped) => (piped, true),
                None => (String::from(""), false),
            };
            execution_state.increment_stack_level();
            for i in 0..len {
                // not currently type checking need to add that later on
                if pipe {
                    execution_state
                        .var_map
                        .insert(variable.clone(), Literal::Int(i as i32));
                }
                elements.push(eval_expr(&value_expr.clone(), execution_state, ast)?);
            }
            execution_state.pop_stack();
            Ok(Literal::Array(VarType::Generic, elements))
        }
        Expr::BinaryExpr(lhs, op, rhs) => {
            let left = eval_expr(&*lhs, execution_state, ast)?;
            let right = eval_expr(&*rhs, execution_state, ast)?;
            use Literal::*;
            use OpType::*;
            match op {
                Eq => Ok(Bool(left == right)),
                Neq => Ok(Bool(left != right)),
                Lt => Ok(Bool(left < right)),
                Gt => Ok(Bool(left > right)),
                Leq => Ok(Bool(left <= right)),
                Geq => Ok(Bool(left >= right)),
                And => match (left, right) {
                    (Bool(left), Bool(right)) => Ok(Bool(left && right)),
                    _ => panic!("left or right didn't evaluate to bool in `and`"),
                },
                Or => match (left, right) {
                    (Bool(left), Bool(right)) => Ok(Bool(left || right)),
                    _ => panic!("left or right didn't evaluate to bool in `or`"),
                },
                Add => match (left, right) {
                    (Int(l), Int(r)) => Ok(Int((l + r) as i32)),
                    (String(l), String(r)) => Ok(String(format!("{}{}", l, r))),
                    (Char(l), Char(r)) => return Ok(String(format!("{}{}", l, r))),
                    (Char(l), String(r)) => return Ok(String(format!("{}{}", l, r))),
                    (String(l), Char(r)) => return Ok(String(format!("{}{}", l, r))),
                    _ => panic!("expr operation on mismatching types"),
                },
                operator => {
                    // extract integer values, or add chars and strings
                    let (l, r, var_type) = match (left, right) {
                        (Int(l), Int(r)) => (l, r, VarType::Int),
                        _ => panic!("expr operation on mismatching types"),
                    };
                    let res = match operator {
                        Add => l + r,
                        Sub => l - r,
                        Mult => l * r,
                        Div => {
                            if r == 0 {
                                return Err(ExecutionError::DivideByZero);
                            }
                            l / r
                        }
                        Pow => {
                            // println!("{} ^ {}", l, r);
                            l.pow(r as u32)
                        },
                        Modulus => l % r,
                        _ => panic!("unmatched arm for operator"),
                    };
                    match var_type {
                        VarType::Int => Ok(Int(res as i32)),
                        _ => {
                            panic!("type violation caught in execution while trying to evaluate expression")
                        }
                    }
                }
            }
        }
    }
}

fn eval_identifier(
    identifier: Identifier,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Literal, ExecutionError> {
    let mut curr_value = execution_state
        .var_map
        .get(&identifier.var_name)
        .unwrap()
        .clone();
    for ih in identifier.tail {
        match ih {
            IdentifierHelper::ArrayIndex(exp) => match curr_value {
                Literal::Array(_, list) => match eval_expr(&exp, execution_state, ast)? {
                    Literal::Int(i) => {
                        curr_value = list[i as usize].clone();
                    }
                    _ => panic!("Tried an array with an non int value"),
                },
                Literal::Map(_, _, wmap) => {
                    let key = &eval_expr(&exp, execution_state, ast)?;
                    curr_value = wmap
                        .get(key)
                        .expect(&format!("Could not find value \"{}\" in map", key)[..])
                        .clone();
                }
                Literal::String(s) => {
                    let index = eval_expr(&exp, execution_state, ast).unwrap();
                    match index {
                        Literal::Int(i) => {
                            return Ok(Literal::Char(s.chars().nth(i as usize).unwrap()))
                        }
                        _ => panic!("Only ints can index strings"),
                    }
                }
                _ => panic!("[] index to neither map, array or string"),
            },
            IdentifierHelper::StructIndex(attribute) => match curr_value {
                Literal::Struct { name: _, pairs } => {
                    curr_value = pairs.get(&attribute).unwrap().clone()
                }
                _ => panic!("struct index to a non struct"),
            },
        }
    }
    Ok(curr_value)
}

fn eval_reserved_functions(
    func_call: &FnCall,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Literal, ExecutionError> {
    match &func_call.name[..] {
        LEN => match eval_expr(&func_call.params[0].clone(), execution_state, ast)? {
            Literal::Array(_, elements) => Ok(Literal::Int(elements.len() as i32)),
            Literal::String(s) => Ok(Literal::Int(s.len() as i32)),
            _ => panic!("panicked tried to find the length of a non array string"),
        },
        PARSE_INT => {
            match eval_expr(&func_call.params[0].clone(), execution_state, ast)? {
                Literal::String(s) => {
                    // println!("parse int from `{}`", s);
                    Ok(Literal::Int(s.parse::<i32>().expect(
                        &("Expected to parse int, got ".to_owned() + &s[..]),
                    )))
                }
                Literal::Char(c) => Ok(Literal::Int(c as i32 - 48)),
                _ => panic!("panicked tried to find the length of a non array string"),
            }
        }
        USER_INPUT => {
            let mut line = String::new();
            std::io::stdin().read_line(&mut line).unwrap();
            Ok(Literal::String(line))
        }
        APPEND => {
            let value = eval_expr(&func_call.params[0].clone(), execution_state, ast)?;
            match value {
                Literal::Array(vtype, mut values) => {
                    values.push(eval_expr(
                        &func_call.params[1].clone(),
                        execution_state,
                        ast,
                    )?);
                    Ok(Literal::Array(vtype, values))
                }
                _ => panic!("Tried appending non array"),
            }
        }
        PREPEND => {
            let value = eval_expr(&func_call.params[0].clone(), execution_state, ast)?;
            match value {
                Literal::Array(vtype, mut values) => {
                    values.insert(
                        0,
                        eval_expr(&func_call.params[1].clone(), execution_state, ast)?,
                    );
                    Ok(Literal::Array(vtype, values))
                }
                _ => panic!("Tried appending non array"),
            }
        }
        REMOVE => {
            let value = eval_expr(&func_call.params[0].clone(), execution_state, ast)?;
            let index = eval_expr(&func_call.params[1].clone(), execution_state, ast)?;
            match (value, index) {
                (Literal::Array(vtype, mut values), Literal::Int(i)) => {
                    values.remove(i as usize);
                    Ok(Literal::Array(vtype, values))
                }
                (Literal::String(mut values), Literal::Int(i)) => {
                    values.remove(i as usize);
                    Ok(Literal::String(values))
                }
                _ => panic!("Tried poping non array"),
            }
        }
        TO_STR => match eval_expr(&func_call.params[0].clone(), execution_state, ast)? {
            Literal::Int(i) => Ok(Literal::String(i.to_string())),
            Literal::Char(c) => Ok(Literal::String(c.to_string())),
            _ => panic!("panicked tried to find the length of a non array string"),
        },
        _ => {
            panic!("reserved function not found in eval")
        }
    }
}

fn eval_fn_call(
    func_call: FnCall,
    execution_state: &mut ExecutionState,
    ast: &AstMap,
) -> Result<Literal, ExecutionError> {
    // need a new var map for the function, just the parameters
    if RESERVED_FUNCTIONS.contains(&func_call.name[..]) {
        return eval_reserved_functions(&func_call, execution_state, ast);
    }

    let mut var_map: HashMap<String, Literal> = HashMap::new();
    let fn_name = func_call.name;
    let function = match ast.func_map.get(&fn_name) {
        Some(function) => function,
        None => {
            // STRUCT CONSTRUCTOR
            let type_map = ast
                .struct_map
                .get(&fn_name)
                .expect("Value does not exist for function/constructor");
            // iterate through the parameters provided and the function def,
            let mut pairs = BTreeMap::new();
            for (expr, (param_name, _)) in func_call.params.iter().zip(type_map.iter()) {
                let param_const = eval_expr(&expr.clone(), execution_state, ast)?;
                pairs.insert(param_name.clone(), param_const);
            }
            return Ok(Literal::Struct {
                name: fn_name,
                pairs,
            });
        }
    };
    let mut var_stack: Vec<(String, u32)> = Vec::new();
    let params = function.params.clone();
    // iterate through the parameters provided and the function def,
    for (expr, (_, param_name)) in func_call.params.iter().zip(params.iter()) {
        let param_const = eval_expr(&expr.clone(), execution_state, ast)?;
        var_stack.push((param_name.clone(), 0));
        var_map.insert(param_name.to_string(), param_const);
    }
    let mut func_state = ExecutionState {
        var_map,
        var_stack,
        stack_lvl: 0,
    };
    eval_func(fn_name, &mut func_state, ast)
}
