/*
* evaluates the ast, panics on missed type static errors, but catches execution errors and returns result
*/
use crate::ast::*;
use crate::state::{State, ExecutionState};
use colored::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ExecutionError {
    DivideByZero,
    AssertionError(BoolAst),
    IndexOutOfBounds(String, usize),
}

static MAIN: &str = "main";

/* run program calls the main function to run the program */
pub fn run_program(execution_state: &mut ExecutionState, state: &State) -> Result<Constant, ExecutionError> {
    Ok(eval_func(MAIN.to_string(), execution_state, state)?)
}

/* execute turns a ast object into a Result */
pub fn eval_func(name: String, execution_state: &mut ExecutionState, state: &State) -> Result<Constant, ExecutionError> {
    let function = state.func_map.get(&name).unwrap();
    execution_state.increment_stack_level();
    for ast in &function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast((**ast).clone(), execution_state, state)? {
            Some(val) => {
                execution_state.pop_stack();
                return Ok(val);
            }
            None => (),
        }
    }
    panic!("no return statement from function")
}

/*
* evaluate an ast, one line or one if/while stm
*/
fn eval_ast(ast: AstNode, execution_state: &mut ExecutionState, state: &State) -> Result<Option<Constant>, ExecutionError> {
    match ast {
        AstNode::Function(_) => {
            panic!("Why are we here");
            // return Ok(Some(eval_func(func, execution_state, state)?));
        }
        AstNode::Assignment(vtype, name, exp) => {
            let value = eval_expr(exp, execution_state, state)?;
            let actual_val = match (vtype, value) {
                (Some(VarType::Int), Constant::Char(c)) => Constant::Int(c as i32),
                (_, val) => val,
            };
            execution_state.save_variable(name, actual_val);
        }
        AstNode::StructIndexAssignment(struct_name,attribute,value) => {
            match execution_state.var_map.get(&struct_name) {
                Some(constant) => {
                    match constant.clone() {
                        Constant::Struct(mut wstruct) => {
                            wstruct.insert(attribute.clone(), eval_expr(value, execution_state, state)?);
                            execution_state.var_map.insert(struct_name, Constant::Struct(wstruct));
                        }
                        _ => panic!("constant ahh")
                    }
                }
                None => panic!("MISSING")
            }
        }, // TODO
        AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
            let len = match eval_expr(length_exp, execution_state, state)? {
                Constant::Int(i) => i as usize,
                _ => panic!("type mismatch found during execution"),
            };
            // elements of the array
            let mut elements: Vec<Constant> = Vec::new();
            let (variable, pipe) = match piped {
                Some(piped) => (piped, true),
                None => (String::from(""), false),
            };
            execution_state.increment_stack_level();
            for i in 0..len {
                // not currently type checking need to add that later on
                if pipe {
                    execution_state
                        .var_map
                        .insert(variable.clone(), Constant::Int(i as i32));
                }
                elements.push(eval_expr(value_exp.clone(), execution_state, state)?);
            }
            execution_state.pop_stack();
            execution_state.save_variable(name, Constant::Array(var_type, elements));
        }
        AstNode::IndexAssignment(name, index_exp, value_exp) => {
            let (var_type, mut elements) = match execution_state.var_map.get(&name).unwrap().clone() {
                Constant::Array(var_type, elements) => (var_type, elements),
                Constant::Map(key_type, val_type, mut hashmap) => {
                    let index = eval_expr(index_exp, execution_state, state)?;
                    let value = eval_expr(value_exp, execution_state, state)?;
                    hashmap.insert(index, value); // insert into hash Constant:Constant Pair
                    execution_state.save_variable(name, Constant::Map(key_type, val_type, hashmap));
                    return Ok(None);
                }
                _ => panic!("type mismatch found during execution"),
            };
            let index = match eval_expr(index_exp, execution_state, state)? {
                Constant::Int(i) => i as usize,
                _ => panic!("type mismatch found during execution"),
            };
            if index > elements.len() {
                return Err(ExecutionError::IndexOutOfBounds(name.clone(), index));
            }
            let value = eval_expr(value_exp, execution_state, state)?;
            elements[index] = value;
            execution_state.save_variable(name, Constant::Array(var_type, elements));
        }
        AstNode::If(if_pairs) => {
            execution_state.increment_stack_level();
            for (conditional, mut stms) in if_pairs {
                if eval_bool_ast(&conditional, execution_state, state)? {
                    while stms.len() > 0 {
                        match eval_ast(*stms.remove(0), execution_state, state)? {
                            Some(eval) => return Ok(Some(eval)),
                            None => (),
                        }
                    }
                    break;
                }
            }
            execution_state.pop_stack();
        }
        AstNode::While(conditional, stms) => {
            while eval_bool_ast(&conditional, execution_state, state)? {
                execution_state.increment_stack_level();
                for stm in stms.iter() {
                    match eval_ast(*stm.clone(), execution_state, state)? {
                        Some(eval) => return Ok(Some(eval)),
                        None => (),
                    }
                }
                execution_state.pop_stack();
            }
        }
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    println!("\"{}\" => {}", exp, eval_expr(exp.clone(),execution_state, state)?);
                }
                BuiltIn::StaticPrint(_) => (),
                BuiltIn::Assert(boolexp) => {
                    if !eval_bool_ast(&boolexp, execution_state, state)? {
                        return Err(ExecutionError::AssertionError(boolexp));
                    } else {
                        println!("{} {}", "ASSERTION PASS:".blue(), boolexp);
                    }
                }
            }
            ()
        }
        AstNode::ReturnStm(expr) => {
            return Ok(Some(eval_expr(expr, execution_state, state)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/*
* evalulates booleans based on their conjunction
*/
fn eval_bool_ast(bool_ast: &BoolAst, execution_state: &mut ExecutionState, state: &State) -> Result<bool, ExecutionError> {
    Ok(match &*bool_ast {
        BoolAst::Not(body) => !eval_bool_ast(&*body, execution_state, state)?,
        BoolAst::And(a, b) => {
            if eval_bool_ast(&*a, execution_state, state)? {
                // only evaluate the second if the first is true
                eval_bool_ast(&*b, execution_state, state)?
            } else {
                false
            }
        }
        BoolAst::Or(a, b) => eval_bool_ast(&*a, execution_state, state)? | eval_bool_ast(&*b, execution_state, state)?,
        BoolAst::Exp(exp) => eval_bool(&*exp, execution_state, state)?,
        BoolAst::Const(boolean) => *boolean,
    })
}

/*
* evaluates expressions and constants to true false values
*/
fn eval_bool(bool_exp: &BoolExp, execution_state:&mut ExecutionState, state: &State) -> Result<bool, ExecutionError> {
    let BoolExp(lhs, op, rhs) = &*bool_exp;
    use Constant::*;
    let (lres, rres) = match (
        eval_expr(lhs.clone(), execution_state, state)?, 
        eval_expr(rhs.clone(), execution_state, state)?,
    ) {
        (Int(i), Int(j)) => (i as f64, j as f64),
        (Float(i), Float(j)) => (i, j),
        (Char(i), Char(j)) => (i as u32 as f64, j as u32 as f64),
        (Map(_,_,_), _) => panic!("type violation in eval_bool, cannot compare map"),
        (_, Map(_,_,_)) => panic!("type violation in eval_bool, cannot compare map"),
        (String(s1), String(s2)) => {
            return Ok(match op {
                BoolOp::Eq => s1 == s2,
                BoolOp::Neq => s1 != s2,
                BoolOp::Leq => s1 <= s2,
                BoolOp::Geq => s1 >= s2,
                BoolOp::Lt => s1 < s2,
                BoolOp::Gt => s1 > s2,
            });
        }
        _ => panic!("type violation in eval_bool\n{:?} != {:?}", eval_expr(lhs.clone(), execution_state, state)?, eval_expr(rhs.clone(), execution_state, state)?),
    };
    Ok(match op {
        BoolOp::Eq => lres == rres,
        BoolOp::Neq => lres != rres,
        BoolOp::Leq => lres <= rres,
        BoolOp::Geq => lres >= rres,
        BoolOp::Lt => lres < rres,
        BoolOp::Gt => lres > rres,
    })
}

/*
* eval_expr evaluates inline expressions
*/
fn eval_expr(exp: Expr, execution_state: &mut ExecutionState, state: &State) -> Result<Constant, ExecutionError> {
    match exp.clone() {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // get variable as a constant value
                    // println!("execute name lookup: {}", name.clone());
                    Ok(execution_state.var_map.get(&name).unwrap().clone())
                }
                Object::Constant(Constant::Array(var_type, elements)) => {
                    Ok(Constant::Array(var_type, elements))
                }
                Object::Constant(Constant::Index(name, index_exp)) => {
                    // get array from state map
                    // get the index, if its not number return error

                    match execution_state.var_map.get(&name.clone()).unwrap().clone() {
                        Constant::Array(_var_type, mut elements) => {
                            let index = match eval_expr(*index_exp, execution_state, state)? {
                                Constant::Int(i) => i as usize,
                                _ => panic!("array index not a number"),
                            };
                            if elements.len() <= index {
                                println!("exp: {}", exp);
                                return Err(ExecutionError::IndexOutOfBounds(name, index));
                            } else {
                                return Ok(elements.remove(index));
                            }
                        }
                        Constant::String(s) => {
                            let index = match eval_expr(*index_exp, execution_state, state)? {
                                Constant::Int(i) => i as usize,
                                _ => panic!("array index not a number"),
                            };
                            if s.len() <= index {
                                return Err(ExecutionError::IndexOutOfBounds(name, index));
                            } else {
                                return Ok(Constant::Char(s.as_bytes()[index as usize] as char));
                            }
                        }
                        Constant::Map(_,_,hashmap) => {
                            let index = eval_expr(*index_exp, execution_state, state)?;
                            match hashmap.get(index) {
                                Some(val) => return Ok(val),
                                None => panic!("Hashmap value does not exist"),
                            }
                        }
                        _ => panic!("trying to index a variable thats not an array"),
                    };
                }
                Object::Constant(Constant::StructVal(name, attribute)) => {
                    let constant = execution_state.var_map.get(&name).expect("Value does not exist for function/constructor");
                    match constant {
                        Constant::Struct(worm_struct) => {
                            match worm_struct.clone().get(attribute) {
                                Some(val) => return Ok(val),
                                None => panic!("Error undefined attribuite")
                            }
                        },
                        _ => panic!("Non struct attempted to be accessed")
                    }
                }
                Object::Constant(constant) => Ok(constant),
                Object::FnCall(func_call) => {
                    // need a new var map for the function, just the parameters
                    if func_call.name == "len" {
                        match eval_expr(func_call.params[0].clone(), execution_state, state)? {
                            Constant::Array(_, elements) => {
                                Ok(Constant::Int(elements.len() as i32))
                            }
                            Constant::String(s) => Ok(Constant::Int(s.len() as i32)),
                            _ => panic!("panicked tried to find the length of a non array string"),
                        }
                    } else if func_call.name == "parse_int" {
                        match eval_expr(func_call.params[0].clone(), execution_state, state)? {
                            Constant::String(s) => {
                                // println!("parse int from `{}`", s);
                                Ok(Constant::Int(s.parse::<i32>().unwrap()))
                            }
                            Constant::Char(c) => Ok(Constant::Int(c as i32 - 48)),
                            _ => panic!("panicked tried to find the length of a non array string"),
                        }
                    } else if func_call.name == "user_input" {
                        let mut line = String::new();
                        std::io::stdin().read_line(&mut line).unwrap();
                        Ok(Constant::String(line))
                    } else if func_call.name == "append" {
                        let value = eval_expr(func_call.params[0].clone(), execution_state, state)?;
                        match value {
                            Constant::Array(vtype, mut values) => {
                                values.push(
                                    eval_expr(func_call.params[1].clone(), execution_state, state)?
                                );
                                Ok(Constant::Array(vtype, values))
                            }
                            _ => panic!("Tried appending non array")
                        }
                    } else if func_call.name == "to_string" {
                        match eval_expr(func_call.params[0].clone(), execution_state, state)? {
                            Constant::Int(i) => Ok(Constant::String(i.to_string())),
                            Constant::Char(c) => Ok(Constant::String(c.to_string())),
                            _ => panic!("panicked tried to find the length of a non array string"),
                        }
                    } else {
                        let mut var_map: HashMap<String, Constant> = HashMap::new();
                        let fn_name = func_call.name;
                        let function = match state.func_map.get(&fn_name) {
                            Some(function) => function,
                            None => {
                                // STRUCT CONSTRUCTOR
                                let type_map = state.struct_map.get(&fn_name).expect("Value does not exist for function/constructor");
                                // iterate through the parameters provided and the function def,
                                let mut w = WormStruct {name: fn_name.clone(), pairs: Vec::new() };
                                w.name = fn_name;
                                for (expr, (param_name, _)) in func_call.params.iter().zip(type_map.iter()) {
                                    let param_const = eval_expr(expr.clone(), execution_state, state)?;
                                    w.insert(param_name.clone(), param_const);
                                }
                                return Ok(Constant::Struct(w))

                            }
                        };
                        let mut var_stack: Vec<(String, u32)> = Vec::new();
                        let params = function.params.clone();
                        // iterate through the parameters provided and the function def,
                        for (expr, (_, param_name)) in func_call.params.iter().zip(params.iter()) {
                            let param_const = eval_expr(expr.clone(), execution_state, state)?;
                            var_stack.push((param_name.clone(), 0));
                            var_map.insert(param_name.to_string(), param_const);
                        }
                        let mut func_state = ExecutionState {
                            var_map,
                            var_stack,
                            stack_lvl: 0,
                        };
                        Ok(eval_func(fn_name, &mut func_state, state)?)
                    }
                }
            }
        }
        Expr::ExpOp(lhs, op, rhs) => {
            let left = eval_expr(*lhs, execution_state, state)?;
            let right = eval_expr(*rhs, execution_state, state)?;
            use Constant::*;
            let (l, r, var_type) = match (left, right) {
                (Int(l), Int(r)) => (l as f64, r as f64, VarType::Int),
                (Float(l), Float(r)) => (l, r, VarType::Float),
                (String(l), String(r)) => match op {
                    OpType::Add => return Ok(Constant::String(format!("{}{}", l, r))),
                    _ => panic!("expr operation on strings only allow +"),
                },
                (Char(l), Char(r)) => match op {
                    OpType::Add => return Ok(Constant::String(format!("{}{}", l, r))),
                    _ => panic!("expr operation on strings only allow +"),
                },
                (Char(l), String(r)) => match op {
                    OpType::Add => return Ok(Constant::String(format!("{}{}", l, r))),
                    _ => panic!("expr operation on strings only allow +"),
                },
                (String(l), Char(r)) => match op {
                    OpType::Add => return Ok(Constant::String(format!("{}{}", l, r))),
                    _ => panic!("expr operation on strings only allow +"),
                },
                _ => panic!("expr operation on mismatching types"),
            };
            let res = match op {
                OpType::Add => l + r,
                OpType::Sub => l - r,
                OpType::Mult => l * r,
                OpType::Div => {
                    if r == 0.0 {
                        return Err(ExecutionError::DivideByZero);
                    }
                    l / r
                }
                OpType::Pow => l.powf(r),
                OpType::Modulus => l % r,
            };
            match var_type {
                VarType::Int => Ok(Constant::Int(res as i32)),
                VarType::Float => Ok(Constant::Float(res)),
                _ => {
                    panic!("type violation caught in execution while trying to evaluate expression")
                }
            }
        }
    }
}
