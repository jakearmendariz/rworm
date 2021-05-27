use crate::ast::{*};
use std::collections::HashMap;
use colored::*;


#[derive(Debug, Clone)]
pub enum ExecutionError {
    DivideByZero,
    AssertionError(BoolAst),
    IndexOutOfBounds(String, usize),
}

/* run program calls the main function to run the program */
pub fn run_program(state:&mut State) -> Result<Constant, ExecutionError> {
    let main_function = state.func_map.get("main").unwrap();
    Ok(eval_func(main_function.clone(), state)?)
}

/* execute turns a ast object into a Result */
pub fn eval_func(function:Function, state:&mut State) -> Result<Constant, ExecutionError> {
    state.increment_stack_level();
    for ast in function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast(*ast, state)? {
            Some(val) => {
                state.pop_stack();
                return Ok(val)
            },
            None => ()
        }
    }
    panic!("no return statement from function")
}

/*
* evaluate an ast, one line or one if/while stm
*/
fn eval_ast(ast:AstNode, state:&mut State) -> Result<Option<Constant>, ExecutionError> {
    match ast {
        AstNode::Assignment(vtype, name, exp) => {
            let value = eval_expr(exp, state)?;
            //casting
            let actual_val = match (vtype, value) {
                (Some(VarType::Int), Constant::Char(c)) => Constant::Int(c as i32),
                (_, val) => val,
            };
            state.save_variable(name, actual_val);
        },
        AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
            let len = match eval_expr(length_exp, state)? {
                Constant::Int(i) => i as usize,
                _ => panic!("type mismatch found during execution"),
            };
            // elements of the array
            let mut elements:Vec<Constant> = Vec::new();
            let (variable, pipe) = match piped {
                Some(piped) => (piped, true),
                None => (String::from(""), false)
            };
            state.increment_stack_level();
            for i in 0..len {
                // not currently type checking need to add that later on
                if pipe {
                    state.var_map.insert(variable.clone(), Constant::Int(i as i32));
                }
                elements.push(eval_expr(value_exp.clone(), state)?);
            }
            state.pop_stack();
            state.save_variable(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayFromExp(_, name, expr) => {
            // expecting a function that returns an array
            let (var_type, elements) = match eval_expr(expr, state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => panic!("type mismatch found during execution"),
            };
            state.save_variable(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayIndexAssignment(name, index_exp, value_exp) => {
            let (var_type, mut elements) = match state.var_map.get(&name).unwrap().clone() {
                Constant::Array(var_type, elements) => (var_type, elements),
                Constant::Map(mut hashmap) => {
                    let index = eval_expr(index_exp, state)?;
                    let value = eval_expr(value_exp, state)?;
                    hashmap.insert(index, value); // insert into hash Constant:Constant Pair
                    state.save_variable(name, Constant::Map(hashmap));
                    return Ok(None);
                },
                _ => panic!("type mismatch found during execution"),
            };
            let index = match eval_expr(index_exp, state)? {
                Constant::Int(i) => i as usize,
                _ => panic!("type mismatch found during execution")
            };
            if index > elements.len() {
                return Err(ExecutionError::IndexOutOfBounds(name.clone(), index));
            }
            let value = eval_expr(value_exp, state)?;
            elements[index] = value;
            state.save_variable(name, Constant::Array(var_type, elements));
        },
        AstNode::If(if_pairs) => {
            state.increment_stack_level();
            for (conditional, mut stms) in if_pairs {
                if eval_bool_ast(&conditional, state)? {
                    while stms.len() > 0 {
                        match eval_ast(*stms.remove(0), state)? {
                            Some(eval) => return Ok(Some(eval)),
                            None => ()
                        }
                    }
                    break;
                }
            }
            state.pop_stack();
        },
        AstNode::While(conditional, stms) => {
            while eval_bool_ast(&conditional, state)? {
                state.increment_stack_level();
                for stm in stms.iter() {
                    match eval_ast(*stm.clone(), state)? {
                        Some(eval) => return Ok(Some(eval)),
                        None => ()
                    }
                }
                state.pop_stack();
            }
            
        },
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    println!("\"{}\" => {}", exp, eval_expr(exp.clone(), state)?);
                },
                BuiltIn::StaticPrint(_) => {
                    ()
                },
                BuiltIn::Assert(boolexp) => {
                    if ! eval_bool_ast(&boolexp, state)? {
                        return Err(ExecutionError::AssertionError(boolexp));
                    } else {
                        println!("{} {}", "ASSERTION PASS:".blue(), boolexp);
                    }
                },
            }
            ()
        },
        AstNode::ReturnStm(expr) => {
            return Ok(Some(eval_expr(expr, state)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/* 
* evalulates booleans based on their conjunction 
*/
fn eval_bool_ast(bool_ast:&BoolAst, state:&mut State) ->  Result<bool, ExecutionError> {
    Ok(match &*bool_ast {
        BoolAst::Not(body) => !eval_bool_ast(&*body, state)?,
        BoolAst::And(a, b) => {
            if eval_bool_ast(&*a, state)? { // only evaluate the second if the first is true
                eval_bool_ast(&*b, state)?
            }else {
                false
            }
        },
        BoolAst::Or(a,b) => eval_bool_ast(&*a, state)? | eval_bool_ast(&*b, state)?,
        BoolAst::Exp(exp) => eval_bool(&*exp, state)?,
        BoolAst::Const(boolean) => *boolean,
    })
}

/* 
* evaluates expressions and constants to true false values 
*/
fn eval_bool(bool_exp:&BoolExp, state:&mut State) ->  Result<bool, ExecutionError> {
    let BoolExp(lhs,op,rhs)= &*bool_exp;
    use Constant::{*};
    let (lres, rres) = match (eval_expr(lhs.clone(), state)?, eval_expr(rhs.clone(), state)?) {
        (Int(i), Int(j)) => (i as f64,j as f64),
        (Float(i), Float(j)) => (i, j),
        (Char(i), Char(j)) => (i as u32 as f64,j as u32 as f64),
        (Map(_), _) => panic!("type violation in eval_bool, cannot compare map"),
        (_, Map(_)) => panic!("type violation in eval_bool, cannot compare map"),
        (String(s1), String(s2)) => {
            return Ok(match op {
                BoolOp::Eq => s1 == s2,
                BoolOp::Neq => s1 != s2,
                BoolOp::Leq => s1 <= s2,
                BoolOp::Geq => s1 >= s2,
                BoolOp::Lt => s1 < s2,
                BoolOp::Gt => s1 > s2
            });
        }
        _ => panic!("type violation in eval_bool"),
    };
    Ok(match op {
        BoolOp::Eq => lres == rres,
        BoolOp::Neq => lres != rres,
        BoolOp::Leq => lres <= rres,
        BoolOp::Geq => lres >= rres,
        BoolOp::Lt => lres < rres,
        BoolOp::Gt => lres > rres
    })
}


/* 
* eval_expr evaluates inline expressions 
*/
fn eval_expr(exp:Expr, state:&mut State) -> Result<Constant, ExecutionError> {
    match exp.clone() {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // get variable as a constant value
                    // println!("execute name lookup: {}", name.clone());
                    Ok(state.var_map.get(&name).unwrap().clone())
                },
                Object::Constant(Constant::Array(var_type, elements)) => Ok(Constant::Array(var_type, elements)),
                Object::Constant(Constant::ArrayIndex(name, index_exp)) => {
                    // get array from state map
                    // get the index, if its not number return error

                    match state.var_map.get(&name.clone()).unwrap().clone() {
                        Constant::Array(_var_type, mut elements) => {
                            let index = match eval_expr(*index_exp, state)? {
                                Constant::Int(i) => i as usize,
                                _ => panic!("array index not a number")
                            };
                            if elements.len() <= index {
                                println!("exp: {}", exp);
                                return Err(ExecutionError::IndexOutOfBounds(name, index))
                            }else {
                                return Ok(elements.remove(index))
                            }
                        },
                        Constant::String(s) => {
                            let index = match eval_expr(*index_exp, state)? {
                                Constant::Int(i) => i as usize,
                                _ => panic!("array index not a number")
                            };
                            if s.len() <= index {
                                return Err(ExecutionError::IndexOutOfBounds(name, index))
                            }else {
                                return Ok(Constant::Char(s.as_bytes()[index as usize] as char))
                            }
                        },
                        Constant::Map(hashmap) => {
                            let index = eval_expr(*index_exp, state)?;
                            match hashmap.get(index) {
                                Some(val) => return Ok(val),
                                None => panic!("Hashmap value does not exist"),
                            }
                        }
                        _ => panic!("trying to index a variable thats not an array"),                        
                    };
                },
                Object::Constant(Constant::Float(f)) => Ok(Constant::Float(f)),
                Object::Constant(Constant::Int(i)) => Ok(Constant::Int(i)),
                Object::Constant(Constant::String(s)) => Ok(Constant::String(s)),
                Object::Constant(Constant::Char(c)) => Ok(Constant::Char(c)),
                Object::Constant(Constant::Map(hashmap)) => Ok(Constant::Map(hashmap)), // wack
                Object::FuncCall(func_call) => {
                    // need a new var map for the function, just the parameters
                    if func_call.name == "len".to_string() {
                        match eval_expr(func_call.params[0].clone(), state)? {
                            Constant::Array(_, elements) => Ok(Constant::Int(elements.len() as i32)),
                            Constant::String(s) => Ok(Constant::Int(s.len() as i32)),
                            _ => panic!("panicked tried to find the length of a non array string")
                        }
                    } else if func_call.name == "parse_int".to_string() {
                        match eval_expr(func_call.params[0].clone(), state)? {
                            Constant::String(s) => {
                                // println!("prase int from `{}`", s);
                                Ok(Constant::Int(s.parse::<i32>().unwrap()))
                            },
                            Constant::Char(c) => Ok(Constant::Int(c as i32 - 48)),
                            _ => panic!("panicked tried to find the length of a non array string")
                        }
                    } else if func_call.name == "user_input".to_string() {
                        let mut line = String::new();
                        std::io::stdin().read_line(&mut line).unwrap();
                        Ok(Constant::String(line))
                    } else if func_call.name == "to_string".to_string() {
                        match eval_expr(func_call.params[0].clone(), state)? {
                            Constant::Int(i) => Ok(Constant::String(i.to_string())),
                            Constant::Char(c) => Ok(Constant::String(c.to_string())),
                            _ => panic!("panicked tried to find the length of a non array string")
                        }
                    }
                    else {
                        let mut var_map:HashMap<String, Constant> = HashMap::new();
                        let function = state.func_map.get(&func_call.name).unwrap();
                        let mut var_stack:Vec<(String, u32)> = Vec::new();
                        // let stack_lvl:u32 = 0;
                        let Function{name:_, params, return_type:_, statements:_} = function.clone();
                        let func_clone = function.clone();
                        let func_map = state.func_map.clone();
                        // iterate through the parameters provided and the function def, 
                        for (expr, (_, param_name)) in func_call.params.iter().zip(params.iter()) {
                            let param_const = eval_expr(expr.clone(), &mut state.clone())?;
                            var_stack.push((param_name.clone(), 0));
                            var_map.insert(param_name.to_string(), param_const);
                        }
                        let mut func_state = State {var_map, func_map, var_stack, stack_lvl:0}; 
                        Ok(eval_func(func_clone, &mut func_state)?)
                    }
                },
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            let left = eval_expr(*lhs, state)?;
            let right = eval_expr(*rhs, state)?;
            use Constant::{*};
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
                _ => panic!("expr operation on mismatching types")
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
                },
                OpType::Pow => l.powf(r),
                OpType::Modulus => l % r,
            };
            match var_type {
                VarType::Int => Ok(Constant::Int(res as i32)),
                VarType::Float => Ok(Constant::Float(res)),
                _ => panic!("type violation caught in execution while trying to evaluate expression")
            }
        }
    }
}
