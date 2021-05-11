use crate::ast::{*};
use std::collections::HashMap;
use std::cmp::Ordering;
use log::{info, trace, warn};

#[derive(Debug, Clone)]
pub enum ExecutionError {
    ValueDne(String),
    DivideByZero,
    TypeViolation,
    NeedReturnStm,
    CannotFindFunction(String),
    General(String),
    AssertionError(BoolAst),
}

/* run program calls the main function to run the program */
pub fn run_program(state:&mut State) -> Result<Constant, ExecutionError> {
    let main_function = match state.func_map.get("main") {
        Some(func) => func,
        None => {
            warn!("Error parsing main function");
            return Err(ExecutionError::CannotFindFunction("main".to_string()));
        },
    };
    Ok(eval_func(main_function.clone(), state)?)
}

// Checks equality amon the constants
impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        use Constant::*;
        match (self, other) {
            (Int(i), Int(j)) => i == j,
            (Float(i), Float(j)) => i == j,
            (String(i), String(j)) => i.eq(j),
            _ => false
        }
    }
}

// Checks equality amon the constants
impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Constant::*;
        Some(match (self, other) {
            (Int(i), Int(j)) => i.cmp(j),
            (Float(i), Float(j)) => (*i as i64).cmp(&(*j as i64)),
            (String(i), String(j)) => i.cmp(j),
            _ => return None,
        })
    }
}


/* execute turns a ast object into a Result */
pub fn eval_func(function:Function, state:&mut State) -> Result<Constant, ExecutionError> {
    state.increment_stack_level();
    for ast in function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast(*ast, state)? {
            Some(val) => return Ok(val),
            None => ()
        }
    }
    state.pop_stack();
    Err(ExecutionError::NeedReturnStm)
}

/* given the var type, and a value, tell if they match or not */
fn type_matches_val(var_type:VarType, val:Constant) -> bool {
    match (var_type,  val) {
        (VarType::Int, Constant::Int(_)) | (VarType::Float, Constant::Float(_)) | (VarType::String, Constant::String(_)) => true,
        (_, _) => false, 
    }
}

/* 
* returns a value or that a value does not exist 
*/
fn get_value(name:String, state:&mut State) -> Result<Constant, ExecutionError>{
    match state.var_map.get(&name.clone()) {
        Some(value) => Ok(value.clone()),
        None => Err(ExecutionError::ValueDne(name))
    }
}

/*
* evaluate an ast, one line or one if/while stm
*/
fn eval_ast(ast:AstNode, state:&mut State) -> Result<Option<Constant>, ExecutionError> {
    match ast.clone() {
        AstNode::Assignment(vtype, name, exp) => {
            let variable_type:VarType = match vtype {
                Some(var_type) => var_type,
                None => {
                    // if no variable type, turn it into an expression and parse value (error if dne)
                    match eval_expr(Expr::ExpVal(Object::Variable(name.clone())), state)? {
                        Constant::String(_) => VarType::String,
                        Constant::Float(_) => VarType::Float,
                        Constant::Int(_) => VarType::Int,
                        Constant::Array(_,_) => return Err(ExecutionError::General(String::from("Cannot get type from array"))),
                        Constant::ArrayIndex(name, _) => {
                            // if it is an array, then retrieve the array from memory, then get its type
                            match get_value(name, state)? {
                                Constant::Array(var_type,_) => {
                                    var_type.clone()
                                },
                                _ => return Err(ExecutionError::TypeViolation),
                            }
                        },
                    }
                }
            };
            // type check, variable type must match the result of expression
            match (variable_type,  eval_expr(exp, state)?) {
                (VarType::Int, Constant::Int(i)) => state.save_variable(name, Constant::Int(i)), 
                (VarType::Float, Constant::Float(f)) => state.save_variable(name, Constant::Float(f)), 
                (VarType::String, Constant::String(s)) => state.save_variable(name, Constant::String(s)),
                (_, _) => {
                    trace!("type violation on assignment");
                    return Err(ExecutionError::TypeViolation)
                }, 
            };
        },
        AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
            let len = match eval_expr(length_exp, state)? {
                Constant::Int(i) => i as usize,
                Constant::Float(f) => f as usize,
                _ => {
                    trace!("type is not int or float for array index");
                    return Err(ExecutionError::TypeViolation);
                },
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
            let (var_type, elements) = match eval_expr(expr, state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(ExecutionError::TypeViolation),
            };
            state.save_variable(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayIndexAssignment(name, index_exp, value_exp) => {
            let (var_type, mut elements) = match get_value(name.clone(), state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(ExecutionError::TypeViolation),
            };
            let index = match eval_expr(index_exp, state)? {
                Constant::Int(i) => i as usize,
                _ => return Err(ExecutionError::TypeViolation),
            };
            let value = eval_expr(value_exp, state)?;
            if ! type_matches_val(var_type.clone(), value.clone()) {
                return Err(ExecutionError::TypeViolation);
            };
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
                    println!("{:?} => {:?}", exp.clone(), eval_expr(exp, state)?);
                },
                BuiltIn::Assert(boolexp) => {
                    if ! eval_bool_ast(&boolexp.clone(), state)? {
                        return Err(ExecutionError::AssertionError(boolexp));
                    } 
                }
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
        BoolAst::And(a, b) => eval_bool_ast(&*a, state)? & eval_bool_ast(&*b, state)?,
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
        _ => return Err(ExecutionError::TypeViolation),
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
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // get variable as a constant value
                    match state.var_map.get(&name) {
                        Some(value) => Ok(value.clone()),
                        None => {
                            println!("error doesn;t exist while evaluating expr");
                            return Err(ExecutionError::ValueDne(name));
                        }
                    }
                },
                Object::Constant(Constant::Array(var_type, elements)) => Ok(Constant::Array(var_type, elements)),
                Object::Constant(Constant::ArrayIndex(name, index_exp)) => {
                    // get array from state map
                    let mut array = match state.var_map.get(&name.clone()) {
                        Some(value) => match value.clone() {
                            Constant::Array(_var_type, elements) => elements,
                            _ => {
                                warn!("array index type violation");
                                return Err(ExecutionError::TypeViolation);
                            }
                        },
                        None => return Err(ExecutionError::ValueDne(name))
                    };
                    // get the index, if its not number return error
                    let index = match eval_expr(*index_exp, state)? {
                        Constant::Int(i) => i as usize,
                        Constant::Float(f) => f as usize,
                        _ => return Err(ExecutionError::General(String::from("array index must be a number")))
                    };
                    return Ok(array.remove(index));
                },
                Object::Constant(Constant::Float(f)) => Ok(Constant::Float(f)),
                Object::Constant(Constant::Int(i)) => Ok(Constant::Int(i)),
                Object::Constant(Constant::String(s)) => Ok(Constant::String(s)),
                Object::FuncCall(func_call) => {
                    // need a new var map for the function, just the parameters
                    let mut var_map:HashMap<String, Constant> = HashMap::new();
                    let function = state.func_map.get(&func_call.name).unwrap();
                    let mut var_stack:Vec<(String, u32)> = Vec::new();
                    // let stack_lvl:u32 = 0;
                    let Function{name:_, params, return_type:_, statements:_} = function.clone();
                    let func_clone = function.clone();
                    let func_map = state.func_map.clone();
                    // iterate through the parameters provided and the function def, 
                    for (expr, (var_type, param_name)) in func_call.params.iter().zip(params.iter()) {
                        let param_const = eval_expr(expr.clone(), &mut state.clone())?;
                        var_stack.push((param_name.clone(), 0));
                        match (var_type, &param_const) {
                            (VarType::Int, Constant::Int(_)) | (VarType::Int, Constant::Array(_,_)) | (VarType::Float, Constant::Float(_)) | (VarType::String, Constant::String(_)) 
                                => var_map.insert(param_name.to_string(), param_const),
                            _ => return {
                                warn!("type violation in object::funccall");
                                Err(ExecutionError::TypeViolation)
                            }
                        };   
                    }
                    let mut func_state = State {var_map, func_map, var_stack, stack_lvl:0}; 
                    Ok(eval_func(func_clone, &mut func_state)?)
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
                    _ => return Err(ExecutionError::TypeViolation),//Err(ExecutionError:::General("string operation not allowed".to_string()))
                }
                _ => return Err(ExecutionError::TypeViolation) // do not accept strings or operations between ints and floats, for now
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
            };
            match var_type {
                VarType::Int => Ok(Constant::Int(res as i32)),
                VarType::Float => Ok(Constant::Float(res)),
                _ => Err(ExecutionError::TypeViolation)
            }
        }
    }
}
