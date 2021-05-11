/*
* analysis.rs
* program analysis, an inbetween stage between parsing and evaluation
* main purpose is type checking and checking that all values referenced exist
*/


/* 
In order to do a proper type check, I am going to do two things
I will check every function individually, checking that 
    the function returns the correct type
    while executing a given function, I will assume that other function calls return the correct variables
    I will type check the calls to other functions are preformed correctly
*/
use crate::ast::{*};
use std::collections::HashMap;
use std::cmp::Ordering;
use log::{info, trace, warn};

#[derive(Debug, Clone)]
pub enum StaticError {
    ValueDne(String),
    TypeViolation,
    NeedReturnStm,
    CannotFindFunction(String),
    General(String),
}

/* run program calls the main function to run the program */
pub fn check_program(state:&mut State) -> Result<(), StaticError> {
    for (_, function) in &state.func_map {
        let returned_type = check_function(function.clone(), state)?.get_type();
        if !matches!(returned_type, function.return_type) {
            return Err(StaticError::TypeViolation);
        }

    }
    Ok(())
}


/* execute turns a ast object into a Result */
pub fn check_function(function:Function, state:&mut State) -> Result<Constant, StaticError> {
    for ast in function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast(*ast, state)? {
            Some(val) => return Ok(val),
            None => ()
        }
    }
    Err(StaticError::NeedReturnStm)
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
fn get_value(name:String, state:&mut State) -> Result<Constant, StaticError>{
    match state.var_map.get(&name.clone()) {
        Some(value) => Ok(value.clone()),
        None => Err(StaticError::ValueDne(name))
    }
}

impl Constant {
    fn get_type(self, mut state:State) -> Result<VarType, StaticError> {
        Ok(match self {
            Constant::String(_) => VarType::String,
            Constant::Float(_) => VarType::Float,
            Constant::Int(_) => VarType::Int,
            Constant::Array(_,_) => return Err(StaticError::General(String::from("Cannot get type from array"))),
            Constant::ArrayIndex(name, _) => {
                // if it is an array, then retrieve the array from memory, then get its type
                match get_value(name, &mut state)? {
                    Constant::Array(var_type,_) => {
                        var_type.clone()
                    },
                    _ => return Err(StaticError::TypeViolation),
                }
            },
        })
    }
}

/*
* evaluate an ast, one line or one if/while stm
*/
fn eval_ast(ast:AstNode, state:&mut State) -> Result<Option<Constant>, StaticError> {
    match ast {
        AstNode::Assignment(vtype, name, exp) => {
            let variable_type:VarType = match vtype {
                Some(var_type) => var_type,
                None => {
                    // if no variable type, turn it into an expression and parse value (error if dne)
                    type_of_expr(Expr::ExpVal(Object::Variable(name.clone())), state)?
                }
            };
            // type check, variable type must match the result of expression
            if matches!(variable_type.clone(), type_of_expr(exp, state)?) {
                state.var_map.insert(name, default_const(variable_type));
            } else {
                return Err(StaticError::TypeViolation);
            }
        },
        AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
            let len = match type_of_expr(length_exp, state)? {
                Constant::Int(i) => i as usize,
                Constant::Float(f) => f as usize,
                _ => {
                    trace!("type is not int or float for array index");
                    return Err(StaticError::TypeViolation);
                },
            };
            // elements of the array
            let mut elements:Vec<Constant> = Vec::new();
            let (variable, pipe) = match piped {
                Some(piped) => (piped, true),
                None => (String::from(""), false)
            };
            for i in 0..len {
                // not currently type checking need to add that later on
                if pipe {
                    state.var_map.insert(variable.clone(), Constant::Int(i as i32));
                }
                elements.push(type_of_expr(value_exp.clone(), state)?);
            }
            state.save_variable(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayFromExp(_, name, expr) => {
            let (var_type, elements) = match type_of_expr(expr, state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(StaticError::TypeViolation),
            };
            state.var_map.insert(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayIndexAssignment(name, index_exp, value_exp) => {
            let (var_type, mut elements) = match get_value(name.clone(), state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(StaticError::TypeViolation),
            };
            let index = match type_of_expr(index_exp, state)? {
                Constant::Int(i) => i as usize,
                _ => return Err(StaticError::TypeViolation),
            };
            let value = type_of_expr(value_exp, state)?;
            if ! type_matches_val(var_type.clone(), value.clone()) {
                return Err(StaticError::TypeViolation);
            };
            elements[index] = value;
            state.var_map.insert(name, Constant::Array(var_type, elements));
        },
        AstNode::If(if_pairs) => {
            state.increment_stack_level();
            for (conditional, mut stms) in if_pairs {
                if check_bool_ast(&conditional, state)? {
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
            state.increment_stack_level();
            while check_bool_ast(&conditional, state)? {
                for stm in stms.iter() {
                    match eval_ast(*stm.clone(), state)? {
                        Some(eval) => return Ok(Some(eval)),
                        None => ()
                    }
                }
            }
            state.pop_stack();
        },
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    println!("{:?} => {:?}", exp.clone(), type_of_expr(exp, state)?);
                },
                BuiltIn::Assert(boolexp) => {
                    if ! check_bool_ast(&boolexp.clone(), state)? {
                        return Err(StaticError::AssertionError(boolexp));
                    } 
                }
            }
            ()
        },
        AstNode::ReturnStm(expr) => {
            return Ok(Some(type_of_expr(expr, state)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/* 
* evalulates booleans based on their conjunction 
*/
fn check_bool_ast(bool_ast:&BoolAst, state:&mut State) ->  Result<bool, StaticError> {
    Ok(match &*bool_ast {
        BoolAst::Not(body) => check_bool_ast(&*body, state)?,
        BoolAst::And(a, b) => {check_bool_ast(&*a, state)?; check_bool_ast(&*b, state)?;},
        BoolAst::Or(a,b) => {check_bool_ast(&*a, state)?; check_bool_ast(&*b, state)?},
        BoolAst::Exp(exp) => check_bool(&*exp, state)?,
        BoolAst::Const(boolean) => (),
    })
}

/* 
* evaluates expressions and constants to true false values 
*/
fn check_bool(bool_exp:&BoolExp, state:&mut State) ->  Result<(), StaticError> {
    let BoolExp(lhs,_,rhs)= &*bool_exp;
    // if 
    if !matches!(type_of_expr(lhs.clone(), state)?, type_of_expr(rhs.clone(), state)?) {
        return Err(StaticError::TypeViolation);
    };
    Ok(())
}


fn default_const(var_type:VarType) {
    match var_type {
        VarType::Int => Constant::Int(0),
        VarType::Float => Constant::Float(0),
        VarType::String => Constant::String(String::from(""))
    }
}


/* 
* type_of_expr returns the type provided by an inline expression
*/
fn type_of_expr(exp:Expr, state:&mut State) -> Result<VarType, StaticError> {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // get variable as a constant value
                    match state.var_map.get(&name) {
                        Some(value) => Ok(value.get_type(state)?),
                        None => return Err(StaticError::ValueDne(name))
                    }
                },
                Object::Constant(Constant::Array(var_type, elements)) => Ok(var_type),
                Object::Constant(Constant::ArrayIndex(name, index_exp)) => {
                    match type_of_expr(*index_exp, state)? {
                        Constant::Int(i) => (),
                        _ => return Err(StaticError::General(String::from("array index must be a number")))
                    };
                    // get array from state map
                    match state.var_map.get(&name.clone()) {
                        Some(value) => match value.clone() {
                            Constant::Array(var_type, _) => {
                                Ok(var_type)
                            },
                            _ => {
                                warn!("array index type violation");
                                Err(StaticError::TypeViolation);
                            }
                        },
                        None => Err(StaticError::ValueDne(name))
                    }
                },
                Object::Constant(Constant::Float(f)) => Ok(VarType::Float),
                Object::Constant(Constant::Int(i)) => Ok(VarType::Int),
                Object::Constant(Constant::String(s)) => Ok(VarType::String),
                Object::FuncCall(func_call) => {
                    // retrive function from memory, make sure its value matches
                    let function = match state.func_map.get(&func_call.name.clone()) {
                        Ok(func) => func,
                        Err(_) => StaticError::CannotFindFunction(func_call.name)
                    };
                    let Function{name:_, params, return_type:return_type, statements:_} = function.clone();
                    // iterate through the parameters provided and the function def, 
                    for (expr, (var_type, param_name)) in func_call.params.iter().zip(params.iter()) {
                        let param_const = type_of_expr(expr.clone(), &mut state.clone())?;
                        match (var_type, &param_const) {
                            (VarType::Int, Constant::Int(_)) | (VarType::Int, Constant::Array(_,_)) | (VarType::Float, Constant::Float(_)) | (VarType::String, Constant::String(_)) 
                                => (),
                            _ => return {
                                warn!("type violation in object::funccall");
                                Err(StaticError::TypeViolation)
                            }
                        };   
                    }
                    // function input types match expected values, return a empty constant of matching type
                    Ok(return_type)
                },
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            let left = type_of_expr(*lhs, state)?;
            let right = type_of_expr(*rhs, state)?;
            use Constant::{*};
            if matches!(left.clone(), right) {
                Ok(left.get_type(state)?)
            } else {
                Err(StaticError::TypeViolation)
            }
        }
    }
}
