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
use log::{warn};

#[derive(Debug, Clone)]
pub enum StaticError {
    ValueDne(String),
    TypeViolation,
    NeedReturnStm,
    CannotFindFunction(String),
}

/* run program calls the main function to run the program */
pub fn check_program(state:&mut State) -> Result<(), StaticError> {
    for (_, function) in &state.clone().func_map {
        let returned_type = check_function(function.clone(), state)?;
        let expected = &function.return_type;
        if !type_match(returned_type, expected.clone()) {
            return Err(StaticError::TypeViolation);
        }

    }
    Ok(())
}


/* execute turns a ast object into a Result */
pub fn check_function(function:Function, state:&mut State) -> Result<VarType, StaticError> {
    state.increment_stack_level();
    // save parameters of the function into state
    for (param_type, param_name) in function.params {
        state.save_variable(param_name, default_const(param_type));
    }
    for ast in function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast(*ast, state)? {
            Some(val) => return Ok(val),
            None => ()
        }
    }
    state.pop_stack();
    Err(StaticError::NeedReturnStm)
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
    fn get_type(self, state:&mut State) -> Result<VarType, StaticError> {
        Ok(match self {
            Constant::String(_) => VarType::String,
            Constant::Float(_) => VarType::Float,
            Constant::Int(_) => VarType::Int,
            Constant::Array(vtype,_) => vtype,
            Constant::ArrayIndex(name, _) => {
                // if it is an array, then retrieve the array from memory, then get its type
                match get_value(name, state)? {
                    Constant::Array(var_type,_) => {
                        var_type.clone()
                    },
                    _ => return Err(StaticError::TypeViolation),
                }
            },
        })
    }
}

fn type_match(a:VarType, b:VarType) -> bool {
    use VarType::{*};
    match (a, b) {
        (Int, Int) | (Float, Float) | (String, String) => true,
        _ => false
    }
}

/*
* evaluate an ast, one line or one if/while stm
*/
fn eval_ast(ast:AstNode, state:&mut State) -> Result<Option<VarType>, StaticError> {
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
            let value_type = type_of_expr(exp, state)?;
            if type_match(variable_type.clone(), value_type) {
                state.save_variable(name, default_const(variable_type));
            } else {
                warn!("type violation while assigning variable");
                return Err(StaticError::TypeViolation);
            }
        },
        AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
            match type_of_expr(length_exp, state)? {
                VarType::Int => (),
                _ => {
                    warn!("length of array must be int");
                    return Err(StaticError::TypeViolation);
                },
            };
            // elements of the array
            let (variable, pipe) = match piped {
                Some(piped) => (piped, true),
                None => (String::from(""), false)
            };
            state.increment_stack_level();
            for i in 0..2 {
                // not currently type checking need to add that later on
                if pipe {
                    state.save_variable(variable.clone(), Constant::Int(i as i32));
                }
                type_of_expr(value_exp.clone(), state)?;
            }
            state.pop_stack();
            state.save_variable(name, Constant::Array(var_type, Vec::new()));
        },
        AstNode::ArrayFromExp(_, name, expr) => {
            let var_type = type_of_expr(expr, state)?;
            state.save_variable(name, Constant::Array(var_type, Vec::new()));
        },
        AstNode::ArrayIndexAssignment(name, index_exp, value_exp) => {
            let (var_type, _) = match get_value(name.clone(), state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(StaticError::TypeViolation),
            };
            match type_of_expr(index_exp, state)? {
                VarType::Int => (),
                _ => return Err(StaticError::TypeViolation),
            };
            let value_type = type_of_expr(value_exp, state)?;
            if ! type_match(var_type, value_type) {
                return Err(StaticError::TypeViolation);
            };
        },
        AstNode::If(if_pairs) => {
            state.increment_stack_level();
            for (conditional, mut stms) in if_pairs {
                check_bool_ast(&conditional, state)?;
                while stms.len() > 0 {
                    match eval_ast(*stms.remove(0), state)? {
                        Some(eval) => return Ok(Some(eval)), // TODO change this to type check every statement
                        None => ()
                    }
                }
            }
            state.pop_stack();
        },
        AstNode::While(conditional, stms) => {
            check_bool_ast(&conditional, state)?;
            state.increment_stack_level();
            for stm in stms.iter() {
                match eval_ast(*stm.clone(), state)? {
                    Some(eval) => return Ok(Some(eval)), //if there was a return statement, return the value
                    None => ()
                }
            }
            state.pop_stack();
            
        },
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    type_of_expr(exp, state)?;
                },
                BuiltIn::Assert(boolast) => {
                    check_bool_ast(&boolast, state)?;
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
fn check_bool_ast(bool_ast:&BoolAst, state:&mut State) ->  Result<(), StaticError> {
    match &*bool_ast {
        BoolAst::Not(body) => check_bool_ast(&*body, state)?,
        BoolAst::And(a, b) => {check_bool_ast(&*a, state)?; check_bool_ast(&*b, state)?;},
        BoolAst::Or(a,b) => {check_bool_ast(&*a, state)?; check_bool_ast(&*b, state)?},
        BoolAst::Exp(exp) => check_bool(&*exp, state)?,
        BoolAst::Const(_) => (),
    };
    Ok(())
}

/* 
* evaluates expressions and constants to true false values 
*/
fn check_bool(bool_exp:&BoolExp, state:&mut State) ->  Result<(), StaticError> {
    let BoolExp(lhs,_,rhs)= &*bool_exp;
    // if 
    let right = type_of_expr(rhs.clone(), state)?;
    if ! type_match(type_of_expr(lhs.clone(), state)?, right) {
        return Err(StaticError::TypeViolation);
    };
    Ok(())
}


fn default_const(var_type:VarType) -> Constant{
    match var_type {
        VarType::Int => Constant::Int(0),
        VarType::Float => Constant::Float(0.0),
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
                        Some(value) => Ok(value.clone().get_type(state)?),
                        None => return Err(StaticError::ValueDne(name))
                    }
                },
                Object::Constant(Constant::Array(var_type, _elements)) => Ok(var_type),
                Object::Constant(Constant::ArrayIndex(name, index_exp)) => {
                    match type_of_expr(*index_exp, state)? {
                        VarType::Int => (),
                        _ => return Err(StaticError::TypeViolation)
                    };
                    // get array from state map
                    match state.var_map.get(&name.clone()) {
                        Some(value) => match value.clone() {
                            Constant::Array(var_type, _) => {
                                Ok(var_type)
                            },
                            _ => {
                                warn!("array index type violation");
                                Err(StaticError::TypeViolation)
                            }
                        },
                        None => Err(StaticError::ValueDne(name))
                    }
                },
                Object::Constant(Constant::Float(_)) => Ok(VarType::Float),
                Object::Constant(Constant::Int(_)) => Ok(VarType::Int),
                Object::Constant(Constant::String(_)) => Ok(VarType::String),
                Object::FuncCall(func_call) => {
                    // retrive function from memory, make sure its value matches
                    let function = match state.func_map.get(&func_call.name.clone()) {
                        Some(func) => func,
                        None => return Err(StaticError::CannotFindFunction(func_call.name)),
                    };
                    let Function{name:_, params, return_type, statements:_} = function.clone();
                    // iterate through the parameters provided and the function def, 
                    for (expr, (var_type, _)) in func_call.params.iter().zip(params.iter()) {
                        let param_const = type_of_expr(expr.clone(), &mut state.clone())?;
                        if ! type_match(var_type.clone(), param_const.clone()) {
                            return {
                                warn!("type violation in object::funccall\n\texpected: {:?} recieved {:?}", var_type, param_const);
                                Err(StaticError::TypeViolation)
                            }
                        }
                    }
                    // function input types match expected values, return a empty constant of matching type
                    Ok(return_type)
                },
            }
        },
        Expr::ExpOp(lhs, _, rhs) => {
            let left = type_of_expr(*lhs, state)?;
            let right = type_of_expr(*rhs, state)?;
            if type_match(left.clone(), right) {
                Ok(left)
            } else {
                Err(StaticError::TypeViolation)
            }
        }
    }
}
