use crate::ast::{*};
use std::collections::HashMap;
use std::cmp::Ordering;
// use std::array::IntoIter;

#[derive(Debug, Clone)]
pub enum ExecutionError {
    ValueDne(String),
    DivideByZero,
    TypeViolation,
    NeedReturnStm,
    CannotFindFunction(String),
    General(String),
    AssertionError(BoolAst)
}

fn tab_print(ast:String, tab_count:u32) {
    for i in 0..tab_count {
        print!("    ");
    }
    println!("{}", ast);
}

/* run program calls the main function to run the program */
pub fn pp_run_program(state:&mut State) -> Result<Constant, ExecutionError> {
    let main_function = match state.func_map.get("main") {
        Some(func) => func,
        None => {
            println!("Error parsing main function");
            return Err(ExecutionError::CannotFindFunction("main".to_string()));
        },
    };
    let tab_count:u32 = 0;
    tab_print(format!("main()"), 0);
    Ok(eval_func(main_function.clone(), state, 0)?)
}


/* execute turns a ast object into a Result */
pub fn eval_func(function:Function, state:&mut State, tab_count:u32) -> Result<Constant, ExecutionError> {
    for ast in function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast(*ast, state, tab_count+1)? {
            Some(val) => return Ok(val),
            None => ()
        }
    }
    Err(ExecutionError::NeedReturnStm)
}

/* given the var type, and a value, tell if they match or not */
fn type_matches_val(var_type:VarType, val:Constant) -> bool {
    match (var_type,  val) {
        (VarType::Int, Constant::Int(_)) | (VarType::Float, Constant::Float(_)) | (VarType::String, Constant::String(_)) => true,
        (_, _) => false, 
    }
}

fn get_value(name:String, state:&mut State) -> Result<Constant, ExecutionError>{
    match state.var_map.get(&name.clone()) {
        Some(value) => Ok(value.clone()),
        None => Err(ExecutionError::ValueDne(name))
    }
}

fn eval_ast(ast:AstNode, state:&mut State, tab_count:u32) -> Result<Option<Constant>, ExecutionError> {
    match ast {
        AstNode::Assignment(vtype, name, exp) => {
            let variable_type:VarType = match vtype {
                Some(var_type) => var_type,
                None => {
                    // if no variable type, turn it into an expression and parse value (error if dne)
                    match eval_expr(Expr::ExpVal(Object::Variable(name.clone())), state, tab_count)? {
                        Constant::String(_) => VarType::String,
                        Constant::Float(_) => VarType::Float,
                        Constant::Int(_) => VarType::Int,
                        Constant::Array(_,_) => return Err(ExecutionError::General(String::from("Cannot set array value"))),
                        Constant::ArrayIndex(name, _) => {
                            // if it is an array, then retrieve the array from memory, then get its type
                            match get_value(name, state)? {
                                Constant::Array(var_type,_) => {
                                    var_type.clone()
                                },
                                //array access on not an array type
                                _ => return Err(ExecutionError::TypeViolation),
                            }
                        },
                    }
                }
            };
            tab_print(format!("{} = {}", name.clone(), expr_to_string(exp.clone())), tab_count);
            // type check, variable type must match the result of expression
            match (variable_type,  eval_expr(exp, state, tab_count)?) {
                (VarType::Int, Constant::Int(i)) => state.var_map.insert(name, Constant::Int(i)), 
                (VarType::Float, Constant::Float(f)) => state.var_map.insert(name, Constant::Float(f)), 
                (VarType::String, Constant::String(s)) => state.var_map.insert(name, Constant::String(s)),
                (_, _) => {
                    println!("type violation on assignment");
                    return Err(ExecutionError::TypeViolation)
                }, 
            };
        },
        AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
            tab_print(format!("{:?} {} = {:?}", var_type.clone(), name.clone(), value_exp.clone()), tab_count);
            let len = match eval_expr(length_exp, state, tab_count)? {
                Constant::Int(i) => i as usize,
                Constant::Float(f) => f as usize,
                _ => {
                    println!("type violation on ArrayDef");
                    return Err(ExecutionError::TypeViolation);
                },
            };
            // elements of the array
            let mut elements:Vec<Constant> = Vec::new();
            state.var_map.insert(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayFromExp(_, name, expr) => {
            let (var_type, elements) = match eval_expr(expr.clone(), state, tab_count)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(ExecutionError::TypeViolation),
            };
            tab_print(format!("{:?} {} = {}", var_type.clone(), name.clone(), expr_to_string(expr)), tab_count);
            state.var_map.insert(name, Constant::Array(var_type, elements));
        },
        AstNode::ArrayIndexAssignment(name, index_exp, value_exp) => {
            tab_print(format!("{}[{}] = {}", name.clone(), expr_to_string(index_exp.clone()), expr_to_string(value_exp.clone())), tab_count);
            let (var_type, elements) = match get_value(name.clone(), state)? {
                Constant::Array(var_type, elements) => (var_type, elements),
                _ => return Err(ExecutionError::TypeViolation),
            };
            state.var_map.insert(name, Constant::Array(var_type, elements));
        },
        AstNode::If(if_pairs) => {
            for (conditional, mut stms) in if_pairs {
                if eval_bool_ast(&conditional, state, tab_count)? {
                    while stms.len() > 0 {
                        match eval_ast(*stms.remove(0), state, tab_count + 1)? {
                            Some(eval) => return Ok(Some(eval)),
                            None => ()
                        }
                    }
                    break;
                }
            }
        },
        AstNode::While(conditional, stms) => {
            while eval_bool_ast(&conditional, state, tab_count)? {
                for stm in stms.clone() {
                    match eval_ast(*stm.clone(), state, tab_count + 1)? {
                        Some(eval) => return Ok(Some(eval)),
                        None => ()
                    }
                }
            }
        },
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    println!("{:?} => {:?}", exp.clone(), eval_expr(exp, state, tab_count)?);
                },
                BuiltIn::Assert(boolexp) => {
                    if ! eval_bool_ast(&boolexp.clone(), state, tab_count)? {
                        return Err(ExecutionError::AssertionError(boolexp));
                    } 
                }
            }
            ()
        },
        AstNode::ReturnStm(expr) => {
            tab_print(format!("return {:?}", expr.clone()), tab_count);
            return Ok(Some(eval_expr(expr, state, tab_count)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/* evalulates booleans based on their conjunction */
fn eval_bool_ast(bool_ast:&BoolAst, state:&mut State, tab_count:u32) ->  Result<bool, ExecutionError> {
    Ok(match &*bool_ast {
        BoolAst::Not(body) => !eval_bool_ast(&*body, state, tab_count)?,
        BoolAst::And(a, b) => eval_bool_ast(&*a, state, tab_count)? & eval_bool_ast(&*b, state, tab_count)?,
        BoolAst::Or(a,b) => eval_bool_ast(&*a, state, tab_count)? | eval_bool_ast(&*b, state, tab_count)?,
        BoolAst::Exp(exp) => eval_bool(&*exp, state, tab_count)?,
        BoolAst::Const(boolean) => *boolean,
    })
}

/* evaluates expressions and constants to true false values */
fn eval_bool(bool_exp:&BoolExp, state:&mut State, tab_count:u32) ->  Result<bool, ExecutionError> {
    let BoolExp(lhs,op,rhs)= &*bool_exp;
    use Constant::{*};
    let (lres, rres) = match (eval_expr(lhs.clone(), state, tab_count)?, eval_expr(rhs.clone(), state, tab_count)?) {
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

fn expr_to_string(exp:Expr) -> String {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // get variable as a constant value
                    name
                },
                Object::Constant(Constant::Array(var_type, elements)) => String::from("constant.array"),
                Object::Constant(Constant::ArrayIndex(name, index_exp)) => {
                    // get array from state map
                    format!("{}[{}]", name, expr_to_string(*index_exp))
                },
                Object::Constant(Constant::Float(f)) => format!("{}", f),
                Object::Constant(Constant::Int(i)) => format!("{}", i),
                Object::Constant(Constant::String(s)) => format!("{}", s),
                Object::FuncCall(func_call) => {
                    // need a new var map for the function, just the parameters
                    let mut function_str = format!("{}(", func_call.name);
                    // iterate through the parameters provided and the function def, 
                    for (expr) in func_call.params.iter() {
                        let param_const = expr_to_string(expr.clone());
                        function_str.push_str(&format!("{} ", param_const))  ; 
                    }
                    format!("{})", function_str)
                },
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            let l = expr_to_string(*lhs);
            let r = expr_to_string(*rhs);
            match op {
                OpType::Add =>  format!("{} + {}", l, r),
                OpType::Sub =>  format!("{} - {}", l, r),
                OpType::Mult => format!("{} * {}", l, r),
                OpType::Div =>  format!("{} / {}", l, r),
                OpType::Pow =>  format!("{} ^ {}", l, r),
            }
        }
    }
}

/* 
* eval_expr evaluates inline expressions 
*/
fn eval_expr(exp:Expr, state:&mut State, tab_count:u32) -> Result<Constant, ExecutionError> {
    // tab_print(expr_to_string(exp.clone()), tab_count);
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // get variable as a constant value
                    match state.var_map.get(&name) {
                        Some(value) => Ok(value.clone()),
                        None => return Err(ExecutionError::ValueDne(name))
                    }
                },
                Object::Constant(Constant::Array(var_type, elements)) => Ok(Constant::Array(var_type, elements)),
                Object::Constant(Constant::ArrayIndex(name, index_exp)) => {
                    // get array from state map
                    let mut array = match state.var_map.get(&name.clone()) {
                        Some(value) => match value.clone() {
                            Constant::Array(_var_type, elements) => elements,
                            _ => {
                                println!("array index type violation");
                                return Err(ExecutionError::TypeViolation);
                            }
                        },
                        None => return Err(ExecutionError::ValueDne(name))
                    };
                    // get the index, if its not number return error
                    let index = match eval_expr(*index_exp, state, tab_count)? {
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
                    let Function{name:n, params, return_type:rt, statements:_} = function.clone();
                    tab_print(format!("{}({:?}) -> {:?}", n.clone(), params.clone(), rt), tab_count);
                    // iterate through the parameters provided and the function def, 
                    for (expr, (var_type, param_name)) in func_call.params.iter().zip(params.iter()) {
                        let param_const = eval_expr(expr.clone(), &mut state.clone(), tab_count)?;
                        match (var_type, &param_const) {
                            (VarType::Int, Constant::Int(_)) | (VarType::Int, Constant::Array(_,_)) | (VarType::Float, Constant::Float(_)) | (VarType::String, Constant::String(_)) 
                                => var_map.insert(param_name.to_string(), param_const),
                            _ => return {
                                println!("type violation in object::funccall");
                                Err(ExecutionError::TypeViolation)
                            }
                        };   
                    }
                    let func_map = state.func_map.clone();
                    let mut func_state = State {var_map, func_map}; 
                    Ok(eval_func(function.clone(), &mut func_state, tab_count + 1)?)
                },
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            let left = eval_expr(*lhs, state, tab_count)?;
            let right = eval_expr(*rhs, state, tab_count)?;
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
