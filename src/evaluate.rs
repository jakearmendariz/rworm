use crate::ast::{*};
use std::collections::HashMap;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
pub enum ExecutionError {
    ValueDne,
    DivideByZero,
    TypeViolation,
    NeedReturnStm,
    CannotFindFunction(String)
}


impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        use Constant::*;
        match (self, other) {
            (Int(i), Int(j)) => i == j,
            (Float(i), Float(j)) => i == j,
            (String(i), String(j)) => i.eq(j),
            _ => false//return ExecutionError::TypeViolation,
        }
    }
}

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
    for ast in function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast(*ast, state)? {
            Some(val) => return Ok(val),
            None => ()
        }
    }
    Err(ExecutionError::NeedReturnStm)
}

fn eval_ast(ast:AstNode, state:&mut State) -> Result<Option<Constant>, ExecutionError> {
    println!("{:?}", ast);
    match ast {
        AstNode::Print(name) => {
            match state.var_map.get(&name) {
                Some(value) => println!("\t{}:{:?}",name, value),
                None => println!("\t{}:{}",name, 0)
            }
        },
        AstNode::Assignment(vtype, name, exp) => {
            let variable_type:VarType = match vtype {
                Some(var_type) => {
                    match var_type {
                        VarType::String => {
                            state.var_map.insert(name.clone(), Constant::String("".to_string()));
                            VarType::String
                        }
                        VarType::Int => {
                            state.var_map.insert(name.clone(), Constant::Int(0));
                            VarType::Int
                        }
                        VarType::Float => {
                            state.var_map.insert(name.clone(), Constant::Float(0.0));
                            VarType::Float
                        }
                    }
                },
                None => {
                    // if no variable type, turn it into an expression and parse value (error if dne)
                    match eval_expr(Expr::ExpVal(Object::Variable(name.clone())), state)? {
                        Constant::String(_) => VarType::String,
                        Constant::Float(_) => VarType::Float,
                        Constant::Int(_) => VarType::Int,
                    }
                }
            };
            // type check, lhs and rhs must return the same type
            match (variable_type,  eval_expr(exp, state)?) {
                (VarType::Int, Constant::Int(i)) => state.var_map.insert(name, Constant::Int(i)), 
                (VarType::Float, Constant::Float(f)) => state.var_map.insert(name, Constant::Float(f)), 
                (VarType::String, Constant::String(s)) => state.var_map.insert(name, Constant::String(s)),
                (_, _) => {
                    println!("type violation on assignment");
                    return Err(ExecutionError::TypeViolation)
                }, 
            };
        },
        AstNode::If(conditional, stms) => {
            if eval_bool_ast(conditional, state)? {
                eval_ast(*stms[0].clone(), state)?;
            } else {
                eval_ast(*stms[1].clone(), state)?;
            }
        },
        AstNode::While(conditional, stms) => {
            while eval_bool_ast(conditional.clone(), state)? {
                for stm in &stms {
                    eval_ast(*stm.clone(), state)?;
                }
            }
        },
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Delete(name) => {
                    state.var_map.remove(&name);
                    ()
                },
                BuiltIn::Sum() => {
                    ()
                }
            }
            ()
        },
        AstNode::FuncDef(function) => {
            state.func_map.insert(function.name.clone(), function);
        },
        AstNode::ReturnStm(expr) => {
            return Ok(Some(eval_expr(expr, state)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/* evalulates booleans based on their conjunction */
fn eval_bool_ast(bool_ast:BoolAst, state:&mut State) ->  Result<bool, ExecutionError> {
    Ok(match bool_ast {
        BoolAst::Not(body) => !eval_bool_ast(*body, state)?,
        BoolAst::And(a, b) => eval_bool_ast(*a, state)? & eval_bool_ast(*b, state)?,
        BoolAst::Or(a,b) => eval_bool_ast(*a, state)? | eval_bool_ast(*b, state)?,
        BoolAst::Exp(exp) => eval_bool(exp, state)?,
        BoolAst::Const(boolean) => boolean,
    })
}

/* evaluates expressions and constants to true false values */
fn eval_bool(bool_exp:BoolExp, state:&mut State) ->  Result<bool, ExecutionError> {
    let BoolExp(lhs,op,rhs)= bool_exp;
    use Constant::{*};
    let (lres, rres) = match (eval_expr(lhs, state)?, eval_expr(rhs, state)?) {
        (Int(i), Int(j)) => (i as f64,j as f64),
        (Float(i), Float(j)) => (i, j),
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

/* eval_expr evaluates inline expressions */
fn eval_expr(exp:Expr, state:&mut State) -> Result<Constant, ExecutionError> {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Object::Variable(name) => {
                    // 
                    let value = match state.var_map.get(&name) {
                        Some(value) => Ok(value.clone()),
                        None => return Err(ExecutionError::ValueDne)
                    };
                    match value? {
                        Constant::Int(i) => Ok(Constant::Int(i)),
                        Constant::Float(f) => Ok(Constant::Float(f)),
                        Constant::String(s) => Ok(Constant::String(s)),
                    }
                },
                Object::Constant(Constant::Float(f)) => Ok(Constant::Float(f)),
                Object::Constant(Constant::Int(i)) => Ok(Constant::Int(i)),
                Object::Constant(Constant::String(s)) => Ok(Constant::String(s)),
                Object::FuncCall(func_call) => {
                    let mut var_map:HashMap<String, Constant> = HashMap::new();
                    let mut function = state.func_map.get(&func_call.name).unwrap();
                    let Function{name, params, return_type, statements} = function.clone();
                    for (expr, (var_type, name)) in func_call.params.iter().zip(params.iter()) {
                        match var_type {
                            VarType::Int => var_map.insert(name.to_string(), eval_expr(expr.clone(), &mut state.clone())?),
                            VarType::Float => var_map.insert(name.to_string(), eval_expr(expr.clone(), &mut state.clone())?),
                            VarType::String => var_map.insert(name.to_string(), eval_expr(expr.clone(), &mut state.clone())?), 
                        };
                    }
                    let func_map = state.func_map.clone();
                    let mut func_state = State {var_map, func_map}; 
                    Ok(eval_func(function.clone(), &mut func_state)?)
                },
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            let left = match eval_expr(*lhs, state) {
                Ok(value) => value,
                Err(_) => return Err(ExecutionError::ValueDne)
            };
            let right = match eval_expr(*rhs, state) {
                Ok(value) => value,
                Err(_) => return Err(ExecutionError::ValueDne)
            };
            use Constant::{*};
            let (l, r, is_int) = match (left, right) {
                (Int(l), Int(r)) => (l as f64, r as f64, true),
                (Float(l), Float(r)) => (l, r, false),
                _ => return(Err(ExecutionError::TypeViolation)) // do not accept strings or operations between ints and floats, for now
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
            if is_int {
                Ok(Constant::Int(res as i32))
            } else{
                Ok(Constant::Float(res))
            }
        }
        _ => unreachable!(),
    }
}
