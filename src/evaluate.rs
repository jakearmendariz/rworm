use crate::ast::{*};
use std::collections::HashMap;
use std::cmp::Ordering;

#[derive(Debug, Clone, Default)]
pub struct State {
    var_map:HashMap<String, Variable>,
    func_map:HashMap<String, Function>,
}

#[derive(Debug)]
pub enum ExecutionError {
    ValueDne,
    DivideByZero,
    TypeViolation
}

#[derive(Debug, Clone)]
pub enum Variable {
    Int(i32),
    Float(f64),
    String(String)
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        use Variable::*;
        match (self, other) {
            (Int(i), Int(j)) => i == j,
            (Float(i), Float(j)) => i == j,
            (String(i), String(j)) => i.eq(j),
            _ => false//return ExecutionError::TypeViolation,
        }
    }
}

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Variable::*;
        Some(match (self, other) {
            (Int(i), Int(j)) => i.cmp(j),
            (Float(i), Float(j)) => (*i as i64).cmp(&(*j as i64)),
            (String(i), String(j)) => i.cmp(j),
            _ => return None,
        })
    }
}


/* execute turns a ast object into a Result */
pub fn execute_ast(ast:AstNode, state:&mut State) -> Result<(), ExecutionError> {
    match ast {
        AstNode::Print(name) => {
            match state.var_map.get(&name) {
                Some(value) => println!("\t{}:{:?}",name, value),
                None => println!("\t{}:{}",name, 0)
            }
        },
        AstNode::Assignment(_, name, exp) => {
            let res = eval_expr(exp, state)?;
            state.var_map.insert(name, Variable::Float(res));
        },
        AstNode::If(conditional, stms) => {
            if eval_bool_ast(conditional, state)? {
                execute_ast(*stms[0].clone(), state)?;
            } else {
                execute_ast(*stms[1].clone(), state)?;
            }
        },
        AstNode::While(conditional, stms) => {
            while eval_bool_ast(conditional.clone(), state)? {
                for stm in &stms {
                    execute_ast(*stm.clone(), state)?;
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
        AstNode::Skip() => (),
    };
    Ok(())
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
    let (lres, rres) = (eval_expr(lhs, state)?, eval_expr(rhs, state)?);
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
fn eval_expr(exp:Expr, state:&mut State) -> Result<f64, ExecutionError> {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Value::Variable(name) => {
                    let value = match state.var_map.get(&name) {
                        Some(value) => Ok(value.clone()),
                        None => return Err(ExecutionError::ValueDne)
                    };
                    use Variable::*;
                    Ok(match value? {
                        Int(i) => i as f64,
                        Float(f) => f,
                        String(_) => return Err(ExecutionError::TypeViolation),
                    })
                },
                Value::Number(number) => Ok(number),
                Value::FuncCall(func_call) => {
                    let mut var_map:HashMap<String, Variable> = HashMap::new();
                    let mut function = state.func_map.get(&func_call.name).unwrap();
                    let Function{name, params, return_type, statements, return_stm} = function;
                    for (expr, (var_type, name)) in func_call.params.iter().zip(params.iter()) {
                        var_map.insert(name.to_string(), Variable::Float(eval_expr(expr.clone(), &mut state.clone())?));
                    }
                    let func_map = state.func_map.clone();
                    let mut func_state = State {var_map, func_map};
                    for stm in statements {
                        let result = execute_ast(*stm.clone(), &mut func_state);
                        match result {
                            Ok(_) => (),
                            Err(e) => println!("error:{:?}", e)
                        }
                    }   
                    
                    Ok(eval_expr(return_stm.clone(), &mut func_state)?)
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
            let res = match op {
                OpType::Add => left + right,
                OpType::Sub => left - right,
                OpType::Mult => left * right,
                OpType::Div => {
                    if right == 0.0 {
                        return Err(ExecutionError::DivideByZero);
                    }
                    left / right
                },
                OpType::Pow => left.powf(right),
            };
            Ok(res)
        }
    }
}
