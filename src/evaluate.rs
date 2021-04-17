use crate::ast::{*};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ExecutionError {
    ValueDne,
    DivideByZero
}

/* execute turns a ast object into a Result */
pub fn execute_ast(ast:AstNode, map:&mut HashMap<String, f64>) -> Result<(), ExecutionError> {
    match ast {
        AstNode::Print(name) => {
            match map.get(&name) {
                Some(value) => println!("\t{}:{}",name, value),
                None => println!("\t{}:{}",name, 0)
            }
        },
        AstNode::Assignment(_, name, exp) => {
            let res = eval_expr(exp, map)?;
            map.insert(name, res);
        },
        AstNode::If(conditional, stms) => {
            if eval_bool_ast(conditional, map)? {
                execute_ast(*stms[0].clone(), map)?;
            } else {
                execute_ast(*stms[1].clone(), map)?;
            }
        },
        AstNode::While(conditional, stms) => {
            while eval_bool_ast(conditional.clone(), map)? {
                for stm in &stms {
                    execute_ast(*stm.clone(), map)?;
                }
            }
        },
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Delete(name) => {
                    map.remove(&name);
                },
                BuiltIn::Sum() => {
                    let mut sum = 0.0;
                    for (_, val) in map.iter() {
                        sum += val;
                    }
                    map.insert("sum".to_string(), sum);
                }
            }
            
        },
        AstNode::Skip() => (),
    };
    Ok(())
}

/* evalulates booleans based on their conjunction */
fn eval_bool_ast(bool_ast:BoolAst, map:&mut HashMap<String, f64>) ->  Result<bool, ExecutionError> {
    Ok(match bool_ast {
        BoolAst::Not(body) => !eval_bool_ast(*body, map)?,
        BoolAst::And(a, b) => eval_bool_ast(*a, map)? & eval_bool_ast(*b, map)?,
        BoolAst::Or(a,b) => eval_bool_ast(*a, map)? | eval_bool_ast(*b, map)?,
        BoolAst::Exp(exp) => eval_bool(exp, map)?,
        BoolAst::Const(boolean) => boolean,
    })
}

/* evaluates expressions and constants to true false values */
fn eval_bool(bool_exp:BoolExp, map:&mut HashMap<String, f64>) ->  Result<bool, ExecutionError> {
    let BoolExp(lhs,op,rhs)= bool_exp;
    let (lres, rres) = (eval_expr(lhs, map)?, eval_expr(rhs, map)?);
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
fn eval_expr(exp:Expr, map:&mut HashMap<String, f64>) -> Result<f64, ExecutionError> {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Value::Variable(name) => {
                    match map.get(&name) {
                        Some(value) => Ok(*value),
                        None => return Ok(0.0)
                    }
                },
                Value::Number(number) => Ok(number)
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            let left = match eval_expr(*lhs, map) {
                Ok(value) => value,
                Err(_) => return Err(ExecutionError::ValueDne)
            };
            let right = match eval_expr(*rhs, map) {
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
