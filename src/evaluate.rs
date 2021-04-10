use crate::ast::{*};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ExecutionError {
    ValueDne,
    DivideByZero
}

/*
* execute turns a ast object into a Result
*/
pub fn execute_ast(ast:AstNode, map:&mut HashMap<String, f64>) -> Result<(), ExecutionError> {
    match ast {
        AstNode::Print(name) => {
            match map.get(&name) {
                Some(value) => println!("\t{}:{}",name, value),
                None => return Err(ExecutionError::ValueDne)
            }
        },
        AstNode::Assignment(_, name, exp) => {
            let res = match eval_expr(exp, map) {
                Ok(value) => value,
                Err(_) => return Err(ExecutionError::ValueDne)
            };
            map.insert(name, res);
        },
        AstNode::If(bool_exp, stms) => {
            if eval_bool(bool_exp, map) {
                for stm in stms {
                    execute_ast(*stm, map);
                }
            }
        },
        AstNode::While(bool_exp, stms) => {
            let mut conditional = bool_exp.clone();
            let mut body = stms.clone();
            while eval_bool(conditional, map) {
                for stm in body {
                    execute_ast(*stm, map);
                }
                body = stms.clone();
                conditional = bool_exp.clone();
            }
        }
    };
    Ok(())
}

fn eval_bool(bool_exp:BoolExp, map:&mut HashMap<String, f64>) -> bool{
    let BoolExp(lhs,op,rhs)= bool_exp;
    let (lres, rres) = (eval_expr(lhs, map).unwrap(), eval_expr(rhs, map).unwrap());
    match op {
        BoolOp::Eq => lres == rres,
        BoolOp::Neq => lres != rres,
        BoolOp::Leq => lres <= rres,
        BoolOp::Geq => lres >= rres,
        _ => unreachable!(),
    }
}

/*
* eval_expr evaluates inline expressions
*/
fn eval_expr(exp:Expr, map:&mut HashMap<String, f64>) -> Result<f64, ExecutionError> {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Value::Variable(name) => {
                    match map.get(&name) {
                        Some(value) => Ok(*value),
                        None => return Err(ExecutionError::ValueDne)
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
