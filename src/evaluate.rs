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
                Some(value) => println!("\t{}", value),
                None => return Err(ExecutionError::ValueDne)
            }
        },
        AstNode::Assignment(_, name, exp) => {
            map.insert(name, eval_expr(exp));
        },
    };
    Ok(())
}

/*
* eval_expr evaluates inline expressions
*/
fn eval_expr(exp:Expr) -> f64 {
    match exp {
        Expr::ExpVal(num) => {
            match num {
                Value::Variable(_) => 0.0,
                Value::Number(number) => number
            }
        },
        Expr::ExpOp(lhs, op, rhs) => {
            match op {
                OpType::Add => eval_expr(*lhs) + eval_expr(*rhs),
                OpType::Sub => eval_expr(*lhs) - eval_expr(*rhs),
                OpType::Mult => eval_expr(*lhs) * eval_expr(*rhs),
                OpType::Div => eval_expr(*lhs) / eval_expr(*rhs),
                OpType::Pow => eval_expr(*lhs).powf(eval_expr(*rhs)),
            }
        }
    }
}
