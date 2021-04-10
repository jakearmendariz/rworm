use crate::ast::{*};
use crate::parser::{*};
use std::collections::HashMap;


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