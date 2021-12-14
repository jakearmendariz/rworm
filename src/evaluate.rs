/*
* evaluates the ast, panics on missed type static errors, but catches execution errors and returns result
*/
use crate::ast::*;
use crate::state::{ExecutionState, State};
use colored::*;
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone)]
pub enum ExecutionError {
    DivideByZero,
    AssertionError(Expr),
}

static MAIN: &str = "main";

/* run program calls the main function to run the program */
pub fn run_program(
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Constant, ExecutionError> {
    Ok(eval_func(MAIN.to_string(), execution_state, state)?)
}

/* execute turns a ast object into a Result */
pub fn eval_func(
    name: String,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Constant, ExecutionError> {
    let function = state.func_map.get(&name).unwrap();
    execution_state.increment_stack_level();
    for ast in &function.statements {
        // execute ast will run a single statement or loop, if there is a return value, exit out of function
        match eval_ast((**ast).clone(), execution_state, state)? {
            Some(val) => {
                execution_state.pop_stack();
                return Ok(val);
            }
            None => (),
        }
    }
    panic!("no return statement from function")
}

fn recurse(
    execution_state: &mut ExecutionState,
    state: &State,
    identifier_opt: &mut core::slice::Iter<IdentifierHelper>,
    curr_value: Constant,
    final_value: Constant,
) -> Constant {
    match identifier_opt.next() {
        Some(val) => match val {
            IdentifierHelper::ArrayIndex(expr) => match curr_value {
                Constant::Array(vtype, mut list) => {
                    match eval_expr(expr, execution_state, state).unwrap() {
                        Constant::Int(i) => {
                            if (i as usize) >= list.len() {
                                panic!("Index out of bounds for on array")
                            }
                            list[i as usize] = recurse(
                                execution_state,
                                state,
                                identifier_opt,
                                list[i as usize].clone(),
                                final_value,
                            );
                            return Constant::Array(vtype, list);
                        }
                        _ => panic!("Tried to index an array with a non int"),
                    }
                }
                Constant::Map(ktype, vtype, mut wmap) => {
                    let key = eval_expr(expr, execution_state, state).unwrap();
                    match wmap.clone().get(&key) {
                        Some(constant) => {
                            wmap.insert(
                                key,
                                recurse(
                                    execution_state,
                                    state,
                                    identifier_opt,
                                    constant.clone(),
                                    final_value,
                                ),
                            );
                        }
                        None => {
                            wmap.insert(key, final_value);
                        }
                    };
                    return Constant::Map(ktype, vtype, wmap);
                }
                Constant::String(s) => {
                    let index = eval_expr(expr, execution_state, state).unwrap();
                    match index {
                        Constant::Int(i) => {
                            return {
                                if (i as usize) >= s.len() {
                                    panic!("Index out of bounds for {}", s)
                                }
                                Constant::Char(s.chars().nth(i as usize).unwrap())
                            }
                        }
                        _ => panic!("Only ints can index strings"),
                    }
                }
                _ => panic!("Tried to index a non string or non array"),
            },
            IdentifierHelper::StructIndex(attribute) => match curr_value {
                Constant::Struct { name, mut pairs } => {
                    let updated_val = pairs.clone().get(&attribute.clone()).unwrap().clone();
                    pairs.insert(
                        attribute.clone(),
                        recurse(
                            execution_state,
                            state,
                            identifier_opt,
                            updated_val.clone(),
                            final_value,
                        ),
                    );
                    return Constant::Struct { name, pairs };
                }
                _ => panic!("struct index on a non struct"),
            },
        },
        None => final_value,
    }
}

fn save_value(
    execution_state: &mut ExecutionState,
    state: &State,
    identifier: Identifier,
    value: Constant,
) {
    let large = execution_state
        .var_map
        .get(&identifier.var_name)
        .unwrap()
        .clone();
    let value = recurse(
        execution_state,
        state,
        &mut identifier.tail.iter(),
        large,
        value,
    );
    execution_state.var_map.insert(identifier.var_name, value);
}

/*
* evaluate an ast, one line or one if/while stm
*/
fn eval_ast(
    ast: AstNode,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Option<Constant>, ExecutionError> {
    match ast {
        AstNode::Assignment {
            var_type,
            identifier,
            expr,
            position: _,
        } => {
            let value = eval_expr(&expr, execution_state, state)?;
            let actual_val = match (var_type.clone(), value) {
                (Some(VarType::Int), Constant::Char(c)) => Constant::Int(c as i32),
                (_, val) => val,
            };
            match var_type {
                Some(_) => execution_state.save_variable(identifier.var_name, actual_val),
                None => {
                    save_value(execution_state, state, identifier, actual_val);
                }
            }
        }
        AstNode::If(if_pairs) => {
            execution_state.increment_stack_level();
            for (conditional, mut stms) in if_pairs {
                if eval_bool_expr(&conditional, execution_state, state)? {
                    while stms.len() > 0 {
                        match eval_ast(*stms.remove(0), execution_state, state)? {
                            Some(eval) => return Ok(Some(eval)),
                            None => (),
                        }
                    }
                    break;
                }
            }
            execution_state.pop_stack();
        }
        AstNode::While(conditional, stms) => {
            while eval_bool_expr(&conditional, execution_state, state)? {
                execution_state.increment_stack_level();
                for stm in stms.iter() {
                    match eval_ast(*stm.clone(), execution_state, state)? {
                        Some(eval) => return Ok(Some(eval)),
                        None => (),
                    }
                }
                execution_state.pop_stack();
            }
        }
        AstNode::BuiltIn(builtin) => {
            match builtin {
                BuiltIn::Print(exp) => {
                    println!(
                        "\"{}\" => {}",
                        exp,
                        eval_expr(&exp, execution_state, state)?
                    );
                }
                BuiltIn::StaticPrint(_) => (),
                BuiltIn::Assert(boolexp) => {
                    if !eval_bool_expr(&boolexp, execution_state, state)? {
                        return Err(ExecutionError::AssertionError(boolexp));
                    } else {
                        println!("{} {}", "ASSERTION PASS:".blue(), boolexp);
                    }
                }
            }
            ()
        }
        AstNode::ReturnStm(expr, _) => {
            return Ok(Some(eval_expr(&expr, execution_state, state)?));
        }
        AstNode::Skip() => (),
    }
    Ok(None)
}

/*
* evalulates booleans based on their conjunction
*/
fn eval_bool_expr(
    bool_expr: &Expr,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<bool, ExecutionError> {
    match eval_expr(bool_expr, execution_state, state)? {
        Constant::Bool(a) => Ok(a),
        _ => panic!("expected boolean result in conditional"),
    }
}

/*
* evaluates expressions and constants to true false values
*/
fn eval_bool(
    execution_state: &mut ExecutionState,
    state: &State,
    lhs: &Expr,
    op: &BoolOp,
    rhs: &Expr,
) -> Result<bool, ExecutionError> {
    // let BoolExp(lhs, op, rhs) = &*bool_exp;
    use Constant::*;
    let (lres, rres) = match (
        eval_expr(lhs, execution_state, state)?,
        eval_expr(rhs, execution_state, state)?,
    ) {
        (Int(i), Int(j)) => (i as f64, j as f64),
        (Char(i), Char(j)) => (i as u32 as f64, j as u32 as f64),
        (Map(_, _, _), _) | (_, Map(_, _, _)) => {
            panic!("type violation in eval_bool, cannot compare map")
        }
        (String(s1), String(s2)) => {
            return Ok(match op {
                BoolOp::Eq => s1 == s2,
                BoolOp::Neq => s1 != s2,
                BoolOp::Leq => s1 <= s2,
                BoolOp::Geq => s1 >= s2,
                BoolOp::Lt => s1 < s2,
                BoolOp::Gt => s1 > s2,
            });
        }
        _ => panic!(
            "type violation in eval_bool\n{:?} != {:?}",
            eval_expr(lhs, execution_state, state)?,
            eval_expr(rhs, execution_state, state)?
        ),
    };
    Ok(match op {
        BoolOp::Eq => lres == rres,
        BoolOp::Neq => lres != rres,
        BoolOp::Leq => lres <= rres,
        BoolOp::Geq => lres >= rres,
        BoolOp::Lt => lres < rres,
        BoolOp::Gt => lres > rres,
    })
}

/*
* eval_expr evaluates inline expressions
*/
fn eval_expr(
    exp: &Expr,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Constant, ExecutionError> {
    match exp.clone() {
        Expr::Identifier(identifier) => eval_identifier(identifier, execution_state, state),
        Expr::Constant(constant, _) => match constant {
            Constant::Array(var_type, elements) => Ok(Constant::Array(var_type, elements)),
            _ => Ok(constant),
        },
        Expr::FnCall {
            name,
            params,
            position,
        } => eval_fn_call(
            FnCall {
                name,
                params,
                position,
            },
            execution_state,
            state,
        ),
        Expr::ListComprehension {
            piped_var,
            value_expr,
            in_expr,
        } => {
            let len = match eval_expr(&in_expr, execution_state, state)? {
                Constant::Int(i) => i as usize,
                _ => panic!("type mismatch found during execution"),
            };
            // elements of the array
            let mut elements: Vec<Constant> = Vec::new();
            let (variable, pipe) = match piped_var {
                Some(piped) => (piped, true),
                None => (String::from(""), false),
            };
            execution_state.increment_stack_level();
            for i in 0..len {
                // not currently type checking need to add that later on
                if pipe {
                    execution_state
                        .var_map
                        .insert(variable.clone(), Constant::Int(i as i32));
                }
                elements.push(eval_expr(&value_expr.clone(), execution_state, state)?);
            }
            execution_state.pop_stack();
            Ok(Constant::Array(VarType::Generic, elements))
        }
        Expr::BinaryExpr(lhs, op, rhs) => {
            let left = eval_expr(&*lhs, execution_state, state)?;
            let right = eval_expr(&*rhs, execution_state, state)?;
            use Constant::*;
            use OpType::*;
            match op {
                Eq => Ok(Bool(left == right)),
                Neq => Ok(Bool(left != right)),
                Lt => Ok(Bool(left < right)),
                Gt => Ok(Bool(left > right)),
                Leq => Ok(Bool(left <= right)),
                Geq => Ok(Bool(left >= right)),
                And => match (left, right) {
                    (Bool(left), Bool(right)) => {
                        Ok(Bool(left && right))
                    }
                    _ => panic!("left or right didn't evaluate to bool in `and`"),
                },
                Or => match (left, right) {
                    (Bool(left), Bool(right)) => {
                        Ok(Bool(left || right))
                    }
                    _ => panic!("left or right didn't evaluate to bool in `or`"),
                },
                operator => {
                    // extract integer values, or add chars and strings
                    let (l, r, var_type) = match (left, right) {
                        (Int(l), Int(r)) => (l as f64, r as f64, VarType::Int),
                        (String(l), String(r)) => match operator {
                            Add => return Ok(String(format!("{}{}", l, r))),
                            _ => panic!("expr operation on strings only allow +"),
                        },
                        (Char(l), Char(r)) => match operator {
                            Add => return Ok(String(format!("{}{}", l, r))),
                            _ => panic!("expr operation on strings only allow +"),
                        },
                        (Char(l), String(r)) => match operator {
                            Add => return Ok(String(format!("{}{}", l, r))),
                            _ => panic!("expr operation on strings only allow +"),
                        },
                        (String(l), Char(r)) => match operator {
                            Add => return Ok(String(format!("{}{}", l, r))),
                            _ => panic!("expr operation on strings only allow +"),
                        },
                        _ => panic!("expr operation on mismatching types"),
                    };
                    let res = match operator {
                        Add => l + r,
                        Sub => l - r,
                        Mult => l * r,
                        Div => {
                            if r == 0.0 {
                                return Err(ExecutionError::DivideByZero);
                            }
                            l / r
                        }
                        Pow => l.powf(r),
                        Modulus => l % r,
                        _ => panic!("unmatched arm for operator"),
                    };
                    match var_type {
                        VarType::Int => Ok(Int(res as i32)),
                        _ => {
                            panic!("type violation caught in execution while trying to evaluate expression")
                        }
                    }
                }
            }
        }
    }
}

fn eval_identifier(
    identifier: Identifier,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Constant, ExecutionError> {
    let mut curr_value = execution_state
        .var_map
        .get(&identifier.var_name)
        .unwrap()
        .clone();
    for ih in identifier.tail {
        match ih {
            IdentifierHelper::ArrayIndex(exp) => match curr_value {
                Constant::Array(_, list) => match eval_expr(&exp, execution_state, state)? {
                    Constant::Int(i) => {
                        curr_value = list[i as usize].clone();
                    }
                    _ => panic!("Tried an array with an non int value"),
                },
                Constant::Map(_, _, wmap) => {
                    let key = &eval_expr(&exp, execution_state, state)?;
                    curr_value = wmap
                        .get(key)
                        .expect(&format!("Could not find value \"{}\" in map", key)[..])
                        .clone();
                }
                Constant::String(s) => {
                    let index = eval_expr(&exp, execution_state, state).unwrap();
                    match index {
                        Constant::Int(i) => {
                            return Ok(Constant::Char(s.chars().nth(i as usize).unwrap()))
                        }
                        _ => panic!("Only ints can index strings"),
                    }
                }
                _ => panic!("[] index to neither map, array or string"),
            },
            IdentifierHelper::StructIndex(attribute) => match curr_value {
                Constant::Struct { name: _, pairs } => {
                    curr_value = pairs.get(&attribute).unwrap().clone()
                }
                _ => panic!("struct index to a non struct"),
            },
        }
    }
    Ok(curr_value)
}

fn eval_reserved_functions(
    func_call: &FnCall,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Constant, ExecutionError> {
    match &func_call.name[..] {
        LEN => match eval_expr(&func_call.params[0].clone(), execution_state, state)? {
            Constant::Array(_, elements) => Ok(Constant::Int(elements.len() as i32)),
            Constant::String(s) => Ok(Constant::Int(s.len() as i32)),
            _ => panic!("panicked tried to find the length of a non array string"),
        },
        PARSE_INT => {
            match eval_expr(&func_call.params[0].clone(), execution_state, state)? {
                Constant::String(s) => {
                    // println!("parse int from `{}`", s);
                    Ok(Constant::Int(s.parse::<i32>().expect(
                        &("Expected to parse int, got ".to_owned() + &s[..]),
                    )))
                }
                Constant::Char(c) => Ok(Constant::Int(c as i32 - 48)),
                _ => panic!("panicked tried to find the length of a non array string"),
            }
        }
        USER_INPUT => {
            let mut line = String::new();
            std::io::stdin().read_line(&mut line).unwrap();
            Ok(Constant::String(line))
        }
        APPEND => {
            let value = eval_expr(&func_call.params[0].clone(), execution_state, state)?;
            match value {
                Constant::Array(vtype, mut values) => {
                    values.push(eval_expr(
                        &func_call.params[1].clone(),
                        execution_state,
                        state,
                    )?);
                    Ok(Constant::Array(vtype, values))
                }
                _ => panic!("Tried appending non array"),
            }
        }
        PREPEND => {
            let value = eval_expr(&func_call.params[0].clone(), execution_state, state)?;
            match value {
                Constant::Array(vtype, mut values) => {
                    values.insert(
                        0,
                        eval_expr(&func_call.params[1].clone(), execution_state, state)?,
                    );
                    Ok(Constant::Array(vtype, values))
                }
                _ => panic!("Tried appending non array"),
            }
        }
        REMOVE => {
            let value = eval_expr(&func_call.params[0].clone(), execution_state, state)?;
            let index = eval_expr(&func_call.params[1].clone(), execution_state, state)?;
            match (value, index) {
                (Constant::Array(vtype, mut values), Constant::Int(i)) => {
                    values.remove(i as usize);
                    Ok(Constant::Array(vtype, values))
                }
                (Constant::String(mut values), Constant::Int(i)) => {
                    values.remove(i as usize);
                    Ok(Constant::String(values))
                }
                _ => panic!("Tried poping non array"),
            }
        }
        TO_STR => match eval_expr(&func_call.params[0].clone(), execution_state, state)? {
            Constant::Int(i) => Ok(Constant::String(i.to_string())),
            Constant::Char(c) => Ok(Constant::String(c.to_string())),
            _ => panic!("panicked tried to find the length of a non array string"),
        },
        _ => {
            panic!("reserved function not found in eval")
        }
    }
}

fn eval_fn_call(
    func_call: FnCall,
    execution_state: &mut ExecutionState,
    state: &State,
) -> Result<Constant, ExecutionError> {
    // need a new var map for the function, just the parameters
    if RESERVED_FUNCTIONS.contains(&func_call.name[..]) {
        return eval_reserved_functions(&func_call, execution_state, state);
    }

    let mut var_map: HashMap<String, Constant> = HashMap::new();
    let fn_name = func_call.name;
    let function = match state.func_map.get(&fn_name) {
        Some(function) => function,
        None => {
            // STRUCT CONSTRUCTOR
            let type_map = state
                .struct_map
                .get(&fn_name)
                .expect("Value does not exist for function/constructor");
            // iterate through the parameters provided and the function def,
            let mut pairs = BTreeMap::new();
            for (expr, (param_name, _)) in func_call.params.iter().zip(type_map.iter()) {
                let param_const = eval_expr(&expr.clone(), execution_state, state)?;
                pairs.insert(param_name.clone(), param_const);
            }
            return Ok(Constant::Struct {
                name: fn_name,
                pairs,
            });
        }
    };
    let mut var_stack: Vec<(String, u32)> = Vec::new();
    let params = function.params.clone();
    // iterate through the parameters provided and the function def,
    for (expr, (_, param_name)) in func_call.params.iter().zip(params.iter()) {
        let param_const = eval_expr(&expr.clone(), execution_state, state)?;
        var_stack.push((param_name.clone(), 0));
        var_map.insert(param_name.to_string(), param_const);
    }
    let mut func_state = ExecutionState {
        var_map,
        var_stack,
        stack_lvl: 0,
    };
    eval_func(fn_name, &mut func_state, state)
}
